# Load necessary libraries and set parameters: -----

setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  gemini.R,
  cli,
  httr2,
  readtext,
  crayon,
  stringr,
  purrr,
  readr,
  writexl,
  scales,
  showtext,
  readxl,
  tidyverse,
  future,
  furrr
)

# Set API key for Gemini: ----
setAPI(Sys.getenv("GEMINI_API_KEY"))

# Define Gemini request function: ----

new_gemini <- function(prompt, model = "2.5-flash", temperature = 1, maxOutputTokens = 1000000,
                       topK = 40, topP = 0.95, seed = 1234) {

  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  sb <- cli_status("Gemini is answering...")

  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )

  request_body <- list(
    contents = list(parts = list(list(text = prompt))),
    generationConfig = generation_config
  )

  req <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(request_body) |>
    req_timeout(120)

  resp <- req_perform(req)

  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in generate request: Status code ", resp$status_code))
    return(NULL)
  }

  cli_status_clear(id = sb)

  candidates <- resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}


# Load prompt template: ----

source("create_prompts.R")
prompt_request <- prompt_history_surprises
name_prompt_request <- deparse(substitute(prompt_history_surprises))


# Load ECB press conference texts: ----

dates_ecb_presconf <- list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%
  str_remove("\\.txt") %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
  sort()

names_ecb_presconf <- list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%
  str_remove("\\.txt") %>%
  sort()

ecb_pressconf <- list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%
  paste0("../intermediate_data/texts/", .) %>%
  map(~ readtext(.x)) %>%
  map(~ .$text) %>%
  set_names(names_ecb_presconf) %>%
  .[names_ecb_presconf]


# Load standard deviation data: ----

range_diff_df <- readRDS("../intermediate_data/range_difference_df.rds") %>%
  mutate(date = as.Date(date)) %>%
  arrange(tenor, date) |> 
  filter(tenor %in% c("3M", "2Y", "10Y")) 


# Define processing function: ----

process_single_conference <- function(conf_date, conf_text, prompt_template, log_file_path, seed = 120, max_attempts = 5) {

  cat(crayon::yellow(paste0("🔄 Starting processing for ", conf_date, "\n")))

  # Get previous 3 std devs for each tenor
  std_info <- range_diff_df %>%
    filter(date < as.Date(conf_date)) %>%
    group_by(tenor) %>%
    arrange(desc(date)) %>%
    slice_head(n = 3) %>%
    ungroup() %>%
    arrange(tenor, desc(date)) %>%
    mutate(
      line = paste0(tenor, " — ", date, ": Pre = ", round(correct_pre_mean, 4),
                    ", Post = ", round(correct_post_mean, 4))
    ) %>%
    pull(line) %>%
    paste(collapse = "\n")

  # Construct prompt
  full_prompt <- gsub("\\[date\\]", conf_date, prompt_template)
  full_prompt <- paste0(
    full_prompt,
    "Press Conference on ", conf_date, "\n",
    "Previous 3 conferences' standard deviations:\n", std_info, "\n",
    "Text:", conf_text, "\n\n"
  )

  for (attempt in 1:max_attempts) {
    Sys.sleep(5 * attempt)

    result <- tryCatch({
      res <- new_gemini(full_prompt, seed = seed, temperature = 1)
      saveRDS(res, file = paste0("../intermediate_data/gemini_result/", name_prompt_request, "/", conf_date, ".rds"))
      cat(crayon::green(paste0("✅ Press conference on ", conf_date, " processed and saved.\n")))
      return(TRUE)
    }, error = function(e) {
      cat(crayon::red(paste0("❌ Error processing ", conf_date, ": ", e$message, "\n")))
      write(paste0(conf_date, ": ", e$message), file = log_file_path, append = TRUE)
      return(FALSE)
    })

    if (result) break
  }

  if (!result) {
    cat(crayon::red(paste0("❌ All attempts failed for ", conf_date, "\n")))
  }

  return(result)
}


# Run in parallel: ----

log_file <- "failed_requests.log"
start_time <- Sys.time()

if (file.exists(log_file)) file.remove(log_file)

dir.create(paste0("../intermediate_data/gemini_result/", name_prompt_request), recursive = TRUE, showWarnings = FALSE)

plan(multisession, workers = 6) # Adjust number of workers based on your system

cat(crayon::blue("Starting parallel processing of individual conferences...\n"))

results_parallel <- future_map2(
  dates_ecb_presconf,
  ecb_pressconf,
  ~ process_single_conference(
    conf_date = .x,
    conf_text = .y,
    prompt_template = prompt_request,
    log_file_path = log_file,
    seed = 120
  ),
  .options = furrr_options(seed = TRUE)
)

# Print metrics: ----

end_time <- Sys.time()
total_time <- end_time - start_time

cat(crayon::blue("Parallel processing complete.\n"))
cat("Total time taken:", total_time, "seconds\n")

plan(sequential)
