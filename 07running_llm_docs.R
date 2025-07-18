# Load necessary libraries and set parameters: -----

setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# Ensure pacman is loaded
if (!require("pacman")) install.packages("pacman")

# Load and install all required packages
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

# Create custom function to send request to Gemini API with higher timeout time: ----

new_gemini <- function(prompt, model = "2.0-flash", temperature = 1, maxOutputTokens = 1000000,
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

  if (model == "2.0-flash-exp-image-generation") {
    generation_config$responseModalities <- list("Text", "Image")
  }

  request_body <- list(
    contents = list(
      parts = list(
        list(text = prompt)
      )
    ),
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


# Retrieve prompts and set prompt parameter: -----

source("create_prompts.R")

prompt_request <- prompt_anchor_values
name_prompt_request <- deparse(substitute(prompt_anchor_values))


# Load ECB press conference texts: -----

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


# Load OIS rates pre-conference: -----

ois_daily_df <- read_xlsx("../raw_data/ois_daily_data.xlsx", skip = 1) %>%
  select(1, 2, 4, 6) %>%
  setNames(c("date", "3M", "2Y", "10Y")) %>%
  mutate(date = as.Date(date))


# Custom function to process a single conference with OIS: -----

process_single_conference <- function(conf_date, conf_text, prompt_template, log_file_path, seed = 120, max_attempts = 5) {

  cat(crayon::yellow(paste0("🔄 Starting processing for ", conf_date, "\n")))

  # Get OIS values for the date
  ois_row <- ois_daily_df %>% filter(date == as.Date(conf_date))
  if (nrow(ois_row) == 0) {
    cat(crayon::red(paste0("⚠️ No OIS data found for ", conf_date, "\n")))
    return(FALSE)
  }

  ois_values <- paste(paste0(names(ois_row)[-1], ": ", as.character(ois_row[1, -1])), collapse = ", ")

  # Construct the full prompt
  full_prompt <- gsub("\\[date\\]", conf_date, prompt_template)
  full_prompt <- paste0(full_prompt,
                        "Press Conference on ", conf_date, "\n",
                        "Mean OIS rate pre-conference: ", ois_values, "\n",
                        "Text:", conf_text, "\n\n")

  for (attempt in 1:max_attempts) {
    Sys.sleep(5 * attempt)

    result <- tryCatch({
      res <- new_gemini(full_prompt, seed = seed, temperature = 1)
      saveRDS(res, file = paste0("../intermediate_data/gemini_result/", name_prompt_request, "/", conf_date, ".rds"))
      cat(crayon::green(paste0("✅ Press conference on ", conf_date, " processed and saved.\n")))
      return(TRUE)
    }, error = function(e) {
      cat(crayon::red(paste0("❌ Error processing press conference on ", conf_date, ": ", e$message, "\n")))
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


# Run LLM remotely in parallel: -----

log_file <- "failed_requests.log"
start_time <- Sys.time()

if (file.exists(log_file)) file.remove(log_file)

# Ensure output directory exists
dir.create(paste0("../intermediate_data/gemini_result/", name_prompt_request), recursive = TRUE, showWarnings = FALSE)

# Set up parallel plan
plan(multisession, workers = 5)

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

# Print metrics: -----

end_time <- Sys.time()
total_time <- end_time - start_time

cat(crayon::blue("Parallel processing complete.\n"))
cat("Total time taken:", total_time, "seconds\n")

# Reset plan
plan(sequential)
