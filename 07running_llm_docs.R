# Load necessary libraries and set parameters: -----

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
  future, # New: for parallel processing setup
  furrr   # New: for parallel mapping
)

# Set API key for Gemini: ----
# Ensure you have set the environment variable GEMINI_API_KEY with your API key
setAPI(Sys.getenv("GEMINI_API_KEY"))
# Create custom function to send request to Gemini API with higher timeout time: ----

new_gemini <- function(prompt, model = "2.0-flash", temperature = 1, maxOutputTokens = 1000000,
                       topK = 40, topP = 0.95, seed = 1234) {

  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  # Create generation config
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )

  # Add responseModalities only for image generation model (not relevant for this task but good to keep)
  if (model == "2.0-flash-exp-image-generation") {
    generation_config$responseModalities <- list("Text", "Image")
  }

  # Create request body as a separate list
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
    req_timeout(120)  # Increase the timeout here (in seconds)

  resp <- req_perform(req)

  # Check the status code of the response
  if (resp$status_code != 200) {
    # Using stop() will trigger the tryCatch in the calling function
    stop(paste0("Error in generate request: Status code ", resp$status_code, " - ", resp_body_string(resp)))
  }

  candidates <- resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}


# Retrieve prompts and set prompt parameter: -----

source("create_prompts.R") # Ensure this file exists and defines prompt_naive


prompt_request=prompt_naive

name_prompt_request=deparse(substitute(prompt_naive)) # Get the name of the prompt variable

# Create a list of press conferences with dates and names: ----

dates_ecb_presconf=list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%
  str_remove("\\.txt") %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
  sort() # Ensure chronological order for OIS lookups

names_ecb_presconf=list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%
  str_remove("\\.txt") %>%
  sort() # Match sorted dates


ecb_pressconf=list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%
  paste0("../intermediate_data/texts/",.) %>%
  map(~ readtext(.x)) %>%
  map(~ .$text) %>%
  set_names(names_ecb_presconf) %>%
  .[names_ecb_presconf] # Ensure order matches sorted names_ecb_presconf


# Custom function to process a single conference -----
# This version is designed to be called by furrr::future_map
process_single_conference <- function(conf_date, conf_text, prompt_template, log_file_path, seed = 120, max_attempts = 5) {

cat(crayon::yellow(paste0("🔄 Starting processing for ", conf_date, "\n")))

  # Construct the full prompt for this single conference
  full_prompt_for_conf <- gsub("\\[date\\]", conf_date, prompt_template) # Replace [date] with current date
  full_prompt_for_conf <- paste0(full_prompt_for_conf,
                                 "Press Conference on ", conf_date, "\n",
                                 "Text:", conf_text, "\n\n")

  for (attempt in 1:max_attempts) {
    Sys.sleep(5 * attempt) # Exponential backoff

    result <- tryCatch({
      res <- new_gemini(full_prompt_for_conf, seed = seed, temperature = 1)
      saveRDS(res, file = paste0("../intermediate_data/gemini_result/", conf_date, ".rds"))
      cat(crayon::green(paste0("✅ Press conference on ", conf_date, " processed and saved.\n")))
      return(TRUE)
    }, error = function(e) {
      cat(crayon::red(paste0("❌ Error processing press conference on ", conf_date, ": ", e$message, "\n")))
      write(paste0(conf_date, ": ", e$message), file = log_file_path, append = TRUE)
      return(FALSE)
    })

    if (result) break # Exit loop if successful
  }

  if (!result) {
    cat(crayon::red(paste0("❌ All attempts failed for ", conf_date, "\n")))
  }
  return(result) # Return success/failure status
}


# Run LLM remotely in parallel: ----

# Initialize time and log
log_file <- "failed_requests.log"
start_time <- Sys.time()

# Clear previous log
if (file.exists(log_file)) file.remove(log_file)

# Set up a parallel plan (e.g., using all available cores minus one)
# Adjust 'workers' based on your system's capabilities and API rate limits.
# Be mindful of potential API rate limits; too many concurrent requests might lead to errors.
plan(multisession, workers = 3) # Adjust the number of workers as needed



# Prepare arguments for future_map: a list where each element corresponds to a single conference
# We'll map over the dates_ecb_presconf, and pass the corresponding text
cat(crayon::blue("Starting parallel processing of individual conferences...\n"))

results_parallel <- future_map2(
  dates_ecb_presconf, # First argument: conference dates
  ecb_pressconf,       # Second argument: conference texts (names match dates)
  ~ process_single_conference(
    conf_date = .x,
    conf_text = .y,
    prompt_template = prompt_request,
    log_file_path = log_file,
    seed = 120 # You can make this dynamic if needed
  ),
  .options = furrr_options(seed = TRUE) # Ensure reproducibility of random seeds
)

# Print metrics:
end_time <- Sys.time()
total_time <- end_time - start_time

cat(crayon::blue("Parallel processing complete.\n"))
cat("Total time taken:", total_time, "seconds\n")

# Don't forget to close the parallel workers when done
plan(sequential)