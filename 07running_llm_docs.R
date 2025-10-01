#===============================================================================
# SCRIPT: Running LLM Analysis on ECB Press Conferences
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
# Last Modified: October 2025
# 
# Purpose:
#   Process ECB press conference transcripts using Google's Gemini LLM to 
#   simulate 30 heterogeneous trader reactions. Generates forecasts for Euro 
#   OIS rates at 3-month, 2-year, and 10-year maturities.
#
# Inputs:
#   - ../intermediate_data/texts/*.txt (ECB press conference transcripts)
#   - create_prompts.R (defines prompt templates)
#   - GEMINI_API_KEY environment variable
#
# Outputs:
#   - ../intermediate_data/gemini_result/[prompt_name]/[date].rds
#   - failed_requests.log (error log if requests fail)
#
# Dependencies:
#   - Gemini API key (set as environment variable)
#   - Active internet connection
#   - Sufficient API quota
#
# Runtime: ~2-4 hours for full dataset with 5 parallel workers
# Cost: ~$X per full run (estimate based on your usage)
#
# Notes:
#   - Uses parallel processing (5 workers by default)
#   - Includes exponential backoff for failed requests
#   - Each conference processed independently to avoid contamination
#===============================================================================

# Setup ------------------------------------------------------------------------

# Ensure pacman is loaded
if (!require("pacman")) install.packages("pacman")

# Load and install all required packages
pacman::p_load(
  gemini.R,     # Interface to Google Gemini API
  cli,          # Command line interface utilities
  httr2,        # HTTP client for API requests
  readtext,     # Read text files
  crayon,       # Colored console output
  stringr,      # String manipulation
  purrr,        # Functional programming tools
  readr,        # Fast file reading
  writexl,      # Write Excel files
  scales,       # Scaling functions for visualization
  showtext,     # Font rendering
  readxl,       # Read Excel files
  tidyverse,    # Core tidyverse packages
  future,       # Parallel processing framework
  furrr         # Apply purrr functions in parallel
)

# Set API key for Gemini -------------------------------------------------------
# NOTE: Set GEMINI_API_KEY in your .Renviron file or system environment
setAPI(Sys.getenv("GEMINI_API_KEY"))

# Verify API key is set
if (Sys.getenv("GEMINI_API_KEY") == "") {
  stop("GEMINI_API_KEY not found. Please set it in your environment variables.")
}


#===============================================================================
# CUSTOM FUNCTIONS
#===============================================================================

#' Send Request to Gemini API with Extended Timeout
#' 
#' @description
#' Custom wrapper around Gemini API with increased timeout (120s) and robust
#' error handling. Supports various generation parameters.
#'
#' @param prompt Character string containing the full prompt
#' @param model Character. Model version (default: "2.5-flash")
#' @param temperature Numeric. Controls randomness (0-2, default: 1)
#' @param maxOutputTokens Numeric. Maximum response length (default: 1000000)
#' @param topK Numeric. Top-K sampling parameter (default: 40)
#' @param topP Numeric. Nucleus sampling parameter (default: 0.95)
#' @param seed Numeric. Random seed for reproducibility (default: 1234)
#'
#' @return Character vector containing the model's response
#'
#' @details
#' Temperature = 1 provides balanced randomness suitable for simulating
#' diverse trader behavior. Higher values increase creativity/variability.
#'
#' @examples
#' \dontrun{
#' response <- new_gemini("What is monetary policy?", temperature = 0.5)
#' }
new_gemini <- function(prompt, 
                       model = "2.5-flash", 
                       temperature = 1, 
                       maxOutputTokens = 1000000,
                       topK = 40, 
                       topP = 0.95, 
                       seed = 1234) {
  
  # Construct API endpoint
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")
  
  # Create generation configuration
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )
  
  # Add responseModalities for image generation model (future-proofing)
  if (model == "2.0-flash-exp-image-generation") {
    generation_config$responseModalities <- list("Text", "Image")
  }
  
  # Construct request body
  request_body <- list(
    contents = list(
      parts = list(
        list(text = prompt)
      )
    ),
    generationConfig = generation_config
  )
  
  # Build and execute HTTP request
  req <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(request_body) |>
    req_timeout(120)  # Extended timeout for long documents
  
  resp <- req_perform(req)
  
  # Check response status
  if (resp$status_code != 200) {
    stop(paste0("API Error: Status code ", resp$status_code, 
                " - ", resp_body_string(resp)))
  }
  
  # Extract and return generated text
  candidates <- resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) {
    candidate$content$parts
  }))
  
  return(outputs)
}


#' Process Single Press Conference
#' 
#' @description
#' Wrapper function to process one ECB press conference through the LLM.
#' Designed for use with furrr::future_map for parallel processing.
#'
#' @param conf_date Character. Date of conference (YYYY-MM-DD format)
#' @param conf_text Character. Full transcript text
#' @param prompt_template Character. Prompt template with [date] placeholder
#' @param log_file_path Character. Path to error log file
#' @param seed Numeric. Random seed for reproducibility (default: 120)
#' @param max_attempts Numeric. Maximum retry attempts (default: 5)
#'
#' @return Logical. TRUE if successful, FALSE otherwise
#'
#' @details
#' - Implements exponential backoff: waits 5s, 10s, 15s, ... between retries
#' - Saves results as RDS files for memory efficiency
#' - Logs all errors to specified log file
#'
#' @examples
#' \dontrun{
#' success <- process_single_conference(
#'   conf_date = "2024-01-15",
#'   conf_text = "Full transcript...",
#'   prompt_template = prompt_naive,
#'   log_file_path = "errors.log"
#' )
#' }
process_single_conference <- function(conf_date, 
                                     conf_text, 
                                     prompt_template, 
                                     log_file_path, 
                                     seed = 120, 
                                     max_attempts = 5) {
  
  cat(crayon::yellow(paste0("🔄 Starting processing for ", conf_date, "\n")))
  
  # Construct conference-specific prompt
  full_prompt_for_conf <- gsub("\\[date\\]", conf_date, prompt_template)
  full_prompt_for_conf <- paste0(
    full_prompt_for_conf,
    "Press Conference on ", conf_date, "\n",
    "Text:", conf_text, "\n\n"
  )
  
  # Retry loop with exponential backoff
  for (attempt in 1:max_attempts) {
    Sys.sleep(5 * attempt)  # Exponential backoff: 5s, 10s, 15s, ...
    
    result <- tryCatch({
      # Call LLM API
      res <- new_gemini(full_prompt_for_conf, seed = seed, temperature = 1)
      
      # Save result
      output_path <- paste0(
        "../intermediate_data/gemini_result/", 
        name_prompt_request, "/", 
        conf_date, ".rds"
      )
      saveRDS(res, file = output_path)
      
      cat(crayon::green(paste0("✅ Press conference on ", conf_date, 
                              " processed and saved.\n")))
      return(TRUE)
      
    }, error = function(e) {
      # Log error
      cat(crayon::red(paste0("❌ Error processing press conference on ", 
                            conf_date, ": ", e$message, "\n")))
      write(paste0(conf_date, ": ", e$message), 
            file = log_file_path, 
            append = TRUE)
      return(FALSE)
    })
    
    if (result) break  # Exit loop if successful
  }
  
  # Report final failure if all attempts exhausted
  if (!result) {
    cat(crayon::red(paste0("❌ All ", max_attempts, 
                          " attempts failed for ", conf_date, "\n")))
  }
  
  return(result)
}


#===============================================================================
# DATA PREPARATION
#===============================================================================

# Load prompt template ---------------------------------------------------------
cat(crayon::blue("Loading prompt template...\n"))
source("create_prompts.R")  # Defines prompt_naive and other prompts

# Set which prompt to use
prompt_request <- prompt_naive
name_prompt_request <- deparse(substitute(prompt_naive))

# Create output directory if needed
output_dir <- paste0("../intermediate_data/gemini_result/", name_prompt_request)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat(crayon::green(paste0("✅ Created output directory: ", output_dir, "\n")))
}

# Extract conference dates -----------------------------------------------------
# NOTE: Dates extracted from filenames to ensure chronological order
dates_ecb_presconf <- list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%                      # Keep only files with dates
  str_remove("\\.txt") %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
  sort()                                     # Chronological order

# Extract conference names (for ordering) --------------------------------------
names_ecb_presconf <- list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%
  str_remove("\\.txt") %>%
  sort()

# Load conference texts --------------------------------------------------------
cat(crayon::blue(paste0("Loading ", length(dates_ecb_presconf), 
                       " press conference transcripts...\n")))

ecb_pressconf <- list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%
  paste0("../intermediate_data/texts/", .) %>%
  map(~ readtext(.x)) %>%
  map(~ .$text) %>%
  set_names(names_ecb_presconf) %>%
  .[names_ecb_presconf]  # Ensure order matches dates

cat(crayon::green(paste0("✅ Loaded ", length(ecb_pressconf), 
                        " transcripts successfully\n")))


#===============================================================================
# PARALLEL PROCESSING EXECUTION
#===============================================================================

# Initialize logging and timing ------------------------------------------------
log_file <- "failed_requests.log"
start_time <- Sys.time()

# Clear previous log
if (file.exists(log_file)) {
  file.remove(log_file)
  cat(crayon::yellow("Cleared previous error log\n"))
}

# Configure parallel processing ------------------------------------------------
# NOTE: Adjust 'workers' based on:
#   1. Your system's CPU cores (check with: parallel::detectCores())
#   2. Gemini API rate limits (typically 60 requests/minute)
#   3. Available memory (each worker needs ~500MB)
# 
# Recommended: Start with 3-5 workers and monitor performance
plan(multisession, workers = 5)

cat(crayon::blue(paste0("🚀 Starting parallel processing with 5 workers...\n")))
cat(crayon::blue(paste0("📊 Processing ", length(dates_ecb_presconf), 
                       " conferences\n")))

# Execute parallel processing --------------------------------------------------
results_parallel <- future_map2(
  dates_ecb_presconf,  # Conference dates
  ecb_pressconf,       # Conference texts
  ~ process_single_conference(
    conf_date = .x,
    conf_text = .y,
    prompt_template = prompt_request,
    log_file_path = log_file,
    seed = 120
  ),
  .options = furrr_options(seed = TRUE)  # Ensure reproducibility
)

# Close parallel workers
plan(sequential)


#===============================================================================
# SUMMARY AND DIAGNOSTICS
#===============================================================================

# Calculate execution time
end_time <- Sys.time()
total_time <- end_time - start_time

# Summarize results
n_success <- sum(unlist(results_parallel))
n_failed <- length(results_parallel) - n_success

cat(crayon::blue("\n" strrep("=", 70), "\n"))
cat(crayon::blue("EXECUTION SUMMARY\n"))
cat(crayon::blue(strrep("=", 70), "\n\n"))
cat("Total conferences:  ", length(results_parallel), "\n")
cat(crayon::green("Successful:         "), n_success, "\n")
cat(crayon::red("Failed:             "), n_failed, "\n")
cat("Total time:         ", round(total_time, 2), " ", 
    attr(total_time, "units"), "\n")
cat("Average per conf:   ", 
    round(as.numeric(total_time) / length(results_parallel), 2), " ",
    attr(total_time, "units"), "\n")
cat(crayon::blue("\n" strrep("=", 70), "\n\n"))

# Check for errors in log
if (file.exists(log_file) && file.info(log_file)$size > 0) {
  cat(crayon::yellow("⚠️  Some requests failed. Check 'failed_requests.log' for details.\n"))
} else {
  cat(crayon::green("✅ All requests completed successfully!\n"))
}

cat(crayon::green(paste0("✅ Results saved to: ", output_dir, "\n")))

#===============================================================================
# END OF SCRIPT
#===============================================================================