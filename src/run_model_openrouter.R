#===============================================================================
# OPENROUTER MODEL RUNNER - QWEN3 235B NAIVE PROMPT
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Run the naive prompt simulation using OpenRouter API with Qwen3 235B model.
#
# Usage:
#   From R console:
#     source("src/run_model_openrouter.R")
#
#   From command line:
#     Rscript src/run_model_openrouter.R
#
# Configuration:
#   - Model: qwen/qwen3-235b-a22b-instruct
#   - API Key: OPENROUTER_API_KEY environment variable
#   - Output: ../intermediate_data/openrouter_result/qwen3_naive/
#===============================================================================


# Setup ------------------------------------------------------------------------
cat("\n", strrep("=", 80), "\n")
cat("OPENROUTER MODEL RUNNER - Qwen3 235B Naive Prompt\n")
cat(strrep("=", 80), "\n\n")

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  httr2,        # HTTP requests
  readtext,     # Read text files
  crayon,       # Colored output
  stringr,      # String manipulation
  purrr,        # Functional programming
  tidyverse,    # Data manipulation
  future,       # Parallel processing
  furrr         # Parallel purrr
)

# Configuration ----------------------------------------------------------------
config <- list(
  # Model settings
  model = "qwen/qwen3-235b-a22b-2507",
  temperature = 1,
  max_tokens = 100000,
  top_p = 0.95,
  seed = 120,

  # Processing settings
  parallel_workers = 4,
  max_retry_attempts = 5,

  # Paths
  texts_dir = "../intermediate_data/texts",
  output_dir = "../intermediate_data/openrouter_result/qwen3_naive",
  log_file = "failed_requests_openrouter.log"
)

# Verify API key ---------------------------------------------------------------
cat(crayon::blue("Checking API key...\n"))
#api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (api_key == "") {
  stop("OPENROUTER_API_KEY not found. Please set it in your .Renviron file.")
}
cat(crayon::green("API key loaded\n\n"))

# Load prompts -----------------------------------------------------------------
cat(crayon::blue("Loading prompts...\n"))
source("config/prompts.R")
prompt_template <- prompt_naive
cat(crayon::green("Loaded prompt: prompt_naive\n\n"))

# Load OpenRouter API functions ------------------------------------------------
source("src/llm_api/openrouter_api.R")

# Load ECB press conference texts ----------------------------------------------
cat(crayon::blue("Loading ECB press conference transcripts...\n"))

if (!dir.exists(config$texts_dir)) {
  stop(paste0("Texts directory not found: ", config$texts_dir))
}

dates_ecb_presconf <- list.files(config$texts_dir) %>%
  str_subset("\\d") %>%
  str_remove("\\.txt") %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
  sort()

names_ecb_presconf <- list.files(config$texts_dir) %>%
  str_subset("\\d") %>%
  str_remove("\\.txt") %>%
  sort()

ecb_pressconf <- list.files(config$texts_dir) %>%
  str_subset("\\d") %>%
  paste0(config$texts_dir, "/", .) %>%
  map(~ readtext(.x)) %>%
  map(~ .$text) %>%
  set_names(names_ecb_presconf) %>%
  .[names_ecb_presconf]

cat(crayon::green(paste0("Loaded ", length(ecb_pressconf),
                         " press conference transcripts\n\n")))

# Create output directory ------------------------------------------------------
if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
  cat(crayon::green(paste0("Created output directory: ", config$output_dir, "\n\n")))
}

# Initialize logging -----------------------------------------------------------
start_time <- Sys.time()

if (file.exists(config$log_file)) {
  file.remove(config$log_file)
  cat(crayon::yellow("Cleared previous error log\n\n"))
}

# Configure parallel processing ------------------------------------------------
n_workers <- config$parallel_workers
plan(multisession, workers = n_workers)

cat(crayon::blue(paste0("Starting parallel processing with ", n_workers, " workers...\n")))
cat(crayon::blue(paste0("Processing ", length(dates_ecb_presconf), " conferences\n\n")))

# Execute parallel processing --------------------------------------------------
results_parallel <- future_map2(
  dates_ecb_presconf,
  ecb_pressconf,
  ~ process_single_conference_openrouter(
    conf_date = .x,
    conf_text = .y,
    prompt_template = prompt_template,
    output_dir = config$output_dir,
    log_file_path = config$log_file,
    model = config$model,
    seed = config$seed,
    temperature = config$temperature,
    max_attempts = config$max_retry_attempts
  ),
  .options = furrr_options(seed = TRUE)
)

# Close parallel workers
plan(sequential)

# Summary and Diagnostics ------------------------------------------------------
end_time <- Sys.time()
total_time <- end_time - start_time

n_success <- sum(unlist(results_parallel))
n_failed <- length(results_parallel) - n_success

cat("\n", strrep("=", 80), "\n")
cat("EXECUTION SUMMARY\n")
cat(strrep("=", 80), "\n\n")
cat("Model:              ", config$model, "\n")
cat("Total conferences:  ", length(results_parallel), "\n")
cat(crayon::green("Successful:         "), n_success, "\n")
cat(crayon::red("Failed:             "), n_failed, "\n")
cat("Total time:         ", round(total_time, 2), " ",
    attr(total_time, "units"), "\n")
cat("Average per conf:   ",
    round(as.numeric(total_time) / length(results_parallel), 2), " ",
    attr(total_time, "units"), "\n")
cat("\n", strrep("=", 80), "\n\n")

if (file.exists(config$log_file) && file.info(config$log_file)$size > 0) {
  cat(crayon::yellow("Some requests failed. Check '", config$log_file, "' for details.\n"))
} else {
  cat(crayon::green("All requests completed successfully!\n"))
}

cat(crayon::green(paste0("Results saved to: ", config$output_dir, "\n\n")))

#===============================================================================
# END OF SCRIPT
#===============================================================================
