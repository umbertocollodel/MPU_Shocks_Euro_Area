#===============================================================================
# QWEN3 MODEL RUNNER - TEMPERATURE 0 (REPRODUCIBLE)
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Run the naive prompt simulation using OpenRouter API with Qwen3 235B at
#   temperature = 0 for full reproducibility (no stochastic sampling).
#   Counterpart to src/run_model_openrouter.R which uses temperature = 1.
#
# Usage:
#   source("21run_qwen3_t0.R")
#   Rscript 21run_qwen3_t0.R
#
# Output:
#   ../intermediate_data/openrouter_result/qwen3_naive_t0/YYYY-MM-DD.rds
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("QWEN3 MODEL RUNNER - Temperature 0 (Reproducible)\n")
cat(strrep("=", 80), "\n\n")

# Load required packages -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  httr2,
  readtext,
  crayon,
  stringr,
  purrr,
  tidyverse,
  future,
  furrr
)

# Configuration ----------------------------------------------------------------
config <- list(
  model       = "qwen/qwen3-235b-a22b-2507",
  temperature = 0,      # Deterministic — key difference from run_model_openrouter.R
  max_tokens  = 100000,
  top_p       = 1,      # top_p should be 1 when temperature = 0
  seed        = 120,

  parallel_workers   = 4,
  max_retry_attempts = 5,

  texts_dir  = "../intermediate_data/texts",
  output_dir = "../intermediate_data/openrouter_result/qwen3_naive_t0",
  log_file   = "failed_requests_openrouter_t0.log"
)

# Verify API key ---------------------------------------------------------------
cat(crayon::blue("Checking API key...\n"))
api_key <- Sys.getenv("OPENROUTER_API_KEY")
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

# Skip conferences already processed -------------------------------------------
if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
  cat(crayon::green(paste0("Created output directory: ", config$output_dir, "\n\n")))
}

already_done <- list.files(config$output_dir, pattern = "\\.rds$") %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}")

remaining <- !dates_ecb_presconf %in% already_done
dates_to_run   <- dates_ecb_presconf[remaining]
texts_to_run   <- ecb_pressconf[remaining]

if (length(dates_to_run) == 0) {
  cat(crayon::green("All conferences already processed. Nothing to do.\n\n"))
  stop("Exiting — all done.", call. = FALSE)
}

cat(crayon::yellow(paste0(
  "Skipping ", sum(!remaining), " already-processed conferences.\n",
  "Running ", length(dates_to_run), " remaining conferences.\n\n"
)))

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
cat(crayon::blue(paste0("Processing ", length(dates_to_run), " conferences\n\n")))

# Execute ----------------------------------------------------------------------
results_parallel <- future_map2(
  dates_to_run,
  texts_to_run,
  ~ process_single_conference_openrouter(
    conf_date     = .x,
    conf_text     = .y,
    prompt_template = prompt_template,
    output_dir    = config$output_dir,
    log_file_path = config$log_file,
    model         = config$model,
    seed          = config$seed,
    temperature   = config$temperature,
    max_attempts  = config$max_retry_attempts
  ),
  .options = furrr_options(seed = TRUE)
)

plan(sequential)

# Summary ----------------------------------------------------------------------
end_time   <- Sys.time()
total_time <- end_time - start_time

n_success <- sum(unlist(results_parallel))
n_failed  <- length(results_parallel) - n_success

cat("\n", strrep("=", 80), "\n")
cat("EXECUTION SUMMARY\n")
cat(strrep("=", 80), "\n\n")
cat("Model:              ", config$model, "\n")
cat("Temperature:        ", config$temperature, "\n")
cat("Total conferences:  ", length(results_parallel), "\n")
cat(crayon::green("Successful:         "), n_success, "\n")
cat(crayon::red("Failed:             "), n_failed, "\n")
cat("Total time:         ", round(total_time, 2), " ", attr(total_time, "units"), "\n")
cat("Average per conf:   ",
    round(as.numeric(total_time) / length(results_parallel), 2), " ",
    attr(total_time, "units"), "\n")
cat("\n", strrep("=", 80), "\n\n")

if (file.exists(config$log_file) && file.info(config$log_file)$size > 0) {
  cat(crayon::yellow(paste0("Some requests failed. Check '", config$log_file, "' for details.\n")))
} else {
  cat(crayon::green("All requests completed successfully!\n"))
}

cat(crayon::green(paste0("Results saved to: ", config$output_dir, "\n\n")))

#===============================================================================
# END OF SCRIPT
#===============================================================================
