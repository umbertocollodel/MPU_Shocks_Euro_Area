#===============================================================================
# UNIFIED MODEL RUNNER
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
# Last Modified: November 2025
#
# Purpose:
#   Unified entry point for running all model variants:
#     1. naive - Basic prompt without historical context
#     2. historical_surprise - Includes past 3 conferences' volatility
#     3. llm_as_judge - Meta-learning with prompt optimization
#
# Usage:
#   From R console:
#     source("src/run_model.R")
#     run_model(model_name = "naive")
#
#   From command line:
#     Rscript src/run_model.R --model naive
#     Rscript src/run_model.R --model historical_surprise
#     Rscript src/run_model.R --model llm_as_judge
#
# Configuration:
#   Edit config/model_config.yaml to change model parameters
#===============================================================================


#' Run Model Based on Configuration
#'
#' @param model_name Character. One of: "naive", "historical_surprise", "llm_as_judge"
#' @param config_path Character. Path to YAML config file (default: "config/model_config.yaml")
#'
#' @return NULL. Writes results to output_dir specified in config
#'
#' @export
run_model <- function(model_name = NULL, config_path = "config/model_config.yaml") {

  # Setup ----------------------------------------------------------------------
  cat(crayon::blue("\n", strrep("=", 80), "\n"))
  cat(crayon::blue("UNIFIED MODEL RUNNER - ECB Communication Analysis\n"))
  cat(crayon::blue(strrep("=", 80), "\n\n"))

  # Load required packages
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(
    yaml,         # Read YAML config
    gemini.R,     # Gemini API setup
    httr2,        # HTTP requests
    readtext,     # Read text files
    crayon,       # Colored output
    stringr,      # String manipulation
    purrr,        # Functional programming
    tidyverse,    # Data manipulation
    future,       # Parallel processing
    furrr         # Parallel purrr
  )

  # Load configuration ---------------------------------------------------------
  cat(crayon::blue("📋 Loading configuration...\n"))

  if (!file.exists(config_path)) {
    stop(paste0("Configuration file not found: ", config_path))
  }

  config <- yaml::read_yaml(config_path)

  # Determine which model to run
  if (is.null(model_name)) {
    model_name <- config$active_model
    cat(crayon::yellow(paste0("   Using active_model from config: ", model_name, "\n")))
  } else {
    cat(crayon::yellow(paste0("   Using command-line model: ", model_name, "\n")))
  }

  # Validate model name
  if (!model_name %in% names(config$models)) {
    stop(paste0("Unknown model: ", model_name,
                "\n   Available models: ", paste(names(config$models), collapse = ", ")))
  }

  model_config <- config$models[[model_name]]
  cat(crayon::green(paste0("✅ Model: ", model_name, " (", model_config$description, ")\n\n")))

  # Set API key ----------------------------------------------------------------
  api_key <- Sys.getenv(config$shared$api_key_env_var)
  if (api_key == "") {
    stop(paste0(config$shared$api_key_env_var,
                " not found. Please set it in your .Renviron file."))
  }
  gemini.R::setAPI(api_key)
  cat(crayon::green("✅ API key loaded\n\n"))

  # Load prompts ---------------------------------------------------------------
  cat(crayon::blue("📝 Loading prompts...\n"))
  source("config/prompts.R")
  prompt_template <- get(model_config$prompt_name)
  cat(crayon::green(paste0("✅ Loaded prompt: ", model_config$prompt_name, "\n\n")))

  # Load Gemini API functions --------------------------------------------------
  source("src/llm_api/gemini_api.R")

  # Route to appropriate model -------------------------------------------------
  if (model_config$language == "R") {
    run_r_model(model_name, model_config, config, prompt_template)
  } else if (model_config$language == "python") {
    run_python_model(model_name, model_config, config)
  } else {
    stop(paste0("Unknown language: ", model_config$language))
  }
}


#' Run R-based Model (naive or historical_surprise)
#'
#' @keywords internal
run_r_model <- function(model_name, model_config, config, prompt_template) {

  # Load ECB press conference texts --------------------------------------------
  cat(crayon::blue("📂 Loading ECB press conference transcripts...\n"))

  texts_dir <- config$shared$texts_dir
  if (!dir.exists(texts_dir)) {
    stop(paste0("Texts directory not found: ", texts_dir))
  }

  dates_ecb_presconf <- list.files(texts_dir) %>%
    str_subset("\\d") %>%
    str_remove("\\.txt") %>%
    str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
    sort()

  names_ecb_presconf <- list.files(texts_dir) %>%
    str_subset("\\d") %>%
    str_remove("\\.txt") %>%
    sort()

  ecb_pressconf <- list.files(texts_dir) %>%
    str_subset("\\d") %>%
    paste0(texts_dir, "/", .) %>%
    map(~ readtext(.x)) %>%
    map(~ .$text) %>%
    set_names(names_ecb_presconf) %>%
    .[names_ecb_presconf]

  cat(crayon::green(paste0("✅ Loaded ", length(ecb_pressconf),
                          " press conference transcripts\n\n")))

  # Load historical data if needed ---------------------------------------------
  range_diff_df <- NULL
  if (model_config$use_history) {
    cat(crayon::blue("📊 Loading historical volatility data...\n"))

    if (!file.exists(model_config$history_data_path)) {
      stop(paste0("Historical data file not found: ", model_config$history_data_path))
    }

    range_diff_df <- readRDS(model_config$history_data_path) %>%
      mutate(date = as.Date(date)) %>%
      arrange(tenor, date) %>%
      filter(tenor %in% c("3M", "2Y", "10Y"))

    cat(crayon::green("✅ Historical data loaded\n\n"))
  }

  # Create output directory ----------------------------------------------------
  output_dir <- model_config$output_dir
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(crayon::green(paste0("✅ Created output directory: ", output_dir, "\n\n")))
  }

  # Initialize logging ---------------------------------------------------------
  log_file <- config$shared$log_file
  start_time <- Sys.time()

  if (file.exists(log_file)) {
    file.remove(log_file)
    cat(crayon::yellow("🗑️  Cleared previous error log\n\n"))
  }

  # Configure parallel processing ----------------------------------------------
  n_workers <- model_config$parallel_workers
  plan(multisession, workers = n_workers)

  cat(crayon::blue(paste0("🚀 Starting parallel processing with ", n_workers, " workers...\n")))
  cat(crayon::blue(paste0("📊 Processing ", length(dates_ecb_presconf), " conferences\n\n")))

  # Execute parallel processing ------------------------------------------------
  if (model_config$use_history) {
    # Use historical context version
    results_parallel <- future_map2(
      dates_ecb_presconf,
      ecb_pressconf,
      ~ process_conference_with_history(
        conf_date = .x,
        conf_text = .y,
        prompt_template = prompt_template,
        range_diff_df = range_diff_df,
        history_window = model_config$history_window,
        output_dir = output_dir,
        log_file_path = log_file,
        model = model_config$gemini_model,
        seed = model_config$seed,
        temperature = model_config$temperature,
        max_attempts = model_config$max_retry_attempts
      ),
      .options = furrr_options(seed = TRUE)
    )
  } else {
    # Use basic version
    results_parallel <- future_map2(
      dates_ecb_presconf,
      ecb_pressconf,
      ~ process_single_conference(
        conf_date = .x,
        conf_text = .y,
        prompt_template = prompt_template,
        output_dir = output_dir,
        log_file_path = log_file,
        model = model_config$gemini_model,
        seed = model_config$seed,
        temperature = model_config$temperature,
        max_attempts = model_config$max_retry_attempts
      ),
      .options = furrr_options(seed = TRUE)
    )
  }

  # Close parallel workers
  plan(sequential)

  # Summary and Diagnostics ----------------------------------------------------
  end_time <- Sys.time()
  total_time <- end_time - start_time

  n_success <- sum(unlist(results_parallel))
  n_failed <- length(results_parallel) - n_success

  cat(crayon::blue("\n", strrep("=", 80), "\n"))
  cat(crayon::blue("EXECUTION SUMMARY\n"))
  cat(crayon::blue(strrep("=", 80), "\n\n"))
  cat("Model:              ", model_name, "\n")
  cat("Total conferences:  ", length(results_parallel), "\n")
  cat(crayon::green("Successful:         "), n_success, "\n")
  cat(crayon::red("Failed:             "), n_failed, "\n")
  cat("Total time:         ", round(total_time, 2), " ",
      attr(total_time, "units"), "\n")
  cat("Average per conf:   ",
      round(as.numeric(total_time) / length(results_parallel), 2), " ",
      attr(total_time, "units"), "\n")
  cat(crayon::blue("\n", strrep("=", 80), "\n\n"))

  if (file.exists(log_file) && file.info(log_file)$size > 0) {
    cat(crayon::yellow("⚠️  Some requests failed. Check '", log_file, "' for details.\n"))
  } else {
    cat(crayon::green("✅ All requests completed successfully!\n"))
  }

  cat(crayon::green(paste0("✅ Results saved to: ", output_dir, "\n\n")))
}


#' Run Python-based Model (llm_as_judge)
#'
#' @keywords internal
run_python_model <- function(model_name, model_config, config) {

  cat(crayon::blue("🐍 Running Python-based model...\n\n"))

  # Check if Python is available
  python_cmd <- Sys.which("python")
  if (python_cmd == "") {
    python_cmd <- Sys.which("python3")
  }
  if (python_cmd == "") {
    stop("Python not found in PATH. Please install Python or add it to your PATH.")
  }

  cat(crayon::green(paste0("✅ Found Python: ", python_cmd, "\n\n")))

  # Path to Python script
  python_script <- "src/llm_api/llm_optimizer.py"
  if (!file.exists(python_script)) {
    stop(paste0("Python script not found: ", python_script))
  }

  # Run Python script
  cat(crayon::blue("🚀 Launching LLM-as-Judge optimizer...\n"))
  cat(crayon::yellow("⚠️  This may take several hours depending on dataset size.\n\n"))

  result <- system2(python_cmd, args = python_script, stdout = TRUE, stderr = TRUE)

  cat(crayon::blue("\n", strrep("=", 80), "\n"))
  cat(crayon::blue("PYTHON MODEL OUTPUT\n"))
  cat(crayon::blue(strrep("=", 80), "\n\n"))
  cat(paste(result, collapse = "\n"))
  cat("\n\n")

  cat(crayon::green("✅ Python model execution completed\n\n"))
}


#===============================================================================
# COMMAND-LINE INTERFACE
#===============================================================================

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  # Extract --model argument
  model_idx <- which(args == "--model")
  if (length(model_idx) > 0 && model_idx < length(args)) {
    model_name <- args[model_idx + 1]
    run_model(model_name = model_name)
  } else {
    cat("Usage: Rscript src/run_model.R --model <model_name>\n")
    cat("Available models: naive, historical_surprise, llm_as_judge\n")
  }
} else {
  # If no arguments, can be sourced and called directly
  cat(crayon::blue("ℹ️  Module loaded. Call run_model(model_name = '...') to execute.\n"))
}

#===============================================================================
# END OF SCRIPT
#===============================================================================
