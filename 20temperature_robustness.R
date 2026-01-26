# ==============================================================================
# SCRIPT: Temperature Robustness Analysis - Single Agent Distribution
# ==============================================================================
# This script runs N simulations for a SINGLE agent with FIXED traits on a
# SINGLE transcript to isolate the role of LLM temperature in prediction variance.
#
# The goal is to obtain an ergodic distribution of predictions without the
# confounding factor of heterogeneous trader characteristics.
#
# Architecture:
#   1. run_temperature_robustness() - Makes API calls, saves raw responses
#   2. clean_temperature_results()  - Parses raw responses into structured data
#   3. analyze_temperature_results() - Statistical analysis and visualization
#
# API: OpenRouter with Gemini 2.5 Flash
# Estimated cost: ~$0.05-0.10 for N=100 runs
#
# IMPORTANT: Run this script from the 'code/' directory
# ==============================================================================

# Load necessary libraries: ----
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  httr2,
  readtext,
  crayon,
  stringr,
  purrr,
  readr,
  writexl,
  readxl,
  tidyverse,
  lubridate
)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Number of simulation runs PER CONFERENCE
# N=50-100 is a good balance for ergodic approximation
N_SIMULATIONS <- 50

# Temperature parameter (higher = more randomness)
# 1.0 is the default used in main analysis
TEMPERATURE <- 1.0

# Model configuration
MODEL <- "google/gemini-2.5-flash"

# Fixed trader characteristics (no heterogeneity)
FIXED_TRADER <- list(
  id = "T001",
  risk_aversion = "Medium",
  behavioral_bias = "Anchoring",
  interpretation_style = "Fundamentalist"
)

# Select conferences spanning different ambiguity levels
# Each conference is labeled with its expected ambiguity/surprise level
TEST_CONFERENCES <- c(
  "2015-01-22",  # QE announcement (high ambiguity)
  "2022-07-21",  # First 50bp hike (high surprise)
  "2019-06-06",  # Steady state (low ambiguity)
  "2014-06-05",  # Forward guidance (medium)
  "2023-09-14"   # Late tightening cycle (medium)
)

# Conference metadata for labeling
CONFERENCE_LABELS <- c(

  "2015-01-22" = "QE Announcement\n(High Ambiguity)",
  "2022-07-21" = "First 50bp Hike\n(High Surprise)",
  "2019-06-06" = "Steady State\n(Low Ambiguity)",
  "2014-06-05" = "Forward Guidance\n(Medium)",
  "2023-09-14" = "Late Tightening\n(Medium)"
)

# Output directories
OUTPUT_DIR <- "../intermediate_data/temperature_robustness/"
RAW_RESPONSES_DIR <- "../intermediate_data/temperature_robustness/raw_responses/"
FIGURES_DIR <- "../output/figures/temperature_robustness/"

# ==============================================================================
# VERIFY ENVIRONMENT
# ==============================================================================

# Check working directory
if (!file.exists("../intermediate_data/texts")) {
  stop("Please run this script from the 'code/' directory.\n",
       "Current directory: ", getwd())
}

# Check API key
api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (api_key == "") {
  stop("OPENROUTER_API_KEY environment variable not set.\n",
       "Please set it in your .Renviron file or run:\n",
       "Sys.setenv(OPENROUTER_API_KEY = 'your-key-here')")
}

# Create output directories
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RAW_RESPONSES_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)

cat(crayon::blue("==============================================\n"))
cat(crayon::blue("TEMPERATURE ROBUSTNESS ANALYSIS\n"))
cat(crayon::blue("==============================================\n"))
cat(paste0("Number of simulations: ", N_SIMULATIONS, "\n"))
cat(paste0("Temperature: ", TEMPERATURE, "\n"))
cat(paste0("Model: ", MODEL, "\n"))
cat(paste0("Fixed trader: ", FIXED_TRADER$interpretation_style,
           " with ", FIXED_TRADER$behavioral_bias, " bias\n"))

# ==============================================================================
# SINGLE-AGENT PROMPT TEMPLATE
# ==============================================================================

# Modified prompt for single agent with fixed characteristics
prompt_single_agent <- paste0("
Context:
You are simulating a SINGLE trader in the Euro area interest rate swap market.
This trader interprets the ECB Governing Council press conference, which communicates
monetary policy decisions, economic assessments, and includes a Q&A session with journalists.
The trader then makes a trading decision to maximize profit based on their interpretation.

Trader Characteristics (FIXED - do not change):
- Trader ID: ", FIXED_TRADER$id, "
- Risk Aversion: ", FIXED_TRADER$risk_aversion, " - determines sensitivity to uncertainty and preference for stability.
- Behavioral Bias: ", FIXED_TRADER$behavioral_bias, " - systematic cognitive pattern affecting decision-making.
- Interpretation Style: ", FIXED_TRADER$interpretation_style, " - approach to analyzing ECB communication.

Task:
You are given the text of a single ECB press conference.
For this single trader, simulate their individual trading action in the interest rate swap market
across three tenors (3 months, 2 years, 10 years).

For each tenor, the trader must:
   - Provide an expected rate direction: Up / Down / Unchanged (relative to the pre-conference rate)
   - Provide a new expected swap rate (in percent, to two decimal places)
   - Provide a confidence score (0-100%) reflecting how strongly the trader believes in the forecast

Output:
Provide a table with the following structure:

| Date       | Trader ID | Tenor | Expected Direction | New Expected Rate (%) | Confidence (%) |
|------------|-----------|-------|--------------------|-----------------------|----------------|
| YYYY-MM-DD | T001      | 3M    | Up                 | 3.15                  | 65             |
| YYYY-MM-DD | T001      | 2Y    | Down               | 2.85                  | 80             |
| YYYY-MM-DD | T001      | 10Y   | Unchanged          | 3.50                  | 70             |

Guidelines:
- Use only the information available as of [date].
- Apply the trader's fixed characteristics consistently.
- Output only a markdown table with the specified columns, no additional text.
- Do not use JSON or any other data serialization format.
")

# ==============================================================================
# OPENROUTER API FUNCTION
# ==============================================================================

#' Call OpenRouter API (Gemini 2.5 Flash)
#'
#' @param prompt Character. The full prompt to send
#' @param temperature Numeric. Temperature parameter (0-2)
#' @param max_retries Numeric. Maximum retry attempts
#'
#' @return Character. Model response or NULL on failure
new_openrouter_temp <- function(prompt,
                                 temperature = TEMPERATURE,
                                 max_retries = 3) {

  url <- "https://openrouter.ai/api/v1/chat/completions"

  request_body <- list(
    model = MODEL,
    messages = list(
      list(role = "user", content = prompt)
    ),
    temperature = temperature,
    max_tokens = 1000  # Small output for single agent
    # NOTE: No seed parameter - we want temperature-driven randomness
  )

  for (attempt in 1:max_retries) {
    result <- tryCatch({
      req <- httr2::request(url) |>
        httr2::req_headers(
          "Authorization" = paste("Bearer", api_key),
          "HTTP-Referer" = "http://localhost",
          "X-Title" = "ECB-temperature-analysis",
          "Content-Type" = "application/json"
        ) |>
        httr2::req_body_json(request_body) |>
        httr2::req_timeout(60)

      resp <- httr2::req_perform(req)

      if (resp$status_code != 200) {
        stop(paste0("HTTP Error: ", resp$status_code))
      }

      response_json <- httr2::resp_body_json(resp)
      output <- response_json$choices[[1]]$message$content

      return(output)

    }, error = function(e) {
      if (attempt == max_retries) {
        return(NULL)
      }
      cat(crayon::yellow(paste0("  Attempt ", attempt, " failed, retrying...\n")))
      Sys.sleep(2 ^ attempt)  # Exponential backoff
      return(NULL)
    })

    if (!is.null(result)) return(result)
  }

  return(NULL)
}

# ==============================================================================
# LOAD TRANSCRIPT BY DATE
# ==============================================================================

#' Load a specific transcript by date
#'
#' @param conf_date Character. Conference date in YYYY-MM-DD format
#'
#' @return List with date, text, and file path, or NULL if not found
load_transcript <- function(conf_date) {

  # Get list of all transcripts
  text_files <- list.files("../intermediate_data/texts/",
                           pattern = "\\.txt$",
                           full.names = TRUE)

  if (length(text_files) == 0) {
    stop("No transcript files found in ../intermediate_data/texts/\n",
         "Please run 04scraping_ecb_pressconf.R first.")
  }

  # Find matching file
 selected_file <- text_files[str_detect(text_files, conf_date)]

  if (length(selected_file) == 0) {
    cat(crayon::red(paste0("Transcript not found for date: ", conf_date, "\n")))
    return(NULL)
  }

  # Read transcript
  conf_text <- readtext::readtext(selected_file[1])$text

  return(list(
    date = conf_date,
    text = conf_text,
    file = selected_file[1],
    label = if (conf_date %in% names(CONFERENCE_LABELS)) CONFERENCE_LABELS[conf_date] else conf_date
  ))
}

# ==============================================================================
# STEP 1: RUN API CALLS FOR MULTIPLE CONFERENCES
# ==============================================================================

#' Run API calls for a single conference
#'
#' @param conf_date Character. Conference date
#' @param log_file Character. Path to error log
#'
#' @return List with success/failure counts
run_single_conference <- function(conf_date, log_file) {

  cat(crayon::blue(paste0("\n--- Processing conference: ", conf_date, " ---\n")))

  # Load transcript
  transcript <- load_transcript(conf_date)

  if (is.null(transcript)) {
    return(list(successful = 0, failed = N_SIMULATIONS, skipped = TRUE))
  }

  cat(paste0("  Label: ", gsub("\n", " ", transcript$label), "\n"))
  cat(paste0("  Transcript length: ", nchar(transcript$text), " characters\n"))

  # Create conference-specific output directory
  conf_output_dir <- paste0(RAW_RESPONSES_DIR, conf_date, "/")
  dir.create(conf_output_dir, recursive = TRUE, showWarnings = FALSE)

  # Construct full prompt
  full_prompt <- gsub("\\[date\\]", conf_date, prompt_single_agent)
  full_prompt <- paste0(
    full_prompt,
    "\nPress Conference on ", conf_date, "\n",
    "Text: ", transcript$text, "\n\n"
  )

  # Initialize counters
  successful <- 0
  failed <- 0

  # Run N simulations
  for (i in 1:N_SIMULATIONS) {

    # Progress indicator (every 10)
    if (i %% 10 == 0 || i == 1) {
      cat(crayon::cyan(paste0("  Progress: ", i, "/", N_SIMULATIONS, "\n")))
    }

    # Make API call
    result <- new_openrouter_temp(full_prompt)

    if (!is.null(result)) {
      # Save raw response
      output_file <- paste0(conf_output_dir, "iteration_",
                            sprintf("%03d", i), ".rds")
      saveRDS(list(
        iteration = i,
        timestamp = Sys.time(),
        raw_response = result,
        conference_date = conf_date,
        conference_label = transcript$label
      ), file = output_file)

      successful <- successful + 1
    } else {
      write(paste0(Sys.time(), " - ", conf_date, " iteration ", i, ": API call failed"),
            file = log_file, append = TRUE)
      failed <- failed + 1
    }

    # Small delay to avoid rate limiting
    Sys.sleep(0.3)
  }

  cat(crayon::green(paste0("  Completed: ", successful, "/", N_SIMULATIONS, " successful\n")))

  return(list(successful = successful, failed = failed, skipped = FALSE))
}

#' Run all API calls for multiple conferences
#'
#' @param conferences Character vector. Conference dates to process (defaults to TEST_CONFERENCES)
#'
#' @return List with summary statistics
run_temperature_robustness <- function(conferences = TEST_CONFERENCES) {

  cat(crayon::blue("\n============================================\n"))
  cat(crayon::blue("STEP 1: RUNNING API CALLS (MULTI-CONFERENCE)\n"))
  cat(crayon::blue("============================================\n\n"))

  cat(paste0("Conferences to process: ", length(conferences), "\n"))
  cat(paste0("Simulations per conference: ", N_SIMULATIONS, "\n"))
  cat(paste0("Total API calls: ", length(conferences) * N_SIMULATIONS, "\n\n"))

  # Save conference list metadata
  saveRDS(list(
    conferences = conferences,
    labels = CONFERENCE_LABELS[conferences],
    n_simulations = N_SIMULATIONS,
    temperature = TEMPERATURE,
    model = MODEL,
    trader = FIXED_TRADER
  ), file = paste0(OUTPUT_DIR, "run_metadata.rds"))

  # Set up log file
  log_file <- paste0(OUTPUT_DIR, "api_errors_", Sys.Date(), ".log")
  if (file.exists(log_file)) file.remove(log_file)

  start_time <- Sys.time()

  # Process each conference
  results_list <- list()

  for (conf_date in conferences) {
    result <- run_single_conference(conf_date, log_file)
    results_list[[conf_date]] <- result
  }

  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "mins")

  # Aggregate results
  total_successful <- sum(sapply(results_list, function(x) x$successful))
  total_failed <- sum(sapply(results_list, function(x) x$failed))
  total_expected <- length(conferences) * N_SIMULATIONS

  # Summary
  cat(crayon::green("\n============================================\n"))
  cat(crayon::green("API CALLS COMPLETE\n"))
  cat(crayon::green("============================================\n"))
  cat(paste0("Total successful: ", total_successful, "/", total_expected, "\n"))
  cat(paste0("Total failed: ", total_failed, "\n"))
  cat(paste0("Success rate: ", round(total_successful/total_expected * 100, 1), "%\n"))
  cat(paste0("Total time: ", round(total_time, 1), " minutes\n"))
  cat(paste0("Raw responses saved to: ", RAW_RESPONSES_DIR, "\n"))
  cat(crayon::yellow("\nNext step: Run clean_temperature_results()\n"))

  return(list(
    by_conference = results_list,
    total_successful = total_successful,
    total_failed = total_failed,
    total_time = total_time,
    conferences = conferences
  ))
}

# ==============================================================================
# STEP 2: CLEAN AND PARSE RAW RESPONSES
# ==============================================================================

#' Parse a single markdown table from LLM response
#'
#' @param raw_response Character. Raw LLM response
#' @param iteration Numeric. Iteration number
#'
#' @return Data frame or NULL on failure
parse_single_response <- function(raw_response, iteration) {

  # Expected column names
  names_col <- c("date", "id", "tenor", "direction", "rate", "confidence")

  tryCatch({
    # Split into lines and clean up
    lines <- raw_response |>
      str_split("\n") |>
      unlist() |>
      str_trim()

    # Keep only lines that look like table rows (contain | and data)
    # Filter out: empty lines, separator rows (only -, |, :, spaces)
    table_lines <- lines |>
      keep(~ str_detect(.x, "\\|")) |>
      discard(~ str_detect(.x, "^[\\|\\-:\\s]+$")) |>
      discard(~ nchar(.x) < 10)

    # Need at least 4 rows (header + 3 data rows)
    if (length(table_lines) < 4) {
      return(NULL)
    }

    # Skip header row (first row with column names)
    data_lines <- table_lines[-1]

    # Parse each data row
    parsed_rows <- map(data_lines, function(line) {
      # Split by | and clean
      cells <- str_split(line, "\\|")[[1]] |>
        str_trim() |>
        discard(~ .x == "")

      # Should have exactly 6 columns
      if (length(cells) == 6) {
        return(cells)
      } else {
        return(NULL)
      }
    }) |>
      compact()

    # Need at least 3 valid rows (3M, 2Y, 10Y)
    if (length(parsed_rows) < 3) {
      return(NULL)
    }

    # Convert to data frame
    parsed <- do.call(rbind, parsed_rows) |>
      as.data.frame(stringsAsFactors = FALSE)

    names(parsed) <- names_col

    # Clean and validate
    parsed <- parsed |>
      mutate(
        iteration = iteration,
        date = str_trim(date),
        id = str_trim(id),
        tenor = str_trim(tenor),
        direction = str_trim(direction),
        rate = as.numeric(str_extract(rate, "[0-9.-]+")),
        confidence = as.numeric(str_extract(confidence, "[0-9]+"))
      ) |>
      filter(
        tenor %in% c("3M", "2Y", "10Y"),
        !is.na(rate),
        rate > -10 & rate < 20  # Sanity check on rates
      )

    if (nrow(parsed) >= 3) {
      return(parsed)
    } else {
      return(NULL)
    }

  }, error = function(e) {
    return(NULL)
  })
}

#' Clean all raw responses and create structured dataset (multi-conference)
#'
#' @return Data frame with all parsed results including conference_date
clean_temperature_results <- function() {

  cat(crayon::blue("\n============================================\n"))
  cat(crayon::blue("STEP 2: CLEANING RAW RESPONSES (MULTI-CONFERENCE)\n"))
  cat(crayon::blue("============================================\n\n"))

  # Find all conference subdirectories
  conf_dirs <- list.dirs(RAW_RESPONSES_DIR, full.names = TRUE, recursive = FALSE)

  # Also check for files directly in RAW_RESPONSES_DIR (backwards compatibility)
  direct_files <- list.files(RAW_RESPONSES_DIR,
                             pattern = "iteration_.*\\.rds$",
                             full.names = TRUE)

  if (length(conf_dirs) == 0 && length(direct_files) == 0) {
    stop("No raw response files found in ", RAW_RESPONSES_DIR, "\n",
         "Please run run_temperature_robustness() first.")
  }

  # Collect all files with their conference dates
  all_files <- list()

  # Files in subdirectories (new multi-conference format)
  for (conf_dir in conf_dirs) {
    conf_date <- basename(conf_dir)
    files <- list.files(conf_dir, pattern = "iteration_.*\\.rds$", full.names = TRUE)
    for (f in files) {
      all_files[[length(all_files) + 1]] <- list(file = f, conf_date = conf_date)
    }
  }

  # Files directly in root (backwards compatibility)
  for (f in direct_files) {
    # Try to extract conference date from file content
    all_files[[length(all_files) + 1]] <- list(file = f, conf_date = NA)
  }

  cat(paste0("Found ", length(all_files), " raw response files\n"))
  cat(paste0("Conferences: ", paste(unique(sapply(all_files, function(x) x$conf_date)), collapse = ", "), "\n\n"))

  # Parse each file
  all_results <- list()
  successful <- 0
  failed <- 0
  failed_details <- list()

  for (item in all_files) {
    data <- readRDS(item$file)
    iteration <- data$iteration

    # Get conference date from file or metadata
    conf_date <- if (!is.na(item$conf_date)) {
      item$conf_date
    } else if (!is.null(data$conference_date)) {
      data$conference_date
    } else {
      "unknown"
    }

    # Get conference label
    conf_label <- if (!is.null(data$conference_label)) {
      data$conference_label
    } else if (conf_date %in% names(CONFERENCE_LABELS)) {
      CONFERENCE_LABELS[conf_date]
    } else {
      conf_date
    }

    parsed <- parse_single_response(data$raw_response, iteration)

    if (!is.null(parsed)) {
      # Add conference metadata
      parsed <- parsed |>
        mutate(
          conference_date = conf_date,
          conference_label = conf_label
        )
      all_results[[length(all_results) + 1]] <- parsed
      successful <- successful + 1
    } else {
      failed <- failed + 1
      failed_details[[length(failed_details) + 1]] <- list(
        conf_date = conf_date,
        iteration = iteration
      )
    }
  }

  # Combine all results
  if (length(all_results) > 0) {
    combined_results <- bind_rows(all_results)

    # Summary by conference
    conf_summary <- combined_results |>
      group_by(conference_date) |>
      summarise(
        n_iterations = n_distinct(iteration),
        n_rows = n(),
        .groups = "drop"
      )

    cat("\nParsing summary by conference:\n")
    print(conf_summary)

    # Save cleaned results
    saveRDS(combined_results,
            file = paste0(OUTPUT_DIR, "temperature_robustness_cleaned.rds"))
    writexl::write_xlsx(combined_results,
                        path = paste0(OUTPUT_DIR, "temperature_robustness_cleaned.xlsx"))

    cat(crayon::green("\n============================================\n"))
    cat(crayon::green("CLEANING COMPLETE\n"))
    cat(crayon::green("============================================\n"))
    cat(paste0("Successfully parsed: ", successful, "/", length(all_files), "\n"))
    cat(paste0("Failed to parse: ", failed, "\n"))
    cat(paste0("Parse success rate: ", round(successful/length(all_files) * 100, 1), "%\n"))
    cat(paste0("Conferences: ", n_distinct(combined_results$conference_date), "\n"))

    if (length(failed_details) > 0 && length(failed_details) <= 20) {
      cat(crayon::yellow("\nFailed iterations:\n"))
      for (fd in failed_details) {
        cat(paste0("  ", fd$conf_date, " iteration ", fd$iteration, "\n"))
      }
    } else if (length(failed_details) > 20) {
      cat(crayon::yellow(paste0("\n", length(failed_details), " iterations failed (too many to list)\n")))
    }

    cat(paste0("\nResults saved to: ", OUTPUT_DIR, "temperature_robustness_cleaned.rds\n"))
    cat(crayon::yellow("\nNext step: Run analyze_temperature_results()\n"))

    return(combined_results)

  } else {
    cat(crayon::red("\nNo results could be parsed. Check raw responses manually.\n"))
    return(NULL)
  }
}

# ==============================================================================
# STEP 3: ANALYSIS AND VISUALIZATION
# ==============================================================================

#' Load cross-agent dispersion data from main analysis (for all conferences)
#'
#' @param conf_dates Character vector. Conference dates to filter (YYYY-MM-DD)
#'
#' @return Data frame with cross-agent stats by conference and tenor, or NULL
load_cross_agent_data_multi <- function(conf_dates) {

  # Find most recent aggregate result file
  aggregate_dir <- "../intermediate_data/aggregate_gemini_result/prompt_naive/"

  if (!dir.exists(aggregate_dir)) {
    cat(crayon::yellow("Cross-agent data directory not found. Skipping signal band.\n"))
    return(NULL)
  }

  files <- list.files(aggregate_dir, pattern = "\\.xlsx$", full.names = TRUE)

  if (length(files) == 0) {
    cat(crayon::yellow("No cross-agent data files found. Skipping signal band.\n"))
    return(NULL)
  }

  # Use most recent file
  latest_file <- files[length(files)]
  cat(paste0("Loading cross-agent data from: ", basename(latest_file), "\n"))

  tryCatch({
    agent_data <- readxl::read_xlsx(latest_file) |>
      filter(date %in% conf_dates) |>
      group_by(date, tenor) |>
      summarise(
        n_agents = n(),
        mean_rate_agents = mean(rate, na.rm = TRUE),
        sd_rate_agents = sd(rate, na.rm = TRUE),
        iqr_rate_agents = IQR(rate, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(tenor %in% c("3M", "2Y", "10Y")) |>
      rename(conference_date = date)

    if (nrow(agent_data) == 0) {
      cat(crayon::yellow("No cross-agent data found for any conference dates.\n"))
      return(NULL)
    }

    cat(paste0("Found cross-agent data for ", n_distinct(agent_data$conference_date), " conferences.\n"))
    return(agent_data)

  }, error = function(e) {
    cat(crayon::yellow(paste0("Error loading cross-agent data: ", e$message, "\n")))
    return(NULL)
  })
}

#' Analyze parsed results and create visualizations
#'
#' @param results Data frame with parsed results (optional, loads from file if NULL)
#'
#' @return List with summary statistics and plots
analyze_temperature_results <- function(results = NULL) {

  # Load results if not provided
  if (is.null(results)) {
    results_file <- paste0(OUTPUT_DIR, "temperature_robustness_cleaned.rds")
    if (!file.exists(results_file)) {
      stop("No cleaned results file found. Run clean_temperature_results() first.")
    }
    results <- readRDS(results_file)
  }

  cat(crayon::blue("\n============================================\n"))
  cat(crayon::blue("STEP 3: ANALYSIS AND VISUALIZATION\n"))
  cat(crayon::blue("============================================\n\n"))

  n_iterations <- length(unique(results$iteration))
  cat(paste0("Analyzing ", n_iterations, " iterations\n\n"))

  # Get conference date from transcript metadata
  transcript_meta <- readRDS(paste0(OUTPUT_DIR, "transcript_metadata.rds"))
  conf_date <- transcript_meta$date
  cat(paste0("Conference date: ", conf_date, "\n\n"))

  # ============================================
  # CONVERT TO BASIS POINTS
  # ============================================

  # Convert rates from % to basis points (1% = 100 bps)
  # We compute deviation from mean in bps for cleaner comparison
  results_bps <- results |>
    group_by(tenor) |>
    mutate(
      rate_bps = rate * 100,  # Convert to bps
      mean_rate_bps = mean(rate_bps, na.rm = TRUE),
      deviation_bps = rate_bps - mean_rate_bps  # Deviation from mean
    ) |>
    ungroup()

  # ============================================
  # SUMMARY STATISTICS (in bps)
  # ============================================

  summary_stats <- results_bps |>
    group_by(tenor) |>
    summarise(
      n = n(),
      mean_rate_bps = mean(rate_bps, na.rm = TRUE),
      sd_temp_bps = sd(rate_bps, na.rm = TRUE),  # Temperature-driven SD
      min_rate_bps = min(rate_bps, na.rm = TRUE),
      max_rate_bps = max(rate_bps, na.rm = TRUE),
      range_bps = max_rate_bps - min_rate_bps,
      iqr_bps = IQR(rate_bps, na.rm = TRUE),
      mean_confidence = mean(confidence, na.rm = TRUE),
      sd_confidence = sd(confidence, na.rm = TRUE),
      .groups = "drop"
    )

  cat("Summary Statistics by Tenor (in basis points):\n")
  print(summary_stats |> mutate(across(where(is.numeric), ~ round(.x, 2))))

  # ============================================
  # LOAD CROSS-AGENT DATA FOR SIGNAL BAND
  # ============================================

  agent_data <- load_cross_agent_data(conf_date)

  # Merge with temperature stats
  if (!is.null(agent_data)) {
    # Convert agent data to bps
    agent_data <- agent_data |>
      mutate(
        mean_rate_agents_bps = mean_rate_agents * 100,
        sd_agents_bps = sd_rate_agents * 100,
        iqr_agents_bps = iqr_rate_agents * 100
      )

    summary_stats <- summary_stats |>
      left_join(agent_data |> select(tenor, sd_agents_bps, iqr_agents_bps, n_agents),
                by = "tenor") |>
      mutate(
        # Variance Ratio: temperature variance / agent variance
        VR = (sd_temp_bps^2) / (sd_agents_bps^2),
        # Noise Fraction: temperature SD as fraction of agent SD
        NF = sd_temp_bps / sd_agents_bps
      )

    cat("\n\nComparison with Cross-Agent Dispersion:\n")
    comparison_df <- summary_stats |>
      select(tenor, sd_temp_bps, sd_agents_bps, VR, NF) |>
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    print(comparison_df)
  }

  # Direction distribution
  direction_dist <- results |>
    group_by(tenor, direction) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(tenor) |>
    mutate(proportion = round(count / sum(count) * 100, 1)) |>
    arrange(tenor, desc(count))

  cat("\n\nDirection Distribution by Tenor:\n")
  print(direction_dist)

  # Save summary tables
  writexl::write_xlsx(
    list(
      summary = summary_stats,
      direction = direction_dist
    ),
    path = paste0(OUTPUT_DIR, "temperature_robustness_summary.xlsx")
  )

  # ============================================
  # VISUALIZATIONS (in basis points)
  # ============================================

  color_palette <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

  # Bin width in bps (use 5 bps for consistency)
  BIN_WIDTH_BPS <- 5

  # Create facet labels with stats
  facet_labels <- summary_stats |>
    mutate(
      label = if (!is.null(agent_data) && "sd_agents_bps" %in% names(summary_stats)) {
        paste0(tenor, "\nSD_temp = ", round(sd_temp_bps, 1), " bps",
               "  |  SD_agents = ", round(sd_agents_bps, 1), " bps",
               "  |  VR = ", round(VR, 2),
               "  |  NF = ", round(NF, 2))
      } else {
        paste0(tenor, "\nSD_temp = ", round(sd_temp_bps, 1), " bps",
               "  |  Range = ", round(range_bps, 1), " bps")
      }
    ) |>
    select(tenor, label)

  # Add labels to data
  results_bps <- results_bps |>
    left_join(facet_labels, by = "tenor")

  # ============================================
  # MAIN PLOT: Histogram with Signal Band
  # ============================================

  # Prepare signal band data (±1 SD of cross-agent dispersion)
  if (!is.null(agent_data)) {
    signal_band <- summary_stats |>
      mutate(
        band_lower = -sd_agents_bps,  # Relative to mean (deviation)
        band_upper = sd_agents_bps
      ) |>
      left_join(facet_labels, by = "tenor")
  }

  # Calculate common x-axis limits (symmetric around 0)
  max_dev <- max(abs(results_bps$deviation_bps), na.rm = TRUE)
  if (!is.null(agent_data)) {
    max_dev <- max(max_dev, max(summary_stats$sd_agents_bps, na.rm = TRUE) * 1.5)
  }
  x_limit <- ceiling(max_dev / 10) * 10  # Round up to nearest 10 bps

  # Main histogram plot with signal band
  p_main <- ggplot(results_bps, aes(x = deviation_bps, fill = tenor)) +
    # Signal band (if available)
    {if (!is.null(agent_data))
      geom_rect(data = signal_band,
                aes(xmin = band_lower, xmax = band_upper,
                    ymin = -Inf, ymax = Inf),
                fill = "grey80", alpha = 0.4, inherit.aes = FALSE)
    } +
    # Histogram
    geom_histogram(binwidth = BIN_WIDTH_BPS, alpha = 0.7, color = "white") +
    # Mean line (at 0 since we're showing deviation)
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
    # Facet with custom labels
    facet_wrap(~label, ncol = 1, scales = "fixed") +
    # Common x-axis scale
    scale_x_continuous(
      limits = c(-x_limit, x_limit),
      breaks = seq(-x_limit, x_limit, by = 20)
    ) +
    scale_fill_manual(values = color_palette) +
    labs(
      title = "Temperature-Driven Variation vs Cross-Agent Dispersion",
      subtitle = paste0("N = ", n_iterations, " simulations | Temperature = ", TEMPERATURE,
                        " | Conference: ", conf_date,
                        if (!is.null(agent_data)) "\nGrey band = ±1 SD of 30-agent dispersion (signal)" else ""),
      x = "Deviation from Mean (basis points)",
      y = "Count",
      caption = paste0("Fixed trader: ", FIXED_TRADER$interpretation_style,
                       " with ", FIXED_TRADER$behavioral_bias, " bias | Bin width = ", BIN_WIDTH_BPS, " bps")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      strip.text = element_text(size = 10, face = "bold", hjust = 0),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )

  # ============================================
  # SUPPLEMENTARY PLOTS
  # ============================================

  # Plot 2: Boxplot comparison (in bps)
  p_box <- ggplot(results_bps, aes(x = tenor, y = deviation_bps, fill = tenor)) +
    # Signal band whiskers (if available)
    {if (!is.null(agent_data))
      geom_errorbar(data = signal_band,
                    aes(x = tenor, ymin = band_lower, ymax = band_upper),
                    width = 0.3, linewidth = 1.2, color = "grey50", inherit.aes = FALSE)
    } +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    geom_jitter(alpha = 0.15, width = 0.15, size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_fill_manual(values = color_palette) +
    scale_y_continuous(limits = c(-x_limit, x_limit)) +
    labs(
      title = "Rate Predictions by Tenor",
      subtitle = if (!is.null(agent_data)) "Grey bars = ±1 SD cross-agent dispersion" else "",
      x = "Tenor",
      y = "Deviation from Mean (bps)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "none"
    )

  # Plot 3: Direction distribution
  p_dir <- ggplot(direction_dist, aes(x = direction, y = proportion, fill = tenor)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_text(aes(label = paste0(proportion, "%")),
              position = position_dodge(width = 0.9),
              vjust = -0.3, size = 3) +
    facet_wrap(~tenor, ncol = 3) +
    scale_fill_manual(values = color_palette) +
    labs(
      title = "Direction Predictions",
      subtitle = "Proportion of Up/Down/Unchanged across simulations",
      x = "Direction",
      y = "Proportion (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "none"
    )

  # Plot 4: Confidence distribution
  p_conf <- ggplot(results_bps, aes(x = confidence, fill = tenor)) +
    geom_histogram(bins = 15, alpha = 0.7, color = "white") +
    facet_wrap(~tenor, ncol = 3) +
    scale_fill_manual(values = color_palette) +
    labs(
      title = "Distribution of Confidence Scores",
      x = "Confidence (%)",
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "none"
    )

  # Save plots
  ggsave(paste0(FIGURES_DIR, "temperature_vs_agents.pdf"), p_main,
         width = 10, height = 12, dpi = 320)
  ggsave(paste0(FIGURES_DIR, "temperature_boxplot.pdf"), p_box,
         width = 8, height = 6, dpi = 320)
  ggsave(paste0(FIGURES_DIR, "temperature_direction.pdf"), p_dir,
         width = 12, height = 5, dpi = 320)
  ggsave(paste0(FIGURES_DIR, "temperature_confidence.pdf"), p_conf,
         width = 12, height = 5, dpi = 320)

  cat(crayon::green(paste0("\nFigures saved to: ", FIGURES_DIR, "\n")))

  # ============================================
  # KEY FINDINGS
  # ============================================

  cat(crayon::blue("\n============================================\n"))
  cat(crayon::blue("KEY FINDINGS\n"))
  cat(crayon::blue("============================================\n"))

  avg_sd_temp <- mean(summary_stats$sd_temp_bps, na.rm = TRUE)
  avg_range <- mean(summary_stats$range_bps, na.rm = TRUE)

  cat(paste0("\nAverage temperature SD: ", round(avg_sd_temp, 2), " bps\n"))
  cat(paste0("Average range (max-min): ", round(avg_range, 2), " bps\n"))

  if (!is.null(agent_data) && "sd_agents_bps" %in% names(summary_stats)) {
    avg_sd_agents <- mean(summary_stats$sd_agents_bps, na.rm = TRUE)
    avg_VR <- mean(summary_stats$VR, na.rm = TRUE)
    avg_NF <- mean(summary_stats$NF, na.rm = TRUE)

    cat(paste0("Average cross-agent SD: ", round(avg_sd_agents, 2), " bps\n"))
    cat(paste0("Average Variance Ratio (VR): ", round(avg_VR, 3), "\n"))
    cat(paste0("Average Noise Fraction (NF): ", round(avg_NF, 3), "\n"))

    # Interpretation based on NF
    if (avg_NF < 0.3) {
      cat(crayon::green("\nINTERPRETATION: Temperature noise is SMALL relative to agent signal.\n"))
      cat("Temperature SD is <30% of cross-agent SD. Agent heterogeneity dominates.\n")
    } else if (avg_NF < 0.7) {
      cat(crayon::yellow("\nINTERPRETATION: Temperature noise is MODERATE relative to agent signal.\n"))
      cat("Consider using lower temperature or averaging multiple runs.\n")
    } else {
      cat(crayon::red("\nINTERPRETATION: Temperature noise is LARGE relative to agent signal.\n"))
      cat("Temperature variance >= 70% of agent variance. Results may be unreliable.\n")
    }
  } else {
    # Fallback interpretation without agent data
    if (avg_sd_temp < 10) {
      cat(crayon::green("\nINTERPRETATION: LOW temperature-driven variance (<10 bps).\n"))
    } else if (avg_sd_temp < 30) {
      cat(crayon::yellow("\nINTERPRETATION: MODERATE temperature-driven variance (10-30 bps).\n"))
    } else {
      cat(crayon::red("\nINTERPRETATION: HIGH temperature-driven variance (>30 bps).\n"))
    }
  }

  # Return all outputs
  return(list(
    results = results_bps,
    summary = summary_stats,
    direction = direction_dist,
    agent_data = agent_data,
    plots = list(main = p_main, box = p_box, direction = p_dir, confidence = p_conf),
    metrics = list(
      avg_sd_temp_bps = avg_sd_temp,
      avg_range_bps = avg_range,
      avg_sd_agents_bps = if (!is.null(agent_data)) mean(summary_stats$sd_agents_bps, na.rm = TRUE) else NA,
      avg_VR = if (!is.null(agent_data)) mean(summary_stats$VR, na.rm = TRUE) else NA,
      avg_NF = if (!is.null(agent_data)) mean(summary_stats$NF, na.rm = TRUE) else NA,
      n_iterations = n_iterations
    )
  ))
}

# ==============================================================================
# CONVENIENCE FUNCTION: RUN FULL PIPELINE
# ==============================================================================

#' Run complete temperature robustness analysis
#'
#' @return List with final analysis results
run_full_temperature_analysis <- function() {

  cat(crayon::blue("============================================\n"))
  cat(crayon::blue("FULL TEMPERATURE ROBUSTNESS PIPELINE\n"))
  cat(crayon::blue("============================================\n\n"))

  # Step 1: API calls
  api_results <- run_temperature_robustness()

  if (api_results$successful == 0) {
    stop("No successful API calls. Check your API key and connection.")
  }

  # Step 2: Parse responses
  cleaned_results <- clean_temperature_results()

  if (is.null(cleaned_results)) {
    stop("Parsing failed. Check raw responses manually.")
  }

  # Step 3: Analyze
  analysis <- analyze_temperature_results(cleaned_results)

  cat(crayon::green("\n============================================\n"))
  cat(crayon::green("PIPELINE COMPLETE\n"))
  cat(crayon::green("============================================\n"))

  return(analysis)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

if (interactive()) {
  cat(crayon::blue("\nTemperature robustness analysis ready.\n"))
  cat("Options:\n")
  cat("  1. Run full pipeline:     run_full_temperature_analysis()\n")
  cat("  2. Step-by-step:\n")
  cat("     a) run_temperature_robustness()  # API calls\n")
  cat("     b) clean_temperature_results()   # Parse responses\n")
  cat("     c) analyze_temperature_results() # Analysis\n")
}

# Uncomment to run automatically:
# analysis <- run_full_temperature_analysis()
