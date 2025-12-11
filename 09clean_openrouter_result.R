#===============================================================================
# CLEAN OPENROUTER LLM RESULTS - ROBUST PARSING
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Parse raw LLM responses from OpenRouter/Qwen3 into structured data.
#   Robust parsing with error handling outside the LLM call function.
#
# Input:
#   ../intermediate_data/openrouter_result/qwen3_naive/*.rds
#
# Output:
#   ../intermediate_data/aggregate_openrouter_result/qwen3_naive/qwen3_{date}.xlsx
#
# Usage:
#   source("09clean_openrouter_result.R")
#===============================================================================

# Load required packages -------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  readr,
  writexl,
  stringr,
  purrr
)

# Configuration ----------------------------------------------------------------
input_dir <- "../intermediate_data/openrouter_result/qwen3_naive/"
output_dir <- "../intermediate_data/aggregate_openrouter_result/qwen3_naive/"

# Create output directory if needed
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Column names for the output table
names_col <- c("date", "id", "tenor", "direction", "rate", "confidence")

# Get list of RDS files --------------------------------------------------------
cat("\n", strrep("=", 60), "\n")
cat("PARSING OPENROUTER/QWEN3 RESULTS\n")
cat(strrep("=", 60), "\n\n")

rds_files <- list.files(
  path = input_dir,
  pattern = "\\d{4}-\\d{2}-\\d{2}\\.rds$",
  full.names = TRUE
)

if (length(rds_files) == 0) {
  stop("No RDS files found in: ", input_dir)
}

cat("Found", length(rds_files), "RDS files to process\n\n")

# Extract dates from filenames
dates_from_files <- rds_files %>%
  basename() %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}")

# Parse markdown table from LLM response ---------------------------------------
#' Parse a single LLM response (markdown table) into a tibble
#'
#' @param response Character. Raw LLM response containing markdown table
#' @param conf_date Character. Conference date for error reporting
#'
#' @return tibble or NULL if parsing fails
parse_llm_response <- function(response, conf_date) {
  tryCatch({
    # Check if response is valid
    if (is.null(response) || length(response) == 0 || nchar(response) == 0) {
      cat("  Warning: Empty response for", conf_date, "\n")
      return(NULL)
    }

    # Extract markdown table from response
    # The table should have | delimiters
    lines <- strsplit(response, "\n")[[1]]

    # Find table lines (contain | character)
    table_lines <- lines[grepl("\\|", lines)]

    if (length(table_lines) < 3) {
      cat("  Warning: No valid table found for", conf_date, "\n")
      return(NULL)
    }

    # Remove separator line (contains only |, -, and spaces)
    table_lines <- table_lines[!grepl("^[\\|\\s\\-:]+$", table_lines)]

    # Reconstruct table as text
    table_text <- paste(table_lines, collapse = "\n")

    # Parse using read_delim
    parsed <- readr::read_delim(
      I(table_text),
      delim = "|",
      trim_ws = TRUE,
      show_col_types = FALSE
    )

    # Remove first and last columns (empty from table borders)
    if (ncol(parsed) > 2) {
      parsed <- parsed %>%
        select(-1, -ncol(.))
    }

    # Check if we have valid data
    if (nrow(parsed) < 2 || ncol(parsed) < 6) {
      cat("  Warning: Insufficient data in table for", conf_date, "\n")
      return(NULL)
    }

    # Set column names
    parsed <- parsed %>%
      setNames(names_col)

    # Remove header row if it contains column names
    if (any(grepl("Date|Trader|Tenor|Direction|Rate|Confidence",
                  parsed$date[1], ignore.case = TRUE))) {
      parsed <- parsed %>% slice(-1)
    }

    # Clean and convert data types
    parsed <- parsed %>%
      mutate(
        date = as.character(date),
        id = as.character(id),
        tenor = as.character(tenor),
        direction = as.character(direction),
        rate = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", rate))),
        confidence = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", confidence)))
      )

    cat("  Parsed", nrow(parsed), "rows for", conf_date, "\n")
    return(parsed)

  }, error = function(e) {
    cat("  Error parsing", conf_date, ":", e$message, "\n")
    return(NULL)
  })
}

# Process all files ------------------------------------------------------------
cat("Processing files...\n\n")

results <- map2(rds_files, dates_from_files, function(file_path, conf_date) {
  # Read RDS file
  response <- tryCatch({
    readRDS(file_path)
  }, error = function(e) {
    cat("  Error reading", conf_date, ":", e$message, "\n")
    return(NULL)
  })

  # Parse response
  parse_llm_response(response, conf_date)
})

# Combine results --------------------------------------------------------------
cat("\nCombining results...\n")

# Keep only successful parses
valid_results <- results %>%
  keep(~ !is.null(.x) && nrow(.x) > 0)

if (length(valid_results) == 0) {
  stop("No valid results to export. Check parsing errors above.")
}

# Combine all into single dataframe
clean_df <- valid_results %>%
  bind_rows() %>%
  # Filter to valid tenors
  filter(tenor %in% c("3M", "2Y", "10Y")) %>%
  # Remove any rows with all NA
  filter(!is.na(date) | !is.na(id) | !is.na(tenor))

# Summary statistics -----------------------------------------------------------
cat("\n", strrep("=", 60), "\n")
cat("PARSING SUMMARY\n")
cat(strrep("=", 60), "\n\n")
cat("Files processed:     ", length(rds_files), "\n")
cat("Successful parses:   ", length(valid_results), "\n")
cat("Failed parses:       ", length(rds_files) - length(valid_results), "\n")
cat("Total rows:          ", nrow(clean_df), "\n")
cat("Unique dates:        ", n_distinct(clean_df$date), "\n")
cat("Unique traders:      ", n_distinct(clean_df$id), "\n")
cat("\nTenor distribution:\n")
print(table(clean_df$tenor))
cat("\nDirection distribution:\n")
print(table(clean_df$direction))

# Export results ---------------------------------------------------------------
output_file <- paste0(output_dir, "qwen3_", Sys.Date(), ".xlsx")

writexl::write_xlsx(clean_df, output_file)

cat("\n", strrep("=", 60), "\n")
cat("Results exported to:", output_file, "\n")
cat(strrep("=", 60), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
