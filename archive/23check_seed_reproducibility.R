#===============================================================================
# SCRIPT 23: SEED REPRODUCIBILITY CHECK FOR QWEN3 t=1
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Test whether Qwen3 via OpenRouter honours the seed parameter at temperature=1.
#   Re-runs 3 conferences with seed=120 (same as the original t=1 run) and
#   compares outputs to the saved RDS files on two levels:
#     (1) Exact string match of the raw LLM response
#     (2) Parsed synthetic SD match (more lenient — rounding / whitespace safe)
#
#   Conclusion:
#     - Both match  → seed is honoured; the full t=1 run is reproducible.
#     - SDs match but strings differ → soft reproducibility (sufficient for paper).
#     - Neither matches → proceed to Script 24 (distribution test).
#
# Inputs:
#   ../intermediate_data/openrouter_result/qwen3_naive/*.rds  (original t=1 run)
#
# Outputs:
#   Printed report + ../intermediate_data/seed_check/seed_check_results.rds
#
# Cost: ~$0.30 (3 API calls)
#
# Usage:
#   source("23check_seed_reproducibility.R")
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("SEED REPRODUCIBILITY CHECK — Qwen3 t=1, seed=120\n")
cat(strrep("=", 80), "\n\n")

# Packages ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr2, readtext, crayon, stringr, purrr, tidyverse)

# Configuration ----------------------------------------------------------------
SEED        <- 120
TEMPERATURE <- 1
MODEL       <- "qwen/qwen3-235b-a22b-2507"

original_dir <- "../intermediate_data/openrouter_result/qwen3_naive"
output_dir   <- "../intermediate_data/seed_check"
texts_dir    <- "../intermediate_data/texts"

# Pick 3 conferences that span the date range and are already processed
CHECK_DATES <- c("2015-01-22",   # QE announcement — high ambiguity
                 "2019-06-06",   # Steady state    — low ambiguity
                 "2022-07-21")   # First 50bp hike — high surprise

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# API key ----------------------------------------------------------------------
api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (api_key == "") stop("OPENROUTER_API_KEY not set in .Renviron.")

# Load prompts and API wrapper -------------------------------------------------
source("config/prompts.R")
source("src/llm_api/openrouter_api.R")

#===============================================================================
# HELPERS
#===============================================================================

#' Parse a raw LLM markdown-table response into a tibble of synthetic SDs
parse_to_sd <- function(response, conf_date) {
  tryCatch({
    lines      <- strsplit(response, "\n")[[1]]
    data_lines <- lines[grepl("\\d{4}-\\d{2}-\\d{2}", lines)]
    if (length(data_lines) == 0) return(NULL)

    parsed_list <- lapply(data_lines, function(line) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      parts <- parts[parts != ""]
      if (length(parts) >= 5) {
        data.frame(
          tenor = parts[3],
          rate  = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", parts[5]))),
          stringsAsFactors = FALSE
        )
      } else NULL
    })

    bind_rows(parsed_list) %>%
      filter(tenor %in% c("3M", "2Y", "10Y"), !is.na(rate)) %>%
      group_by(tenor) %>%
      summarise(sd_rate = sd(rate, na.rm = TRUE), .groups = "drop") %>%
      mutate(date = conf_date)
  }, error = function(e) NULL)
}

#' Load a transcript by date
load_transcript <- function(conf_date) {
  files <- list.files(texts_dir, pattern = "\\.txt$", full.names = TRUE)
  match <- files[str_detect(files, conf_date)]
  if (length(match) == 0) stop(paste("Transcript not found:", conf_date))
  readtext::readtext(match[1])$text
}

#===============================================================================
# STEP 1: LOAD ORIGINAL RESPONSES
#===============================================================================

cat(crayon::blue("Loading original t=1 responses...\n\n"))

original <- map(CHECK_DATES, function(d) {
  path <- file.path(original_dir, paste0(d, ".rds"))
  if (!file.exists(path)) stop(paste("Original RDS not found:", path))
  readRDS(path)
}) %>% set_names(CHECK_DATES)

#===============================================================================
# STEP 2: RE-RUN WITH SAME SEED
#===============================================================================

cat(crayon::blue("Re-running 3 conferences with seed=120, temperature=1...\n\n"))

rerun <- map(CHECK_DATES, function(d) {
  cat(crayon::cyan(paste0("  Processing ", d, "...\n")))
  text <- load_transcript(d)

  prompt <- gsub("\\[date\\]", d, prompt_naive)
  prompt <- paste0(prompt, "Press Conference on ", d, "\nText:", text, "\n\n")

  result <- tryCatch(
    new_openrouter(prompt, model = MODEL, temperature = TEMPERATURE,
                   seed = SEED, max_tokens = 100000),
    error = function(e) {
      cat(crayon::red(paste0("  API error for ", d, ": ", e$message, "\n")))
      NULL
    }
  )

  saveRDS(result, file.path(output_dir, paste0(d, "_rerun.rds")))
  cat(crayon::green(paste0("  Done: ", d, "\n")))
  result
}) %>% set_names(CHECK_DATES)

#===============================================================================
# STEP 3: COMPARE
#===============================================================================

cat(crayon::blue("\nComparing original vs re-run...\n\n"))

results <- map_dfr(CHECK_DATES, function(d) {

  orig <- original[[d]]
  rerun_resp <- rerun[[d]]

  if (is.null(rerun_resp)) {
    return(tibble(date = d, exact_match = NA, sd_match = NA,
                  max_sd_diff = NA, note = "API call failed"))
  }

  # (1) Exact string match
  exact <- identical(orig, rerun_resp)

  # (2) Parsed SD match (tolerance: 0.001 percentage points)
  sd_orig  <- parse_to_sd(orig,       d)
  sd_rerun <- parse_to_sd(rerun_resp, d)

  if (is.null(sd_orig) || is.null(sd_rerun)) {
    return(tibble(date = d, exact_match = exact, sd_match = NA,
                  max_sd_diff = NA, note = "Parsing failed"))
  }

  comparison <- inner_join(sd_orig, sd_rerun, by = c("date", "tenor"),
                           suffix = c("_orig", "_rerun")) %>%
    mutate(diff = abs(sd_rate_orig - sd_rate_rerun))

  max_diff <- max(comparison$diff, na.rm = TRUE)
  sd_ok    <- max_diff < 0.001

  tibble(date = d, exact_match = exact, sd_match = sd_ok,
         max_sd_diff = round(max_diff, 6), note = "OK")
})

#===============================================================================
# STEP 4: REPORT
#===============================================================================

cat("\n", strrep("=", 60), "\n")
cat("RESULTS\n")
cat(strrep("=", 60), "\n\n")
print(results)

n_exact <- sum(results$exact_match, na.rm = TRUE)
n_sd    <- sum(results$sd_match,    na.rm = TRUE)
n_total <- nrow(results)

cat("\n", strrep("=", 60), "\n")
cat("VERDICT\n")
cat(strrep("=", 60), "\n\n")

if (n_exact == n_total) {
  cat(crayon::green(
    "STRONG REPRODUCIBILITY: All responses are character-identical.\n"
  ))
  cat("Qwen3 fully honours seed=120. The t=1 run is perfectly reproducible.\n")
  cat("No need to run Script 24.\n")

} else if (n_sd == n_total) {
  cat(crayon::yellow(
    "SOFT REPRODUCIBILITY: Strings differ but parsed SDs match within 0.001pp.\n"
  ))
  cat("Likely whitespace / formatting variation. Synthetic disagreement measure\n")
  cat("is stable. Sufficient for publication.\n")
  cat("No need to run Script 24.\n")

} else {
  cat(crayon::red(
    "SEED NOT HONOURED: Parsed SDs differ across runs.\n"
  ))
  cat(paste0("SD match: ", n_sd, "/", n_total, " conferences.\n"))
  cat("Proceed to Script 24 to characterise the distribution of correlations.\n")
}

# Save results -----------------------------------------------------------------
saveRDS(results, file.path(output_dir, "seed_check_results.rds"))
cat(crayon::blue(paste0("\nFull results saved to: ", output_dir,
                        "/seed_check_results.rds\n")))

cat("\n", strrep("=", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
