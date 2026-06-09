#===============================================================================
# COUNTERFACTUAL ENSEMBLE P1 — High-Vol ECB Conferences x 2 Versions x R=10
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Counterfactual ensemble: high-vol ECB conferences x 2 versions (original,
#   chatgpt-edited) x 10 seeds via OpenRouter. Parallelised with furrr (5
#   workers). Per-call RDS caching for resumability.
#
# Data sources:
#   Dates:    ../intermediate_data/counterfactual_cross_llm_high_vol/sample_conferences_high_vol.csv
#   Original: ../intermediate_data/texts/{date}_{president}.txt
#   Edited:   ../intermediate_data/counterfactual_cross_llm_high_vol/raw_data/exp_a/{date}_chatgpt_edit.txt
#
# Outputs (ALL under intermediate_data/counterfactual_ensemble_p1/):
#   runs/{date}_{version}_{run}.rds
#   failed_calls.log
#   missing_grid.rds              (only if incomplete)
#   parsed/cf_runs_long.{rds,xlsx}
#   parsed/cf_within_run.{rds,xlsx}
#   parsed/cf_ensemble.{rds,xlsx}
#   parsed/cf_treatment_effect.{rds,xlsx}
#
# Usage:
#   Rscript code/run_counterfactual_ensemble_p1.R
#   source("run_counterfactual_ensemble_p1.R")   # from code/ directory
#   Re-run at any time — completed calls are skipped automatically.
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("COUNTERFACTUAL ENSEMBLE P1 — High-Vol Conferences x 2 Versions x R=10\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# HARD ASSERTION: ALL OUTPUTS MUST GO UNDER counterfactual_ensemble_p1/
# ==============================================================================

output_root <- normalizePath(
  "../intermediate_data/counterfactual_ensemble_p1",
  mustWork = FALSE
)
if (!grepl("counterfactual_ensemble_p1", output_root, fixed = TRUE)) {
  stop(
    "HARD ASSERT: output root does not contain 'counterfactual_ensemble_p1'. ",
    "Refusing to write outputs elsewhere.\nGot: ", output_root
  )
}

# ==============================================================================
# 1. SETUP
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  httr2, crayon, stringr, purrr, readr, readtext, readxl,
  writexl, tidyverse, future, furrr, tictoc
)

api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (nchar(api_key) == 0) stop("OPENROUTER_API_KEY not set. Add it to .Renviron.")

source("config/prompts.R")
prompt_template     <- prompt_naive
name_prompt_request <- "naive_counterfactual_R10"

cat(crayon::green(paste0("Loaded prompt: prompt_naive  [", name_prompt_request, "]\n\n")))

runs_dir   <- "../intermediate_data/counterfactual_ensemble_p1/runs"
parsed_dir <- "../intermediate_data/counterfactual_ensemble_p1/parsed"
log_file   <- "../intermediate_data/counterfactual_ensemble_p1/failed_calls.log"

dir.create(runs_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(parsed_dir, recursive = TRUE, showWarnings = FALSE)

tic("Total script")

# ==============================================================================
# 2. FILE-SYSTEM INVENTORY
# ==============================================================================

cat(crayon::blue("=== FILE-SYSTEM INVENTORY ===\n"))
cat("Top-level intermediate_data/ dirs:\n")
print(list.dirs("../intermediate_data", recursive = FALSE))

cf_dir     <- "../intermediate_data/counterfactual_cross_llm_high_vol"
edit_dir   <- file.path(cf_dir, "raw_data", "exp_a")
dates_file <- file.path(cf_dir, "sample_conferences_high_vol.csv")
texts_dir  <- "../intermediate_data/texts"

cat("\nChecking required paths:\n")
for (d in c(cf_dir, edit_dir, dates_file, texts_dir)) {
  if (file.exists(d)) {
    cat(crayon::green(paste0("  OK:      ", d, "\n")))
  } else {
    cat(crayon::red(paste0("  MISSING: ", d, "\n")))
  }
}

cat("\nEdited transcript files in exp_a/:\n")
edit_files <- list.files(edit_dir, pattern = "_chatgpt_edit\\.txt$",
                         full.names = FALSE)
cat(paste0("  Found ", length(edit_files), " *_chatgpt_edit.txt files\n\n"))

# ==============================================================================
# 3. DATA LOAD
# ==============================================================================

cat(crayon::blue("Loading conference dates from sample_conferences_high_vol.csv...\n"))

the_dates <- readr::read_csv(dates_file, show_col_types = FALSE) %>%
  filter(!is.na(date)) %>%
  pull(date) %>%
  as.Date()

n_dates <- length(the_dates)
cat(crayon::green(paste0(
  "  ", n_dates, " valid dates loaded (NA row excluded).\n",
  "  Note: spec specified ~30 conferences; actual count from CSV is ", n_dates, ".\n\n"
)))

# Load original transcripts --------------------------------------------------
cat(crayon::blue("Loading original transcripts from texts/...\n"))

text_files <- list.files(texts_dir, pattern = "\\.txt$", full.names = TRUE)
file_dates <- str_extract(basename(text_files), "\\d{4}-\\d{2}-\\d{2}")
valid      <- !is.na(file_dates)
text_map   <- set_names(text_files[valid], file_dates[valid])

originals <- map_chr(as.character(the_dates), function(d) {
  if (d %in% names(text_map)) {
    return(tryCatch(readtext::readtext(text_map[[d]])$text,
                    error = function(e) NA_character_))
  }
  # ±3-day fallback for date mismatches between CSV and transcript filenames
  d_date  <- as.Date(d)
  nearby  <- names(text_map)[abs(as.Date(names(text_map)) - d_date) <= 3]
  if (length(nearby) > 0) {
    cat(crayon::yellow(paste0(
      "  Near-date match: ", d, " → ", nearby[1], " (diff ",
      as.integer(abs(as.Date(nearby[1]) - d_date)), " day(s))\n"
    )))
    return(tryCatch(readtext::readtext(text_map[[nearby[1]]])$text,
                    error = function(e) NA_character_))
  }
  NA_character_
})
originals <- set_names(originals, as.character(the_dates))

# Load edited transcripts ----------------------------------------------------
cat(crayon::blue("Loading edited transcripts from exp_a/...\n"))

edited <- map_chr(as.character(the_dates), function(d) {
  path <- file.path(edit_dir, paste0(d, "_chatgpt_edit.txt"))
  if (!file.exists(path)) return(NA_character_)
  tryCatch(readr::read_file(path), error = function(e) NA_character_)
})
edited <- set_names(edited, as.character(the_dates))

# Exclude dates with no original or edited transcript (warn, don't stop) -----
missing_orig <- as.character(the_dates)[is.na(originals) | nchar(trimws(originals)) == 0]
missing_edit <- as.character(the_dates)[is.na(edited)    | nchar(trimws(edited))    == 0]
drop_dates   <- union(missing_orig, missing_edit)

if (length(drop_dates) > 0) {
  cat(crayon::yellow(paste0(
    "  WARNING: No transcript found for ", length(drop_dates), " date(s): ",
    paste(drop_dates, collapse = ", "),
    "\n  These dates will be excluded from the grid.\n\n"
  )))
  keep       <- !(as.character(the_dates) %in% drop_dates)
  the_dates  <- the_dates[keep]
  originals  <- originals[keep]
  edited     <- edited[keep]
  n_dates    <- length(the_dates)
}

stopifnot(
  "originals length mismatch" = length(originals) == n_dates,
  "edited length mismatch"    = length(edited)    == n_dates,
  "remaining NA in originals" = !any(is.na(originals)),
  "remaining NA in edited"    = !any(is.na(edited))
)

cat(crayon::green("All data assertions passed.\n\n"))

# Sample sanity-check --------------------------------------------------------
check_date <- as.character(the_dates[1])
cat(crayon::cyan(paste0("=== Sample: ", check_date, " — ORIGINAL (first 200 chars) ===\n")))
cat(substr(originals[[check_date]], 1, 200), "\n\n")
cat(crayon::cyan(paste0("=== Sample: ", check_date, " — EDITED (first 200 chars) ===\n")))
cat(substr(edited[[check_date]], 1, 200), "\n\n")

# ==============================================================================
# 4. GRID
# ==============================================================================

grid <- expand_grid(
  date    = as.character(the_dates),
  version = c("original", "edited"),
  run     = 1:10
) %>%
  sample_n(nrow(.))

n_expected <- n_dates * 2 * 10

cat(crayon::green(paste0(
  "Grid ready: ", nrow(grid), " rows (",
  n_distinct(grid$date), " dates x 2 versions x 10 runs)\n\n"
)))

# ==============================================================================
# 5. OPENROUTER CALLER — verbatim from run_full_ensemble_p1.R
# ==============================================================================

#' Call OpenRouter API (Gemini 2.5-Flash)
#'
#' @param prompt      Full prompt string
#' @param seed        Integer seed for reproducibility
#' @param temperature Numeric (default 1)
#' @param model       OpenRouter model id
#'
#' @return Character response or NULL on persistent failure
call_openrouter <- function(prompt,
                            seed,
                            temperature = 1,
                            model       = "google/gemini-2.5-flash") {

  url  <- "https://openrouter.ai/api/v1/chat/completions"
  body <- list(
    model       = model,
    messages    = list(list(role = "user", content = prompt)),
    temperature = temperature,
    max_tokens  = 100000,
    top_p       = 0.95,
    seed        = seed
  )

  for (attempt in 1:5) {
    Sys.sleep(5 * attempt)  # backoff: 5, 10, 15, 20, 25 s

    result <- tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_headers(
          "Authorization" = paste("Bearer", api_key),
          "HTTP-Referer"  = "http://localhost",
          "X-Title"       = "ECB-micro-pilot",
          "Content-Type"  = "application/json"
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(120) |>
        httr2::req_perform()

      if (resp$status_code != 200) {
        stop(paste0("HTTP ", resp$status_code, ": ",
                    httr2::resp_body_string(resp)))
      }

      httr2::resp_body_json(resp)$choices[[1]]$message$content

    }, error = function(e) {
      cat(crayon::yellow(paste0("  Attempt ", attempt, " failed: ", e$message, "\n")))
      NULL
    })

    if (!is.null(result)) return(result)
  }

  return(NULL)
}

# ==============================================================================
# 6. PER-CALL FUNCTION
# ==============================================================================

process_one <- function(date, version, run, originals, edited, prompt_template) {
  target <- sprintf("%s/%s_%s_%02d.rds", runs_dir, date, version, run)

  if (file.exists(target) && file.size(target) > 0) {
    cat(crayon::yellow(paste0(
      "  Skip: ", date, " [", version, "] run ", run, " (exists)\n"
    )))
    return(invisible(TRUE))
  }

  cat(crayon::yellow(paste0(
    "  Starting: ", date, " [", version, "] seed ", run, "\n"
  )))

  text <- if (version == "original") {
    originals[[as.character(date)]]
  } else {
    edited[[as.character(date)]]
  }

  full_prompt <- gsub("\\[date\\]", date, prompt_template)
  full_prompt <- paste0(
    full_prompt,
    "Press Conference on ", date, "\n",
    "Text: ", text, "\n\n"
  )

  response <- call_openrouter(full_prompt, seed = run)

  if (!is.null(response)) {
    saveRDS(response, target)
    cat(crayon::green(paste0(
      "  Saved: ", date, "_", version, "_", sprintf("%02d", run), ".rds\n"
    )))
    return(TRUE)
  } else {
    cat(
      file   = log_file,
      append = TRUE,
      paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " | date=", date, " | version=", version, " | run=", run,
        " | error=max retries exceeded\n"
      )
    )
    cat(crayon::red(paste0(
      "  FAILED: ", date, " [", version, "] run ", run, "\n"
    )))
    return(FALSE)
  }
}

# ==============================================================================
# 7. PARALLEL EXECUTION
# ==============================================================================

plan(multisession, workers = 6)

cat(crayon::blue(paste0(
  "Launching parallel run: ", nrow(grid),
  " calls across 6 workers\n\n"
)))

tic("API call phase")

future_pmap(
  list(date = grid$date, version = grid$version, run = grid$run),
  process_one,
  originals       = originals,
  edited          = edited,
  prompt_template = prompt_template,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

toc()  # API call phase

plan(sequential)

# ==============================================================================
# 8. COMPLETION CHECK
# ==============================================================================

completed_files <- list.files(runs_dir, pattern = "\\.rds$")
n_completed     <- length(completed_files)
n_missing       <- n_expected - n_completed

completed_grid <- tibble(stem = tools::file_path_sans_ext(completed_files)) %>%
  tidyr::extract(
    stem,
    into  = c("date", "version", "run"),
    regex = "^(\\d{4}-\\d{2}-\\d{2})_(original|edited)_(\\d+)$"
  ) %>%
  mutate(run = as.integer(run))

full_grid_check <- expand_grid(
  date    = as.character(the_dates),
  version = c("original", "edited"),
  run     = 1:10
)

missing_grid <- anti_join(full_grid_check, completed_grid,
                           by = c("date", "version", "run"))

cat("\n", strrep("=", 60), "\n")
cat(crayon::green(paste0("Completed:  ", n_completed, " / ", n_expected, " calls\n")))

if (nrow(missing_grid) > 0) {
  cat(crayon::red(paste0("Missing:    ", nrow(missing_grid), " calls\n")))
  saveRDS(
    missing_grid,
    "../intermediate_data/counterfactual_ensemble_p1/missing_grid.rds"
  )
  cat(crayon::red("Re-run the script — only missing calls will be retried.\n"))
  cat(strrep("=", 60), "\n\n")
  toc()
  stop("Incomplete run — see missing_grid.rds for details.")
} else {
  cat(crayon::green("All calls complete — proceeding to parsing.\n"))
}

cat(strrep("=", 60), "\n\n")

# ==============================================================================
# 9. PARSING — robust regex parser
# ==============================================================================

cat(crayon::blue("Parsing responses with robust regex parser...\n"))

parse_one_run <- function(filepath) {
  tryCatch({

    response <- readRDS(filepath)

    # skip empty files
    if (is.null(response) || nchar(trimws(response)) == 0) return(NULL)

    # --------------------------------------------------------------------------
    # Parse metadata from filename
    # Expected filename: YYYY-MM-DD_original_1.rds
    # --------------------------------------------------------------------------
    stem <- tools::file_path_sans_ext(basename(filepath))

    parts <- stringr::str_match(
      stem,
      "^(\\d{4}-\\d{2}-\\d{2})_(original|edited)_(\\d+)$"
    )

    if (is.na(parts[1, 1])) return(NULL)

    conf_date    <- parts[1, 2]
    conf_version <- parts[1, 3]
    run_id       <- as.integer(parts[1, 4])

    # --------------------------------------------------------------------------
    # Split response into lines
    # --------------------------------------------------------------------------
    lines <- unlist(strsplit(as.character(response), "\n"))
    lines <- trimws(lines)
    lines <- lines[nchar(lines) > 0]

    # --------------------------------------------------------------------------
    # Regex pattern for valid forecast rows
    #
    # Handles:
    # | 1998-11-03 | T001 | 3M | Down | 3.65 | 75 |
    # 1998-11-03 | T001 | 3M | Down | 3.65 | 75
    # --------------------------------------------------------------------------
    pattern <- paste0(
      "^\\|?\\s*",
      "(\\d{4}-\\d{2}-\\d{2})\\s*\\|\\s*",                         # date
      "(T\\d{3})\\s*\\|\\s*",                                      # agent id
      "(3M|2Y|10Y|3m|3mnt|3-month|10y|10yr|10-year)\\s*\\|\\s*",  # tenor
      "(Up|Down|Unchanged)\\s*\\|\\s*",                            # direction
      "([0-9]+\\.?[0-9]*)\\s*\\|\\s*",                             # rate
      "([0-9]+)\\s*\\|?$"                                          # confidence
    )

    matches <- stringr::str_match(lines, pattern)

    # keep only valid matches
    matches <- matches[!is.na(matches[, 1]), , drop = FALSE]

    if (nrow(matches) == 0) return(NULL)

    # --------------------------------------------------------------------------
    # Build parsed tibble
    # --------------------------------------------------------------------------
    result <- tibble::tibble(
      parsed_date = matches[, 2],
      agent_id    = matches[, 3],
      tenor       = matches[, 4],
      direction   = matches[, 5],
      rate        = as.numeric(matches[, 6]),
      confidence  = as.numeric(matches[, 7])
    ) %>%
      mutate(
        tenor = case_when(
          tenor %in% c("3m", "3mnt", "3-month") ~ "3M",
          tenor %in% c("10y", "10yr", "10-year") ~ "10Y",
          TRUE ~ tenor
        )
      ) %>%
      filter(
        parsed_date == conf_date,
        grepl("^T\\d{3}$", agent_id),
        tenor %in% c("3M", "2Y", "10Y"),
        direction %in% c("Up", "Down", "Unchanged")
      ) %>%
      mutate(
        date    = conf_date,
        version = conf_version,
        run     = run_id
      ) %>%
      select(
        date, version, run,
        agent_id, tenor, direction, rate, confidence
      ) %>%
      distinct()

    return(result)

  }, error = function(e) {
    message("Failed parsing: ", basename(filepath), " -- ", e$message)
    return(NULL)
  })
}

# ------------------------------------------------------------------------------
# Parse all files
# ------------------------------------------------------------------------------

run_files <- list.files(runs_dir, pattern = "\\.rds$", full.names = TRUE)

cf_runs_long <- purrr::map(run_files, parse_one_run) %>%
  purrr::compact() %>%
  dplyr::bind_rows()

# ------------------------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------------------------

cat(crayon::green(paste0(
  "Parsed: ", nrow(cf_runs_long), " rows from ", length(run_files), " files\n",
  "Dates: ", dplyr::n_distinct(cf_runs_long$date), "\n",
  "Versions: ", dplyr::n_distinct(cf_runs_long$version), "\n",
  "Runs: ", dplyr::n_distinct(cf_runs_long$run), "\n",
  "Agents: ", dplyr::n_distinct(cf_runs_long$agent_id), "\n",
  "Tenors: ", dplyr::n_distinct(cf_runs_long$tenor), "\n\n"
)))

# check counts against expectation
expected_per_group <- length(unique(the_dates)) * 30 * 10

counts_check <- cf_runs_long %>%
  count(version, tenor)

print(counts_check)

cat(crayon::yellow(
  paste0("Expected rows per version x tenor: ", expected_per_group, "\n\n")
))

# ------------------------------------------------------------------------------
# Save outputs
# ------------------------------------------------------------------------------

saveRDS(cf_runs_long, file.path(parsed_dir, "cf_runs_long.rds"))
writexl::write_xlsx(cf_runs_long, file.path(parsed_dir, "cf_runs_long.xlsx"))

cat(crayon::green("Saved: parsed/cf_runs_long (.rds + .xlsx)\n\n"))

# ==============================================================================
# 10. ENSEMBLE AGGREGATION (ROBUST VERSION)
# ==============================================================================

cat(crayon::blue("Computing ensemble aggregation...\n"))

# --------------------------------------------------------------------------
# OPTIONAL QUALITY FILTER (recommended)
# keep only reasonably complete cells
# --------------------------------------------------------------------------
min_agents_required <- 15

cf_runs_clean <- cf_runs_long %>%
  group_by(date, version, run, tenor) %>%
  filter(n() >= min_agents_required) %>%
  ungroup()

cat(crayon::yellow(paste0(
  "Filtered sparse cells: ",
  nrow(cf_runs_long) - nrow(cf_runs_clean),
  " rows removed\n"
)))

# --------------------------------------------------------------------------
# Within-run aggregation
# --------------------------------------------------------------------------
within_run <- cf_runs_clean %>%
  group_by(date, version, run, tenor) %>%
  summarise(
    within_sd = sd(rate, na.rm = TRUE),
    mean_rate = mean(rate, na.rm = TRUE),
    n_agents  = n(),
    .groups   = "drop"
  )

# --------------------------------------------------------------------------
# Ensemble aggregation across runs
# --------------------------------------------------------------------------
ensemble <- within_run %>%
  group_by(date, version, tenor) %>%
  summarise(
    sd_mean   = mean(within_sd, na.rm = TRUE),
    sd_se     = ifelse(
      n() > 1,
      sd(within_sd, na.rm = TRUE) / sqrt(n()),
      NA_real_
    ),
    mean_rate = mean(mean_rate, na.rm = TRUE),
    n_runs    = n(),
    .groups   = "drop"
  )

# --------------------------------------------------------------------------
# Treatment effect (FIXED pivot_wider)
# --------------------------------------------------------------------------
treatment_effect <- ensemble %>%
  select(date, tenor, version, sd_mean, sd_se) %>%
  pivot_wider(
    id_cols     = c(date, tenor),
    names_from  = version,
    values_from = c(sd_mean, sd_se),
    names_glue  = "{.value}_{version}"
  ) %>%
  mutate(
    delta_bps = sd_mean_original - sd_mean_edited,
    pct_reduction = ifelse(
      sd_mean_original > 0,
      100 * delta_bps / sd_mean_original,
      NA_real_
    ),
    delta_se = sqrt(sd_se_original^2 + sd_se_edited^2)
  )

# ==============================================================================
# SAVE OUTPUTS
# ==============================================================================

saveRDS(within_run, file.path(parsed_dir, "cf_within_run.rds"))
writexl::write_xlsx(within_run, file.path(parsed_dir, "cf_within_run.xlsx"))

saveRDS(ensemble, file.path(parsed_dir, "cf_ensemble.rds"))
writexl::write_xlsx(ensemble, file.path(parsed_dir, "cf_ensemble.xlsx"))

saveRDS(treatment_effect, file.path(parsed_dir, "cf_treatment_effect.rds"))
writexl::write_xlsx(treatment_effect, file.path(parsed_dir, "cf_treatment_effect.xlsx"))

cat(crayon::green(paste0(
  "Saved: within_run (", nrow(within_run), " rows), ",
  "ensemble (", nrow(ensemble), " rows), ",
  "treatment_effect (", nrow(treatment_effect), " rows)\n\n"
)))

# ==============================================================================
# 11. SANITY CHECKS
# ==============================================================================

cat(strrep("-", 60), "\n")
cat("SANITY CHECKS\n")
cat(strrep("-", 60), "\n\n")

# agent completeness check
sparse <- within_run %>%
  filter(n_agents < min_agents_required)

cat(paste0(
  "Agent count check: ",
  nrow(sparse), " sparse (",
  "<", min_agents_required, " agents)\n"
))

if (nrow(sparse) > 0) {
  cat(crayon::red("Sample sparse cells:\n"))
  print(head(sparse, 10))
}

# rate ranges
cat("\nRate value ranges by tenor:\n")
cf_runs_clean %>%
  group_by(tenor) %>%
  summarise(
    min  = min(rate, na.rm = TRUE),
    mean = mean(rate, na.rm = TRUE),
    max  = max(rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# completeness of runs
n_complete <- sum(ensemble$n_runs == max(ensemble$n_runs, na.rm = TRUE))

cat(paste0(
  "\nComplete (date, version, tenor) combos: ",
  n_complete, " / ", nrow(ensemble), "\n\n"
))

# treatment sanity
cat("Mean pct_reduction by tenor:\n")
treatment_effect %>%
  group_by(tenor) %>%
  summarise(
    mean_pct_reduction = mean(pct_reduction, na.rm = TRUE),
    mean_delta_bps     = mean(delta_bps, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  print()

cat("\nHead of treatment_effect:\n")
treatment_effect %>%
  arrange(date) %>%
  head(12) %>%
  print()

# ==============================================================================
# 12. FINAL SUMMARY
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("FINAL SUMMARY\n")
cat(strrep("=", 60), "\n\n")

within_run %>%
  group_by(version, tenor) %>%
  summarise(mean_sd = mean(within_sd, na.rm = TRUE), .groups = "drop") %>%
  print()

cat("\nHead of ensemble:\n")
ensemble %>%
  arrange(date, version, tenor) %>%
  head(12) %>%
  print()

toc()

cat("\n", strrep("=", 80), "\n")
cat(crayon::green("COUNTERFACTUAL ENSEMBLE P1 COMPLETE (FIXED)\n"))
cat(strrep("=", 80), "\n")