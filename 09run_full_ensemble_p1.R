#===============================================================================
# FULL ENSEMBLE P1 — 283 ECB Conferences × R=10 Seeds
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Full ensemble simulation: 283 ECB conferences x 10 seeds = 2,830 API calls.
#   Parallelised with furrr (5 workers). Per-call RDS caching for resumability.
#
# Outputs:
#   ../intermediate_data/full_ensemble_p1/runs/{date}_{run}.rds
#   ../intermediate_data/full_ensemble_p1/failed_calls.log
#   ../intermediate_data/full_ensemble_p1/missing_grid.rds     (if incomplete)
#   ../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds / .xlsx
#   ../intermediate_data/full_ensemble_p1/parsed/within_run_sd.rds / .xlsx
#   ../intermediate_data/full_ensemble_p1/parsed/ensemble_sd.rds   / .xlsx
#
# Usage:
#   Rscript code/run_full_ensemble_p1.R
#   source("09run_full_ensemble_p1.R")   # from code/ directory
#   Re-run at any time — completed calls are skipped automatically.
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("FULL ENSEMBLE P1 — 283 ECB Conferences x R=10 Seeds\n")
cat(strrep("=", 80), "\n\n")

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
name_prompt_request <- "naive_full_ensemble_R10"

cat(crayon::green(paste0("Loaded prompt: prompt_naive  [", name_prompt_request, "]\n\n")))

runs_dir   <- "../intermediate_data/full_ensemble_p1/runs"
parsed_dir <- "../intermediate_data/full_ensemble_p1/parsed"
log_file   <- "../intermediate_data/full_ensemble_p1/failed_calls.log"

dir.create(runs_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(parsed_dir, recursive = TRUE, showWarnings = FALSE)

tic("Total script")

# ==============================================================================
# 2. DATA LOAD
# ==============================================================================

cat(crayon::blue("Loading ECB press conference transcripts...\n"))

texts_dir  <- "../intermediate_data/texts"
text_files <- list.files(texts_dir, pattern = "\\.txt$", full.names = TRUE)
file_dates <- str_extract(basename(text_files), "\\d{4}-\\d{2}-\\d{2}")

valid    <- !is.na(file_dates)
text_map <- set_names(text_files[valid], file_dates[valid])

cat(crayon::green(paste0("Found ", length(text_map), " transcripts.\n\n")))

# Pre-load all texts into memory so workers receive them as arguments
# (avoids each worker hitting disk independently)
transcripts_tbl <- tibble(
  date = names(text_map),
  file = unname(text_map)
) %>%
  mutate(text = map_chr(file, ~ readtext::readtext(.x)$text)) %>%
  select(date, text)

# 2,830-row grid shuffled so partial failures spread evenly across dates
grid <- expand_grid(date = transcripts_tbl$date, run = 1:10) %>%
  left_join(transcripts_tbl, by = "date") %>%
  select(date, run, text) %>%
  sample_n(n())

cat(crayon::green(paste0(
  "Grid ready: ", nrow(grid), " rows (",
  n_distinct(grid$date), " dates x 10 runs)\n\n"
)))

# ==============================================================================
# 3. OPENROUTER CALLER
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
# 4. PER-CALL FUNCTION
# ==============================================================================

process_one <- function(date, run, text) {
  target <- sprintf("%s/%s_%02d.rds", runs_dir, date, run)

  # Resumability: skip if file already saved and non-empty
  if (file.exists(target) && file.size(target) > 0) {
    cat(crayon::yellow(paste0("  Skip: ", date, " run ", run, " (exists)\n")))
    return(invisible(TRUE))
  }

  cat(crayon::yellow(paste0("  Starting: ", date, " seed ", run, "\n")))

  full_prompt <- gsub("\\[date\\]", date, prompt_template)
  full_prompt <- paste0(
    full_prompt,
    "Press Conference on ", date, "\n",
    "Text: ", text, "\n\n"
  )

  response <- call_openrouter(full_prompt, seed = run)

  if (!is.null(response)) {
    saveRDS(response, target)
    cat(crayon::green(paste0("  Saved: ", date, "_", sprintf("%02d", run), ".rds\n")))
    return(TRUE)
  } else {
    cat(
      file   = log_file,
      append = TRUE,
      paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " | date=", date, " | run=", run,
        " | error=max retries exceeded\n"
      )
    )
    cat(crayon::red(paste0("  FAILED: ", date, " run ", run, "\n")))
    return(FALSE)
  }
}

# ==============================================================================
# 5. PARALLEL EXECUTION
# ==============================================================================

on.exit(plan(sequential), add = TRUE)
plan(multisession, workers = 5)  # tune to machine; I/O-bound so >nCPUs is fine

cat(crayon::blue(paste0(
  "Launching parallel run: ", nrow(grid),
  " calls across 5 workers\n\n"
)))

tic("API call phase")

future_pmap(
  list(date = grid$date, run = grid$run, text = grid$text),
  process_one,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

toc()  # API call phase

plan(sequential)

# ==============================================================================
# 6. COMPLETION CHECK
# ==============================================================================

completed_files <- list.files(runs_dir, pattern = "\\.rds$")
n_completed     <- length(completed_files)
n_expected      <- nrow(transcripts_tbl) * 10
n_missing       <- n_expected - n_completed

completed_grid <- tibble(stem = tools::file_path_sans_ext(completed_files)) %>%
  tidyr::extract(
    stem,
    into  = c("date", "run"),
    regex = "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)$"
  ) %>%
  mutate(run = as.integer(run))

full_grid <- expand_grid(
  date = as.character(transcripts_tbl$date),
  run  = 1:10
)

missing_grid <- anti_join(full_grid, completed_grid, by = c("date", "run"))

cat("\n", strrep("=", 60), "\n")
cat(crayon::green(paste0("Completed:  ", n_completed, " / ", n_expected, " calls\n")))
if (n_missing > 0) {
  cat(crayon::red(paste0("Missing:    ", n_missing, " calls\n")))
  saveRDS(missing_grid, "../intermediate_data/full_ensemble_p1/missing_grid.rds")
  cat(crayon::red("Re-run the script — only missing calls will be retried.\n"))
} else {
  cat(crayon::green("All calls complete — proceeding to parsing.\n"))
}
cat(strrep("=", 60), "\n\n")

# ==============================================================================
# 7–9. PARSING, AGGREGATION, SANITY CHECKS  (only when all calls are complete)
# ==============================================================================

if (n_missing <= 0) {

  # 7. PARSING -----------------------------------------------------------------

  cat(crayon::blue("Parsing responses...\n"))

  names_col <- c("date", "id", "tenor", "direction", "rate", "confidence")

  parse_one_run <- function(filepath) {
    tryCatch({
      response <- readRDS(filepath)
      if (is.null(response) || nchar(trimws(response)) == 0) return(NULL)

      stem  <- tools::file_path_sans_ext(basename(filepath))
      parts <- str_match(stem, "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)$")
      if (is.na(parts[1, 1])) return(NULL)
      conf_date <- parts[1, 2]
      run_id    <- as.integer(parts[1, 3])

      result <- response %>%
        readr::read_delim(
          delim         = "|",
          trim_ws       = TRUE,
          skip          = 1,
          show_col_types = FALSE,
          name_repair   = "minimal"
        ) %>%
        select(-1, -ncol(.)) %>%
        slice(-nrow(.)) %>%
        setNames(names_col) %>%
        slice(-1) %>%
        mutate(date = as.character(date)) %>%
        mutate(across(contains("rate"),       as.numeric)) %>%
        mutate(across(contains("confidence"), as.numeric)) %>%
        filter(tenor %in% c("3M", "2Y", "10Y"))

      result %>%
        mutate(date = conf_date, run = run_id, agent_id = id) %>%
        select(date, run, agent_id, tenor, direction, rate, confidence)

    }, error = function(e) NULL)
  }

  run_files <- list.files(runs_dir, pattern = "\\.rds$", full.names = TRUE)

  all_runs_long <- map(run_files, parse_one_run) %>%
    keep(~ !is.null(.x) && nrow(.x) > 0) %>%
    bind_rows()

  cat(crayon::green(paste0(
    "Parsed: ", nrow(all_runs_long), " rows from ", length(run_files), " files",
    " (", n_distinct(all_runs_long$date), " dates, ",
    n_distinct(all_runs_long$run), " runs, ",
    n_distinct(all_runs_long$tenor), " tenors)\n\n"
  )))

  saveRDS(all_runs_long, file.path(parsed_dir, "all_runs_long.rds"))
  writexl::write_xlsx(all_runs_long, file.path(parsed_dir, "all_runs_long.xlsx"))
  cat(crayon::green("Saved: parsed/all_runs_long (.rds + .xlsx)\n\n"))

  # 8. ENSEMBLE AGGREGATION ----------------------------------------------------

  cat(crayon::blue("Computing ensemble aggregation...\n"))

  # Within-run SD: SD of rate across 30 agents per (date, run, tenor)
  # Expected: 283 x 10 x 3 = 8,490 rows
  within_run_sd <- all_runs_long %>%
    group_by(date, run, tenor) %>%
    summarise(sd = sd(rate, na.rm = TRUE), .groups = "drop")

  # Ensemble SD: mean and SE of within-run SDs across 10 runs per (date, tenor)
  # Expected: 283 x 3 = 849 rows
  ensemble_sd <- within_run_sd %>%
    group_by(date, tenor) %>%
    summarise(
      sd_mean = mean(sd, na.rm = TRUE),
      sd_se   = sd(sd,   na.rm = TRUE) / sqrt(n()),
      n_runs  = n(),
      .groups = "drop"
    )

  saveRDS(within_run_sd, file.path(parsed_dir, "within_run_sd.rds"))
  writexl::write_xlsx(within_run_sd, file.path(parsed_dir, "within_run_sd.xlsx"))

  saveRDS(ensemble_sd, file.path(parsed_dir, "ensemble_sd.rds"))
  writexl::write_xlsx(ensemble_sd, file.path(parsed_dir, "ensemble_sd.xlsx"))

  cat(crayon::green(paste0(
    "Saved: within_run_sd (", nrow(within_run_sd), " rows)",
    " and ensemble_sd (", nrow(ensemble_sd), " rows) (.rds + .xlsx each)\n\n"
  )))

  # 9. SANITY CHECKS -----------------------------------------------------------

  cat(strrep("-", 60), "\n")
  cat("SANITY CHECKS\n")
  cat(strrep("-", 60), "\n\n")

  # Agents per (date, run, tenor): expect 30; flag any < 25
  agent_counts <- all_runs_long %>%
    group_by(date, run, tenor) %>%
    summarise(n_agents = n(), .groups = "drop")

  sparse <- filter(agent_counts, n_agents < 25)
  cat(paste0(
    "Agent count (expect 30 per cell): ",
    nrow(sparse), " combos with < 25 agents",
    if (nrow(sparse) == 0) " [OK]\n" else "\n"
  ))
  if (nrow(sparse) > 0) {
    cat(crayon::red("  WARNING — sparse responses (first 10 shown):\n"))
    print(head(sparse, 10))
  }

  # Rate ranges by tenor: sanity-check for plausible % values
  cat("\nRate value ranges by tenor (%):\n")
  all_runs_long %>%
    group_by(tenor) %>%
    summarise(
      min  = min(rate,  na.rm = TRUE),
      mean = mean(rate, na.rm = TRUE),
      max  = max(rate,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print()

  # Dates with all 10 runs successfully parsed
  n_dates_all10 <- ensemble_sd %>%
    filter(n_runs == 10) %>%
    pull(date) %>%
    n_distinct()

  cat(paste0(
    "\nDates with all 10 runs parsed: ", n_dates_all10,
    " / ", n_distinct(all_runs_long$date), "\n\n"
  ))

  # 10. FINAL LOGGING ----------------------------------------------------------

  cat(strrep("=", 60), "\n")
  cat("FINAL SUMMARY\n")
  cat(strrep("=", 60), "\n\n")

  cat("Mean within-run SD by tenor (sanity vs. paper figures):\n")
  within_run_sd %>%
    group_by(tenor) %>%
    summarise(mean_sd = mean(sd, na.rm = TRUE), .groups = "drop") %>%
    print()

  cat("\nHead of ensemble_sd:\n")
  print(head(ensemble_sd, 12))
  cat("\n")

}

toc()  # total script time

cat("\n", strrep("=", 80), "\n")
cat(crayon::green("FULL ENSEMBLE P1 COMPLETE\n"))
cat(strrep("=", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
