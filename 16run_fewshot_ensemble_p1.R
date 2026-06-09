#===============================================================================
# FEW-SHOT ENSEMBLE P1 — 90 ECB Conferences × R=10 Seeds
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Few-shot ensemble: 90 stratified ECB conferences x 10 seeds = 900 API calls.
#   Uses prompt_history_surprises — injects before/after OIS SD from the
#   previous 3 conferences as context for each press conference.
#   Conferences selected via stratified random sample: 30 per tercile of
#   realized post-conference OIS volatility (2Y tenor as ranking variable).
#
# Design mirrors run_full_ensemble_p1.R (structure, API caller, parsing),
# and run_endogeneity_p1.R (tercile stratification pattern).
#
# Outputs:
#   ../intermediate_data/fewshot_ensemble_p1/selected_dates.rds
#   ../intermediate_data/fewshot_ensemble_p1/runs/{date}_{run}.rds
#   ../intermediate_data/fewshot_ensemble_p1/failed_calls.log
#   ../intermediate_data/fewshot_ensemble_p1/missing_grid.rds  (if incomplete)
#   ../intermediate_data/fewshot_ensemble_p1/parsed/all_runs_long.rds / .xlsx
#   ../intermediate_data/fewshot_ensemble_p1/parsed/within_run_sd.rds / .xlsx
#   ../intermediate_data/fewshot_ensemble_p1/parsed/ensemble_sd.rds   / .xlsx
#
# Usage:
#   Rscript code/run_fewshot_ensemble_p1.R
#   source("run_fewshot_ensemble_p1.R")   # from code/ directory
#   Re-run at any time — completed calls are skipped automatically.
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("FEW-SHOT ENSEMBLE P1 — 90 ECB Conferences x R=10 Seeds\n")
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
prompt_template     <- prompt_history_surprises
name_prompt_request <- "fewshot_ensemble_R10"

cat(crayon::green(paste0("Loaded prompt: prompt_history_surprises  [", name_prompt_request, "]\n\n")))

base_dir   <- "../intermediate_data/fewshot_ensemble_p1"
runs_dir   <- file.path(base_dir, "runs")
parsed_dir <- file.path(base_dir, "parsed")
log_file   <- file.path(base_dir, "failed_calls.log")

dir.create(runs_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(parsed_dir, recursive = TRUE, showWarnings = FALSE)

tic("Total script")

# ==============================================================================
# 2. CONFERENCE SELECTION — stratified random draw, 30 per vol tercile (2Y)
# ==============================================================================

cat(crayon::blue("Loading range_difference_df.rds for conference selection...\n"))

range_df <- readRDS("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = if_else(tenor == "3mnt", "3M", tenor))

# Rank conferences by 2Y post-conference OIS volatility (1-day window)
vol_by_date <- range_df %>%
  filter(tenor == "2Y", !is.na(correct_post_mean_1)) %>%
  select(date, correct_post_mean_1) %>%
  mutate(date = as.character(as.Date(date)))

cat(crayon::green(paste0(
  "  Found 2Y vol scores for ", nrow(vol_by_date), " conference dates.\n\n"
)))

set.seed(20260512)
selected_dates_df <- vol_by_date %>%
  mutate(tercile = ntile(correct_post_mean_1, 3)) %>%
  group_by(tercile) %>%
  slice_sample(n = 30) %>%
  ungroup() %>%
  arrange(date)

saveRDS(selected_dates_df, file.path(base_dir, "selected_dates.rds"))

cat(strrep("-", 60), "\n")
cat("90 SELECTED CONFERENCES (stratified random, 30 per 2Y-vol tercile):\n")
cat(strrep("-", 60), "\n")
print(selected_dates_df, n = 90)
cat(strrep("-", 60), "\n\n")
cat(sprintf("  Tercile 1 (low vol):    %d conferences\n",  sum(selected_dates_df$tercile == 1)))
cat(sprintf("  Tercile 2 (mid vol):    %d conferences\n",  sum(selected_dates_df$tercile == 2)))
cat(sprintf("  Tercile 3 (high vol):   %d conferences\n\n", sum(selected_dates_df$tercile == 3)))

cat(crayon::cyan("▶ Awaiting confirmation of 90 selected conferences before proceeding.\n"))
cat(crayon::cyan("  Type 'yes' and press Enter to continue, or anything else to abort:\n"))
ans_1 <- readline()
if (tolower(trimws(ans_1)) != "yes") {
  toc()
  stop("User did not confirm conference selection. Aborting.", call. = FALSE)
}
cat(crayon::green("  Conference selection confirmed.\n\n"))

the_dates <- selected_dates_df$date

# ==============================================================================
# 3. LOAD TRANSCRIPTS
# ==============================================================================

cat(crayon::blue("Loading ECB press conference transcripts...\n"))

texts_dir  <- "../intermediate_data/texts"
text_files <- list.files(texts_dir, pattern = "\\.txt$", full.names = TRUE)
file_dates <- str_extract(basename(text_files), "\\d{4}-\\d{2}-\\d{2}")

valid    <- !is.na(file_dates)
text_map <- set_names(text_files[valid], file_dates[valid])

missing_transcripts <- setdiff(the_dates, names(text_map))
if (length(missing_transcripts) > 0) {
  cat(crayon::red(paste0(
    "WARNING: ", length(missing_transcripts),
    " selected dates have no transcript file:\n"
  )))
  cat(paste(missing_transcripts, collapse = "\n"), "\n\n")
  the_dates <- intersect(the_dates, names(text_map))
  cat(crayon::yellow(paste0("Proceeding with ", length(the_dates), " dates.\n\n")))
}

transcripts_tbl <- tibble(date = the_dates) %>%
  mutate(text = map_chr(date, ~ readtext::readtext(text_map[[.x]])$text))

cat(crayon::green(paste0("Loaded ", nrow(transcripts_tbl), " transcripts.\n\n")))

# ==============================================================================
# 4. HISTORY CONTEXT BUILDER
# ==============================================================================
# Mirrors process_conference_with_history() in src/llm_api/gemini_api.R
# but returns a plain string for use with OpenRouter parallel workers.

build_history_context <- function(conf_date, range_df, history_window = 3) {
  std_info <- range_df %>%
    filter(tenor %in% c("3M", "2Y", "10Y"),
           as.Date(date) < as.Date(conf_date),
           !is.na(correct_pre_mean_3), !is.na(correct_post_mean_1)) %>%
    group_by(tenor) %>%
    arrange(desc(as.Date(date))) %>%
    slice_head(n = history_window) %>%
    summarise(
      historical_std = paste(
        paste0("Date: ", date,
               ", Before: ", round(correct_pre_mean_3, 4),
               ", After: ",  round(correct_post_mean_1, 4)),
        collapse = "; "
      ),
      .groups = "drop"
    )

  paste0(
    "\n\nHistorical Context (Previous ", history_window, " Conferences):\n",
    paste(paste0("- ", std_info$tenor, ": ", std_info$historical_std),
          collapse = "\n"),
    "\n\n"
  )
}

cat(crayon::blue("Pre-computing history contexts for all 90 selected dates...\n"))

history_tbl <- tibble(date = the_dates) %>%
  mutate(history_context = map_chr(
    date, build_history_context,
    range_df = range_df
  ))

cat(crayon::green("History contexts ready.\n\n"))

# ==============================================================================
# 5. BUILD GRID
# ==============================================================================

# 900-row grid shuffled so partial failures spread evenly across dates
grid <- expand_grid(date = the_dates, run = 1:10) %>%
  left_join(transcripts_tbl, by = "date") %>%
  left_join(history_tbl,     by = "date") %>%
  select(date, run, text, history_context) %>%
  sample_n(n())

cat(crayon::green(paste0(
  "Grid ready: ", nrow(grid), " rows (",
  n_distinct(grid$date), " dates x 10 runs)\n\n"
)))

# ==============================================================================
# 6. OPENROUTER CALLER  — verbatim from run_full_ensemble_p1.R
# ==============================================================================

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
          "X-Title"       = "ECB-fewshot-ensemble",
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
# 7. PER-CALL FUNCTION
# ==============================================================================

process_one <- function(date, run, text, history_context) {
  target <- sprintf("%s/%s_%02d.rds", runs_dir, date, run)

  if (file.exists(target) && file.size(target) > 0) {
    cat(crayon::yellow(paste0("  Skip: ", date, " run ", run, " (exists)\n")))
    return(invisible(TRUE))
  }

  cat(crayon::yellow(paste0("  Starting: ", date, " seed ", run, "\n")))

  full_prompt <- gsub("\\[date\\]", date, prompt_template)
  full_prompt <- paste0(
    full_prompt,
    history_context,
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
# 8. PARALLEL EXECUTION
# ==============================================================================

plan(multisession, workers = 5)

cat(crayon::blue(paste0(
  "Launching parallel run: ", nrow(grid),
  " calls across 5 workers\n\n"
)))

tic("API call phase")

future_pmap(
  list(date            = grid$date,
       run             = grid$run,
       text            = grid$text,
       history_context = grid$history_context),
  process_one,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

toc()  # API call phase

plan(sequential)

# ==============================================================================
# 9. COMPLETION CHECK
# ==============================================================================

completed_files <- list.files(runs_dir, pattern = "\\.rds$")
n_completed     <- length(completed_files)
n_expected      <- length(the_dates) * 10
n_missing       <- n_expected - n_completed

completed_grid <- tibble(stem = tools::file_path_sans_ext(completed_files)) %>%
  tidyr::extract(
    stem,
    into  = c("date", "run"),
    regex = "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)$"
  ) %>%
  mutate(run = as.integer(run))

full_grid <- expand_grid(
  date = as.character(the_dates),
  run  = 1:10
)

missing_grid <- anti_join(full_grid, completed_grid, by = c("date", "run"))

cat("\n", strrep("=", 60), "\n")
cat(crayon::green(paste0("Completed:  ", n_completed, " / ", n_expected, " calls\n")))
if (n_missing > 0) {
  cat(crayon::red(paste0("Missing:    ", n_missing, " calls\n")))
  saveRDS(missing_grid, file.path(base_dir, "missing_grid.rds"))
  cat(crayon::red("Re-run the script — only missing calls will be retried.\n"))
} else {
  cat(crayon::green("All calls complete — proceeding to parsing.\n"))
}
cat(strrep("=", 60), "\n\n")

# ==============================================================================
# 10–12. PARSING, AGGREGATION, SANITY CHECKS  (only when all calls complete)
# ==============================================================================

if (n_missing == 0) {

  # 10. PARSING ----------------------------------------------------------------
  # Reuses the exact read_delim logic from 09run_full_ensemble_p1.R

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
          delim          = "|",
          trim_ws        = TRUE,
          skip           = 1,
          show_col_types = FALSE,
          name_repair    = "minimal"
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

  # 11. ENSEMBLE AGGREGATION ---------------------------------------------------

  cat(crayon::blue("Computing ensemble aggregation...\n"))

  # Within-run SD: SD of rate across 30 agents per (date, run, tenor)
  # Expected: 90 x 10 x 3 = 2,700 rows
  within_run_sd <- all_runs_long %>%
    group_by(date, run, tenor) %>%
    summarise(sd = sd(rate, na.rm = TRUE), .groups = "drop")

  # Ensemble SD: mean and SE across 10 runs per (date, tenor)
  # Expected: 90 x 3 = 270 rows
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

  # 12. SANITY CHECKS ----------------------------------------------------------

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

  # Rate ranges by tenor
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

  # Dates with all 10 runs parsed
  n_dates_all10 <- ensemble_sd %>%
    filter(n_runs == 10) %>%
    pull(date) %>%
    n_distinct()

  cat(paste0(
    "\nDates with all 10 runs parsed: ", n_dates_all10,
    " / ", n_distinct(all_runs_long$date), "\n\n"
  ))

  # Ensemble coverage check
  cat("Ensemble SD by tercile (mean sd_mean across dates):\n")
  ensemble_sd %>%
    left_join(selected_dates_df %>% select(date, tercile), by = "date") %>%
    group_by(tercile, tenor) %>%
    summarise(mean_sd_mean = mean(sd_mean, na.rm = TRUE), .groups = "drop") %>%
    arrange(tenor, tercile) %>%
    print()

  # 13. FINAL LOGGING ----------------------------------------------------------

  cat(strrep("=", 60), "\n")
  cat("FINAL SUMMARY\n")
  cat(strrep("=", 60), "\n\n")

  cat("Mean within-run SD by tenor:\n")
  within_run_sd %>%
    group_by(tenor) %>%
    summarise(mean_sd = mean(sd, na.rm = TRUE), .groups = "drop") %>%
    print()

  cat("\nHead of ensemble_sd (primary output):\n")
  print(head(ensemble_sd, 12))
  cat("\n")

}

toc()  # total script time

cat("\n", strrep("=", 80), "\n")
cat(crayon::green("FEW-SHOT ENSEMBLE P1 COMPLETE\n"))
cat(strrep("=", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
