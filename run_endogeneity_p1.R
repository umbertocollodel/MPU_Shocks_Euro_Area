#===============================================================================
# ENDOGENEITY TEST P1 — Two-Stage Panel Separation
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Tests whether the agent-panel composition is endogenous to transcript
#   content (within-call confounding) by separating panel generation from
#   forecasting into two stages:
#     Stage 1: generate 30 agent panels conditioning ONLY on conference date
#              and macro regime (no transcript). One panel per (conference, run)
#              so that panel-composition uncertainty is averaged out alongside
#              forecast uncertainty across the 10 runs.
#     Stage 2: each frozen (date, run) panel forecasts given the transcript.
#   Resulting disagreement is compared against the headline zero-shot ensemble
#   on the same 30 conferences using Spearman correlations with bootstrapped CIs.
#
#   Sample: 30 conferences, stratified random draw across vol terciles.
#   Stage 1 calls: 30 × 10 = 300 (parallel, 5 workers)
#   Stage 2 calls: 30 × 10 = 300 (parallel, 5 workers)
#   Grand total:   600 API calls
#
# Outputs (ALL under intermediate_data/endogeneity_p1/):
#   selected_dates.rds
#   panels/{date}_{run:02d}.rds               — Stage 1 panel responses
#   runs/{date}_{run:02d}.rds                 — Stage 2 forecast responses
#   failed_panels.log
#   failed_calls.log
#   missing_grid.rds                          (only if Stage 2 incomplete)
#   parsed/endo_runs_long.{rds,xlsx}
#   parsed/endo_within_run.{rds,xlsx}
#   parsed/endo_ensemble.{rds,xlsx}
#   parsed/endo_vs_headline_comparison.{rds,xlsx}
#
# Usage:
#   source("run_endogeneity_p1.R")   # from code/ — run interactively
#   Re-run at any time — completed Stage 1 panels and Stage 2 runs are skipped.
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("ENDOGENEITY TEST P1 — Two-Stage Panel Separation\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# HARD ASSERTION
# ==============================================================================

output_root <- normalizePath(
  "../intermediate_data/endogeneity_p1",
  mustWork = FALSE
)
if (!grepl("endogeneity_p1", output_root, fixed = TRUE)) {
  stop(
    "HARD ASSERT: output root does not contain 'endogeneity_p1'. ",
    "Refusing to write outputs elsewhere.\nGot: ", output_root
  )
}

# ==============================================================================
# 1. SETUP
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  httr2, crayon, stringr, purrr, readr, readtext, readxl,
  writexl, tidyverse, future, furrr, tictoc, boot
)

api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (nchar(api_key) == 0) stop("OPENROUTER_API_KEY not set. Add it to .Renviron.")

source("config/prompts.R")
cat(crayon::green("Loaded config/prompts.R (prompt_naive schema used for Stage 2 output)\n\n"))

# ------------------------------------------------------------------------------
# SET THIS TO TRUE to delete all existing panels, runs, and parsed outputs
# before re-running (e.g. after a prompt change). Set to FALSE to resume an
# interrupted run without repeating completed calls.
force_rerun <- FALSE
# ------------------------------------------------------------------------------

panels_dir <- "../intermediate_data/endogeneity_p1/panels"
runs_dir   <- "../intermediate_data/endogeneity_p1/runs"
parsed_dir <- "../intermediate_data/endogeneity_p1/parsed"
log_panels <- "../intermediate_data/endogeneity_p1/failed_panels.log"
log_calls  <- "../intermediate_data/endogeneity_p1/failed_calls.log"

dir.create(panels_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(runs_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(parsed_dir, recursive = TRUE, showWarnings = FALSE)

if (force_rerun) {
  to_delete <- c(
    list.files(panels_dir, pattern = "\\.rds$", full.names = TRUE),
    list.files(runs_dir,   pattern = "\\.rds$", full.names = TRUE),
    list.files(parsed_dir, pattern = "\\.(rds|xlsx)$", full.names = TRUE),
    log_panels, log_calls
  )
  to_delete <- to_delete[file.exists(to_delete)]
  n_del <- length(to_delete)
  if (n_del > 0) {
    cat(crayon::red(paste0(
      "\nforce_rerun = TRUE: about to delete ", n_del,
      " existing file(s) across panels/, runs/, and parsed/.\n"
    )))
    cat(crayon::cyan("  Type 'yes' to confirm deletion, or anything else to abort:\n"))
    ans_del <- readline()
    if (tolower(trimws(ans_del)) != "yes") {
      stop("Deletion aborted by user. Set force_rerun <- FALSE to resume.", call. = FALSE)
    }
    file.remove(to_delete)
    cat(crayon::green(paste0("  Deleted ", n_del, " file(s). Starting clean.\n\n")))
  } else {
    cat(crayon::yellow("force_rerun = TRUE but no existing files found — nothing to delete.\n\n"))
  }
}

tic("Total script")

# ==============================================================================
# 2. CONFERENCE SELECTION — stratified random draw, 10 per vol tercile
# ==============================================================================

cat(crayon::blue("Loading range_difference_df.rds for conference selection...\n"))

range_df <- readRDS("../intermediate_data/range_difference_df.rds")

vol_by_date <- range_df %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  filter(tenor %in% c("3M", "2Y", "10Y"),
         !is.na(correct_post_mean_1)) %>%
  group_by(date) %>%
  filter(n_distinct(tenor) == 3L) %>%
  summarise(vol_score = mean(correct_post_mean_1, na.rm = TRUE),
            .groups   = "drop") %>%
  filter(!is.na(vol_score))

cat(crayon::green(paste0(
  "  Found vol scores for ", nrow(vol_by_date), " conference dates.\n\n"
)))

set.seed(20260507)
selected_dates_df <- vol_by_date %>%
  mutate(tercile = ntile(vol_score, 3)) %>%
  group_by(tercile) %>%
  slice_sample(n = 10) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(date = as.character(date))

saveRDS(selected_dates_df, "../intermediate_data/endogeneity_p1/selected_dates.rds")

cat(strrep("-", 60), "\n")
cat("30 SELECTED CONFERENCES (stratified random, 10 per vol tercile):\n")
cat(strrep("-", 60), "\n")
print(selected_dates_df, n = 30)
cat(strrep("-", 60), "\n\n")

cat(crayon::cyan("▶ Awaiting confirmation of 30 selected conferences before proceeding.\n"))
cat(crayon::cyan("  Type 'yes' and press Enter to continue, or anything else to abort:\n"))
ans_1 <- readline()
if (tolower(trimws(ans_1)) != "yes") {
  toc()
  stop("User did not confirm conference selection. Aborting.", call. = FALSE)
}
cat(crayon::green("  Conference selection confirmed.\n\n"))

# ==============================================================================
# 3. LOAD TRANSCRIPTS — mirror run_clarity_ensemble_p1.R loader
# ==============================================================================

cat(crayon::blue("Loading original transcripts from ../intermediate_data/texts/...\n"))

texts_dir  <- "../intermediate_data/texts"
text_files <- list.files(texts_dir, pattern = "\\.txt$",
                          full.names = TRUE, recursive = FALSE)
file_dates <- str_extract(basename(text_files), "\\d{4}-\\d{2}-\\d{2}")
valid      <- !is.na(file_dates)
text_map   <- set_names(text_files[valid], file_dates[valid])

the_dates <- selected_dates_df$date

transcripts_map <- map_chr(the_dates, function(d) {
  if (d %in% names(text_map)) {
    tryCatch(
      readtext::readtext(text_map[[d]])$text,
      error = function(e) NA_character_
    )
  } else {
    NA_character_
  }
})
transcripts_map <- set_names(transcripts_map, the_dates)

missing_tx <- the_dates[is.na(transcripts_map) | nchar(trimws(transcripts_map)) == 0]
if (length(missing_tx) > 0) {
  cat(crayon::red(paste0(
    "HARD ASSERT FAILED: transcripts missing or empty for ",
    length(missing_tx), " date(s): ",
    paste(missing_tx, collapse = ", "), "\n"
  )))
  toc()
  stop("All 30 transcripts must be present and non-empty.", call. = FALSE)
}

stopifnot(
  "transcripts_map length mismatch" = length(transcripts_map) == 30L,
  "remaining NA in transcripts_map" = !any(is.na(transcripts_map))
)

cat(crayon::green(paste0(
  "  All 30 transcripts loaded. Sample preview (", the_dates[1], "):\n"
)))
cat(substr(transcripts_map[[the_dates[1]]], 1, 200), "\n\n")

# ==============================================================================
# 4. STAGE 1 PROMPT — date-only panel generation (character literal; NOT in prompts.R)
# ==============================================================================

panel_only_prompt <- "
You are constructing a synthetic panel of 30 market participants who would have
been active in euro-area fixed-income and interest rate swap markets around [date].

CRITICAL CONSTRAINT: Generate this panel based ONLY on the macroeconomic regime
prevailing around [date] — the interest rate environment, the recent trajectory
of ECB monetary policy, and broad market conditions as of [date]. DO NOT
condition on the content of any specific ECB press conference, any speech, or
any text that follows. Generate the panel based on macroeconomic regime alone.

Panel construction:
- Generate exactly 30 participants, identified as T001 through T030.
- Assign each participant:
    (a) risk_aversion      : High / Medium / Low — let the distribution reflect
                             the prevailing uncertainty in euro-area rate markets
                             around [date], based on macro regime alone.
    (b) behavioral_biases  : 1–2 biases per trader, drawn from:
                             Confirmation Bias, Overconfidence, Anchoring,
                             Herding, Loss Aversion, Recency Bias.
                             Separate multiple biases with a semicolon.
    (c) interpretation_style : one of:
                             Fundamentalist, Sentiment Reader, Quantitative,
                             Skeptic, Narrative-Driven.
- Distribute risk aversion, biases, and interpretation styles to reflect
  realistic heterogeneity across euro OIS market participants.
- Ensure meaningful spread: not all participants should share the same
  risk_aversion level or the same interpretation_style.

Output:
Return ONLY the following markdown table. No preamble, no commentary, and no
explanation before or after the table. The table must have exactly these four
columns in this order.

| agent_id | risk_aversion | behavioral_biases               | interpretation_style |
|----------|---------------|---------------------------------|----------------------|
| T001     | Medium        | Anchoring                       | Quantitative         |
| T002     | High          | Overconfidence; Herding         | Sentiment Reader     |
| T003     | Low           | Confirmation Bias               | Fundamentalist       |
| ...      | ...           | ...                             | ...                  |
| T030     | Medium        | Loss Aversion                   | Skeptic              |

Generate all 30 rows. The agent_id column must run sequentially from T001 to T030.
"

cat(strrep("=", 70), "\n")
cat("STAGE 1 PROMPT (panel_only_prompt) — for review:\n")
cat(strrep("=", 70), "\n")
cat(panel_only_prompt, "\n")
cat(strrep("=", 70), "\n\n")

cat(crayon::cyan("▶ Awaiting prompt approval before any API calls.\n"))
cat(crayon::cyan("  Type 'yes' and press Enter to proceed with Stage 1, or anything else to abort:\n"))
ans_2 <- readline()
if (tolower(trimws(ans_2)) != "yes") {
  toc()
  stop("User did not approve panel_only_prompt. Aborting.", call. = FALSE)
}
cat(crayon::green("  Panel prompt approved.\n\n"))

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
# 6. STAGE 1 EXECUTION — parallel panel generation (300 calls: 30 dates × 10 seeds)
#
# Each (date, run) pair gets its own independently-drawn panel (seed = run).
# This averages out panel-composition luck alongside forecast randomness so
# that the within-run SD in Stage 2 reflects both sources of variation.
# ==============================================================================

panel_grid <- expand_grid(date = the_dates, run = 1:10) %>%
  sample_n(nrow(.))

cat(crayon::blue(paste0(
  "Stage 1: generating ", nrow(panel_grid),
  " agent panels in parallel (", length(the_dates), " dates x 10 seeds)...\n\n"
)))

generate_panel <- function(date, run, prompt_template) {
  target <- file.path(panels_dir, sprintf("%s_%02d.rds", date, run))

  if (file.exists(target) && file.size(target) > 0) {
    cat(crayon::yellow(paste0("  Skip panel: ", date, " run ", run, " (exists)\n")))
    return(invisible(TRUE))
  }

  cat(crayon::yellow(paste0("  Generating panel: ", date, " run ", run, "\n")))

  prompt   <- gsub("\\[date\\]", date, prompt_template)
  response <- call_openrouter(prompt, seed = run, temperature = 1)

  if (!is.null(response)) {
    saveRDS(response, target)
    cat(crayon::green(paste0(
      "  Saved panel: ", date, "_", sprintf("%02d", run), ".rds\n"
    )))
    return(TRUE)
  } else {
    cat(
      file   = log_panels,
      append = TRUE,
      paste0(
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        " | date=", date, " | run=", run,
        " | error=max retries exceeded\n"
      )
    )
    cat(crayon::red(paste0("  FAILED panel: ", date, " run ", run, "\n")))
    return(FALSE)
  }
}

plan(multisession, workers = 5)

tic("Stage 1 API phase")

future_pmap(
  list(date = panel_grid$date, run = panel_grid$run),
  generate_panel,
  prompt_template = panel_only_prompt,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

toc()
plan(sequential)

# Stage 1 sanity check --------------------------------------------------------

n_panels    <- length(list.files(panels_dir, pattern = "\\.rds$"))
n_panels_ex <- nrow(panel_grid)   # 300
cat(crayon::blue(paste0(
  "\nStage 1 complete. Panels found: ", n_panels, " / ", n_panels_ex, "\n"
)))
if (n_panels < n_panels_ex) {
  cat(crayon::red(paste0(
    "  WARNING: fewer than ", n_panels_ex, " panels present. Check failed_panels.log.\n"
  )))
}

parse_panel_rows <- function(response_text) {
  lines <- unlist(strsplit(as.character(response_text), "\n"))
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]
  pat   <- paste0(
    "^\\|?\\s*(T\\d{3})\\s*\\|\\s*(.+?)\\s*\\|\\s*(.+?)\\s*\\|",
    "\\s*(.+?)\\s*\\|?\\s*$"
  )
  m <- str_match(lines, pat)
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  if (nrow(m) == 0) return(tibble())
  tibble(
    agent_id             = m[, 2],
    risk_aversion        = m[, 3],
    behavioral_biases    = m[, 4],
    interpretation_style = m[, 5]
  )
}

# Check every panel; stop if any parse to fewer than 25 rows
panel_parse_issues <- character(0)
for (i in seq_len(nrow(panel_grid))) {
  pd <- panel_grid$date[i]
  pr <- panel_grid$run[i]
  pf <- file.path(panels_dir, sprintf("%s_%02d.rds", pd, pr))
  if (!file.exists(pf)) {
    panel_parse_issues <- c(panel_parse_issues, sprintf("%s_%02d", pd, pr))
    next
  }
  pp <- parse_panel_rows(readRDS(pf))
  if (nrow(pp) < 25) panel_parse_issues <- c(panel_parse_issues, sprintf("%s_%02d", pd, pr))
}

if (length(panel_parse_issues) > 0) {
  cat(crayon::red(paste0(
    "\n  ", length(panel_parse_issues),
    " panel(s) parsed to < 25 rows — deleting and re-running those calls:\n"
  )))
  for (stem in panel_parse_issues) {
    bad_file <- file.path(panels_dir, paste0(stem, ".rds"))
    cat(crayon::red(paste0("    Deleting: ", bad_file, "\n")))
    if (file.exists(bad_file)) file.remove(bad_file)
  }
  cat(crayon::yellow("  Re-running failed panels sequentially...\n"))
  plan(sequential)
  for (stem in panel_parse_issues) {
    parts <- stringr::str_match(stem, "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)$")
    generate_panel(parts[1, 2], as.integer(parts[1, 3]), panel_only_prompt)
  }
  # Re-check
  still_bad <- character(0)
  for (stem in panel_parse_issues) {
    pf <- file.path(panels_dir, paste0(stem, ".rds"))
    if (!file.exists(pf) || nrow(parse_panel_rows(readRDS(pf))) < 25)
      still_bad <- c(still_bad, stem)
  }
  if (length(still_bad) > 0) {
    cat(crayon::red(paste0(
      "\n  STOP: ", length(still_bad),
      " panel(s) still bad after retry — manual inspection required:\n"
    )))
    for (d in still_bad) cat(crayon::red(paste0("    ", d, "\n")))
    toc()
    stop("Panel parse failure after retry. Inspect responses and re-run.", call. = FALSE)
  }
  cat(crayon::green("  All re-run panels now parse to >= 25 rows.\n"))
}

cat(crayon::green(paste0("  All ", n_panels_ex, " panels parse to >= 25 rows.\n")))

sample_panel  <- readRDS(file.path(panels_dir, sprintf("%s_%02d.rds", the_dates[1], 1L)))
parsed_sample <- parse_panel_rows(sample_panel)
cat(crayon::blue(paste0(
  "  Sample panel (", the_dates[1], " run 01): ",
  nrow(parsed_sample), " rows, ",
  ncol(parsed_sample), " columns\n"
)))
cat("  Head of parsed panel:\n")
print(head(parsed_sample, 5))
cat("\n")

# ==============================================================================
# 7. STAGE 2 PROMPT — transcript-conditioned forecast with frozen panel
#    (character literal; NOT in config/prompts.R)
# ==============================================================================

forecast_given_panel_prompt <- "
You are conducting a two-stage simulation of the euro-area interest rate swap
market. In Stage 1, a panel of 30 market participants was constructed based
solely on the macroeconomic regime prevailing around [date]. That panel is
provided below. In Stage 2 — your task here — you will generate individual
rate forecasts for each participant, conditional on the ECB press conference
transcript provided at the end of this prompt.

FIXED PANEL — use exactly as given; do not regenerate, rename, reorder,
reinterpret, or add participants:
[panel]

Instructions:
- Use ONLY the 30 participants listed in the panel above (T001–T030).
- For each participant, simulate their individual forecast in the euro-area
  interest rate swap market across three tenors: 3 months (3M), 2 years (2Y),
  and 10 years (10Y), conditional on the press conference transcript below.
- Each participant reads and interprets the transcript through the lens of
  their assigned risk aversion, behavioral biases, and interpretation style.
- For each (participant x tenor) combination, provide:
    (a) Expected direction: Up / Down / Unchanged — relative to the
        pre-conference rate.
    (b) New expected swap rate in percent, to two decimal places.
    (c) Confidence score (0-100) reflecting how strongly the participant
        believes in their forecast, given their characteristics and their
        interpretation of the transcript.

Output:
Return ONLY the following markdown table — one row per (participant x tenor)
combination, for all 30 participants and all 3 tenors (90 data rows total).
No preamble, no commentary, no explanation before or after the table.

| Date       | Trader ID | Tenor | Expected Direction | New Expected Rate (%) | Confidence (%) |
|------------|-----------|-------|--------------------|-----------------------|----------------|
| YYYY-MM-DD | T001      | 3M    | Up                 | 3.15                  | 65             |
| YYYY-MM-DD | T001      | 2Y    | Down               | 2.85                  | 80             |
| YYYY-MM-DD | T001      | 10Y   | Unchanged          | 2.60                  | 50             |
| ...        | ...       | ...   | ...                | ...                   | ...            |
| YYYY-MM-DD | T030      | 10Y   | Up                 | 2.75                  | 70             |

Guidelines:
- Use only the information available as of [date].
- Each participant's forecast must reflect their unique characteristics from
  the panel — risk aversion, behavioral biases, and interpretation style.
- Ensure diversity in interpretation: not all participants should forecast the
  same direction or the same rate level.
- Do not aggregate or summarize responses.
- Output ONLY the markdown table — no JSON or other format.
"

cat(strrep("=", 70), "\n")
cat("STAGE 2 PROMPT (forecast_given_panel_prompt) — for review:\n")
cat(strrep("=", 70), "\n")
cat(forecast_given_panel_prompt, "\n")
cat(strrep("=", 70), "\n\n")

cat(crayon::cyan("▶ Awaiting Stage 2 prompt approval before parallel execution.\n"))
cat(crayon::cyan("  Type 'yes' and press Enter to proceed with Stage 2, or anything else to abort:\n"))
ans_3 <- readline()
if (tolower(trimws(ans_3)) != "yes") {
  toc()
  stop("User did not approve forecast_given_panel_prompt. Aborting.", call. = FALSE)
}
cat(crayon::green("  Stage 2 prompt approved.\n\n"))

# ==============================================================================
# 8. STAGE 2 GRID
# ==============================================================================

grid <- expand_grid(date = the_dates, run = 1:10) %>%
  sample_n(nrow(.))

n_expected <- 300L

cat(crayon::green(paste0(
  "Stage 2 grid: ", nrow(grid), " rows (",
  n_distinct(grid$date), " dates x 10 runs)\n\n"
)))

# ==============================================================================
# 9. STAGE 2 PER-CALL FUNCTION
# ==============================================================================

process_one <- function(date, run, transcripts_map, prompt_template) {
  target <- sprintf("%s/%s_%02d.rds", runs_dir, date, run)

  if (file.exists(target) && file.size(target) > 0) {
    cat(crayon::yellow(paste0("  Skip: ", date, " run ", run, " (exists)\n")))
    return(invisible(TRUE))
  }

  cat(crayon::yellow(paste0("  Starting: ", date, " run ", run, "\n")))

  panel_file <- file.path(panels_dir, sprintf("%s_%02d.rds", date, run))
  if (!file.exists(panel_file) || file.size(panel_file) == 0) {
    cat(crayon::red(paste0(
      "  FAILED: ", date, " run ", run, " — panel file missing\n"
    )))
    return(FALSE)
  }

  panel_response <- readRDS(panel_file)
  text           <- transcripts_map[[as.character(date)]]

  full_prompt <- gsub("\\[panel\\]", panel_response, prompt_template)
  full_prompt <- gsub("\\[date\\]",  date,           full_prompt)
  full_prompt <- paste0(
    full_prompt,
    "Press Conference on ", date, "\n",
    "Text: ", text, "\n\n"
  )

  response <- call_openrouter(full_prompt, seed = run)

  if (!is.null(response)) {
    saveRDS(response, target)
    cat(crayon::green(paste0(
      "  Saved: ", date, "_", sprintf("%02d", run), ".rds\n"
    )))
    return(TRUE)
  } else {
    cat(
      file   = log_calls,
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
# 10. STAGE 2 PARALLEL EXECUTION
# ==============================================================================

plan(multisession, workers = 5)

cat(crayon::blue(paste0(
  "Launching parallel Stage 2: ", nrow(grid),
  " calls across 5 workers\n\n"
)))

tic("Stage 2 API phase")

future_pmap(
  list(date = grid$date, run = grid$run),
  process_one,
  transcripts_map = transcripts_map,
  prompt_template = forecast_given_panel_prompt,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

toc()
plan(sequential)

# ==============================================================================
# 11. COMPLETION CHECK
# ==============================================================================

completed_files <- list.files(runs_dir, pattern = "\\.rds$")
n_completed     <- length(completed_files)
n_missing       <- n_expected - n_completed

completed_grid <- tibble(stem = tools::file_path_sans_ext(completed_files)) %>%
  tidyr::extract(
    stem,
    into  = c("date", "run"),
    regex = "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)$"
  ) %>%
  mutate(run = as.integer(run))

full_grid_check <- expand_grid(date = the_dates, run = 1:5)
missing_grid    <- anti_join(full_grid_check, completed_grid, by = c("date", "run"))

cat("\n", strrep("=", 60), "\n")
cat(crayon::green(paste0("Completed:  ", n_completed, " / ", n_expected, " Stage 2 calls\n")))

if (nrow(missing_grid) > 0) {
  cat(crayon::red(paste0("Missing:    ", nrow(missing_grid), " calls\n")))
  saveRDS(missing_grid, "../intermediate_data/endogeneity_p1/missing_grid.rds")
  cat(crayon::red("Re-run the script — only missing calls will be retried.\n"))
  cat(strrep("=", 60), "\n\n")
  toc()
  stop("Incomplete Stage 2 run — see missing_grid.rds for details.", call. = FALSE)
} else {
  cat(crayon::green("All Stage 2 calls complete — proceeding to parsing.\n"))
}
cat(strrep("=", 60), "\n\n")

# ==============================================================================
# 12. PARSING — regex from run_clarity_ensemble_p1.R
# ==============================================================================

cat(crayon::blue("Parsing Stage 2 responses...\n"))

parse_one_run <- function(filepath) {
  tryCatch({

    response <- readRDS(filepath)
    if (is.null(response) || nchar(trimws(response)) == 0) return(NULL)

    stem  <- tools::file_path_sans_ext(basename(filepath))
    parts <- stringr::str_match(stem, "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)$")
    if (is.na(parts[1, 1])) return(NULL)

    conf_date <- parts[1, 2]
    run_id    <- as.integer(parts[1, 3])

    lines <- unlist(strsplit(as.character(response), "\n"))
    lines <- trimws(lines)
    lines <- lines[nchar(lines) > 0]

    pattern <- paste0(
      "^\\|?\\s*",
      "(\\d{4}-\\d{2}-\\d{2})\\s*\\|\\s*",
      "(T\\d{3})\\s*\\|\\s*",
      "(3M|2Y|10Y|3m|3mnt|3-month|10y|10yr|10-year)\\s*\\|\\s*",
      "(Up|Down|Unchanged)\\s*\\|\\s*",
      "([0-9]+\\.?[0-9]*)\\s*\\|\\s*",
      "([0-9]+)\\s*\\|?$"
    )

    matches <- stringr::str_match(lines, pattern)
    matches <- matches[!is.na(matches[, 1]), , drop = FALSE]
    if (nrow(matches) == 0) return(NULL)

    tibble::tibble(
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
      mutate(date = conf_date, run = run_id) %>%
      select(date, run, agent_id, tenor, direction, rate, confidence) %>%
      distinct()

  }, error = function(e) {
    message("Failed parsing: ", basename(filepath), " -- ", e$message)
    return(NULL)
  })
}

run_files      <- list.files(runs_dir, pattern = "\\.rds$", full.names = TRUE)
endo_runs_long <- purrr::map(run_files, parse_one_run) %>%
  purrr::compact() %>%
  dplyr::bind_rows()

cat(crayon::green(paste0(
  "Parsed: ", nrow(endo_runs_long), " rows from ", length(run_files), " files (",
  n_distinct(endo_runs_long$date), " dates, ",
  n_distinct(endo_runs_long$run),  " runs, ",
  n_distinct(endo_runs_long$tenor), " tenors)\n\n"
)))

saveRDS(endo_runs_long, file.path(parsed_dir, "endo_runs_long.rds"))
writexl::write_xlsx(endo_runs_long, file.path(parsed_dir, "endo_runs_long.xlsx"))
cat(crayon::green("Saved: parsed/endo_runs_long (.rds + .xlsx)\n\n"))

# ==============================================================================
# 13. ENSEMBLE AGGREGATION
# ==============================================================================

cat(crayon::blue("Computing ensemble aggregation...\n"))

min_agents_required <- 15L

within_run <- endo_runs_long %>%
  group_by(date, run, tenor) %>%
  filter(n() >= min_agents_required) %>%
  summarise(
    within_sd = sd(rate, na.rm = TRUE),
    mean_rate = mean(rate, na.rm = TRUE),
    n_agents  = n(),
    .groups   = "drop"
  )

endo_ensemble <- within_run %>%
  group_by(date, tenor) %>%
  summarise(
    sd_mean   = mean(within_sd, na.rm = TRUE),
    sd_se     = ifelse(n() > 1,
                       sd(within_sd, na.rm = TRUE) / sqrt(n()),
                       NA_real_),
    n_runs    = n(),
    mean_rate = mean(mean_rate, na.rm = TRUE),
    .groups   = "drop"
  )

saveRDS(within_run,    file.path(parsed_dir, "endo_within_run.rds"))
writexl::write_xlsx(within_run,    file.path(parsed_dir, "endo_within_run.xlsx"))

saveRDS(endo_ensemble, file.path(parsed_dir, "endo_ensemble.rds"))
writexl::write_xlsx(endo_ensemble, file.path(parsed_dir, "endo_ensemble.xlsx"))

cat(crayon::green(paste0(
  "Saved: endo_within_run (", nrow(within_run), " rows), ",
  "endo_ensemble (", nrow(endo_ensemble), " rows) (.rds + .xlsx each)\n\n"
)))

# ==============================================================================
# 14. COMPARISON WITH HEADLINE ZERO-SHOT
# ==============================================================================

cat(crayon::blue("Loading headline zero-shot ensemble for comparison...\n"))

headline_path <- "../intermediate_data/p1/p1_ensemble_sd.rds"

if (!file.exists(headline_path)) {
  toc()
  stop(
    "Headline ensemble file not found at:\n  ", headline_path, "\n",
    "Please confirm the correct path and update 'headline_path' in the script ",
    "before re-running.",
    call. = FALSE
  )
}

headline_full <- readRDS(headline_path)

headline_subset <- headline_full %>%
  mutate(date = as.character(date)) %>%
  filter(date %in% the_dates) %>%
  rename(sd_mean_head = sd_mean) %>%
  select(date, tenor, sd_mean_head)

cat(crayon::green(paste0(
  "  Headline subset: ", nrow(headline_subset), " rows across ",
  n_distinct(headline_subset$date), " dates.\n\n"
)))

market_vol <- range_df %>%
  mutate(
    tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor),
    date  = as.character(date)
  ) %>%
  filter(tenor %in% c("3M", "2Y", "10Y")) %>%
  select(date, tenor, vol_1d = correct_post_mean_1)

merged <- endo_ensemble %>%
  mutate(date = as.character(date)) %>%
  select(date, tenor, sd_mean_endo = sd_mean) %>%
  inner_join(headline_subset, by = c("date", "tenor")) %>%
  inner_join(market_vol,      by = c("date", "tenor")) %>%
  filter(!is.na(sd_mean_endo), !is.na(sd_mean_head), !is.na(vol_1d))

cat(crayon::green(paste0(
  "  Merged comparison dataset: ", nrow(merged), " rows (",
  n_distinct(merged$date), " dates x ", n_distinct(merged$tenor), " tenors)\n\n"
)))

# Bootstrap -------------------------------------------------------------------

boot_spearman <- function(data, indices, x_var, y_var) {
  d <- data[indices, ]
  cor(d[[x_var]], d[[y_var]], method = "spearman", use = "complete.obs")
}

boot_delta_rho <- function(data, indices) {
  d        <- data[indices, ]
  rho_endo <- cor(d$sd_mean_endo, d$vol_1d,    method = "spearman", use = "complete.obs")
  rho_head <- cor(d$sd_mean_head, d$vol_1d,    method = "spearman", use = "complete.obs")
  rho_head - rho_endo
}

n_boot <- 5000L
set.seed(20260507)

cat(crayon::blue(paste0(
  "Running bootstrap (", n_boot, " reps x 3 tenors x 4 statistics)...\n"
)))

comparison_list <- map(c("3M", "2Y", "10Y"), function(tn) {

  d <- merged %>% filter(tenor == tn)
  n <- nrow(d)

  b_ev  <- boot(d, function(dat, idx) boot_spearman(dat, idx, "sd_mean_endo", "vol_1d"),
                R = n_boot)
  ci_ev <- boot.ci(b_ev, type = "perc", conf = 0.95)

  b_hv  <- boot(d, function(dat, idx) boot_spearman(dat, idx, "sd_mean_head", "vol_1d"),
                R = n_boot)
  ci_hv <- boot.ci(b_hv, type = "perc", conf = 0.95)

  b_eh  <- boot(d, function(dat, idx) boot_spearman(dat, idx, "sd_mean_endo", "sd_mean_head"),
                R = n_boot)
  ci_eh <- boot.ci(b_eh, type = "perc", conf = 0.95)

  b_dr  <- boot(d, boot_delta_rho, R = n_boot)
  ci_dr <- boot.ci(b_dr, type = "perc", conf = 0.95)

  tibble(
    tenor                     = tn,
    n_conferences             = n,
    spearman_endo_vs_vol      = cor(d$sd_mean_endo, d$vol_1d,
                                    method = "spearman", use = "complete.obs"),
    ci_low_endo               = ci_ev$percent[4],
    ci_high_endo              = ci_ev$percent[5],
    spearman_headline_vs_vol  = cor(d$sd_mean_head, d$vol_1d,
                                    method = "spearman", use = "complete.obs"),
    ci_low_head               = ci_hv$percent[4],
    ci_high_head              = ci_hv$percent[5],
    spearman_endo_vs_headline = cor(d$sd_mean_endo, d$sd_mean_head,
                                    method = "spearman", use = "complete.obs"),
    ci_low_eh                 = ci_eh$percent[4],
    ci_high_eh                = ci_eh$percent[5],
    delta_rho                 = cor(d$sd_mean_head, d$vol_1d,
                                    method = "spearman", use = "complete.obs") -
                                cor(d$sd_mean_endo, d$vol_1d,
                                    method = "spearman", use = "complete.obs"),
    ci_low_delta              = ci_dr$percent[4],
    ci_high_delta             = ci_dr$percent[5]
  )
})

comparison_tbl <- bind_rows(comparison_list)

saveRDS(comparison_tbl, file.path(parsed_dir, "endo_vs_headline_comparison.rds"))
writexl::write_xlsx(comparison_tbl, file.path(parsed_dir, "endo_vs_headline_comparison.xlsx"))
cat(crayon::green("Saved: parsed/endo_vs_headline_comparison (.rds + .xlsx)\n\n"))

# ==============================================================================
# 15. SANITY CHECKS
# ==============================================================================

cat(strrep("-", 60), "\n")
cat("SANITY CHECKS\n")
cat(strrep("-", 60), "\n\n")

cat(paste0(
  "Number of Stage 1 panels:        ",
  length(list.files(panels_dir, pattern = "\\.rds$")),
  " (expect 300)\n"
))
cat(paste0(
  "Number of Stage 2 runs completed: ", n_completed, " (expect 300)\n\n"
))

cat("Mean within-run SD by tenor (compare to headline; expect ~10-20 bps range):\n")
within_run %>%
  group_by(tenor) %>%
  summarise(mean_within_sd = mean(within_sd, na.rm = TRUE), .groups = "drop") %>%
  print()

cat("\nHead of comparison table:\n")
print(head(comparison_tbl))
cat("\n")

for (i in seq_len(nrow(comparison_tbl))) {
  row          <- comparison_tbl[i, ]
  zero_in_ci   <- row$ci_low_delta <= 0 && 0 <= row$ci_high_delta
  delta_label  <- sprintf("%.3f", abs(row$delta_rho))
  ci_label     <- sprintf("[%.3f, %.3f]", row$ci_low_delta, row$ci_high_delta)
  msg          <- paste0(
    "Tenor ", row$tenor, ": |Deltaρ| = ", delta_label, ", 95% CI ", ci_label
  )
  if (zero_in_ci) {
    cat(crayon::green(paste0("  [REFUTED]   ", msg, "\n")))
  } else {
    cat(crayon::red(paste0("  [CONCERN]   ", msg, "\n")))
  }
}

# ==============================================================================
# 16. LOGGING
# ==============================================================================

toc()

cat("\n", strrep("=", 80), "\n")
cat(crayon::green("ENDOGENEITY TEST P1 COMPLETE\n"))
cat(strrep("=", 80), "\n\n")

cat(
  "NOTE: Stage 1 panels use baseline fields (risk_aversion, behavioral_biases,\n",
  "  interpretation_style) to match the zero-shot prompt schema. The archetype\n",
  "  field is absent from this run; Figure 10 (archetype heatmap) cannot be\n",
  "  reproduced from these panels.\n\n"
)

cat(
  "INTERPRETATION:\n",
  "  Deltaρ = spearman_headline_vs_vol - spearman_endo_vs_vol.\n",
  "  Deltaρ ~= 0 across all tenors, with 0 inside the 95% CI, indicates\n",
  "  that the dynamic agent generation captures macroeconomic regime\n",
  "  conditioning rather than transcript-content conditioning. The\n",
  "  within-call confounding concern (transcript entering twice: once to\n",
  "  shape the panel, once to generate forecasts) is empirically refuted\n",
  "  at this sample size.\n",
  "  Deltaρ significantly > 0 would indicate the headline model benefits\n",
  "  from joint generation, suggesting a partial confound worth further\n",
  "  investigation.\n\n",
  sep = ""
)

#===============================================================================
# END OF SCRIPT
#===============================================================================
