#===============================================================================
# INTRO ENSEMBLE P1 — All ECB Conferences: Full Transcript vs Intro Only
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Counterfactual C: compare trader disagreement when agents read the full
#   press conference transcript vs. the introductory statement only.
#   Tests whether the Q&A drives disagreement beyond the statement's content.
#
#   Versions: "full" (complete transcript) vs "intro" (statement only)
#   Grid:     ALL ECB conferences in full_ensemble_p1/runs/ × 2 versions × R=10
#   Model:    Gemini 2.5-Flash via OpenRouter
#
#   Cache logic:
#     "full"  runs are seeded directly from full_ensemble_p1/runs/ (no API calls).
#     "intro" runs are skipped if {date}_intro_{run:02d}.rds already exists.
#     Re-running the script only calls the API for genuinely missing cells.
#
# Data sources:
#   Dates:  derived from ../intermediate_data/full_ensemble_p1/runs/ filenames
#   Full:   ../intermediate_data/texts/{date}_{president}.txt
#   Intro:  ../intermediate_data/texts/introductory_statements/{date}_{president}.txt
#
# Outputs (ALL under intermediate_data/intro_ensemble_p1/):
#   runs/{date}_{version}_{run}.rds
#   failed_calls.log
#   missing_grid.rds              (only if incomplete)
#   parsed/intro_runs_long.{rds,xlsx}
#   parsed/intro_within_run.{rds,xlsx}
#   parsed/intro_ensemble.{rds,xlsx}
#   parsed/intro_treatment_effect.{rds,xlsx}
#
# Usage:
#   Rscript code/run_intro_ensemble_p1.R
#   source("run_intro_ensemble_p1.R")   # from code/ directory
#   Re-run at any time — completed calls are skipped automatically.
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("INTRO ENSEMBLE P1 — All Conferences: Full Transcript vs Intro Only x R=10\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# HARD ASSERTION: ALL OUTPUTS MUST GO UNDER intro_ensemble_p1/
# ==============================================================================

output_root <- normalizePath(
  "../intermediate_data/intro_ensemble_p1",
  mustWork = FALSE
)
if (!grepl("intro_ensemble_p1", output_root, fixed = TRUE)) {
  stop(
    "HARD ASSERT: output root does not contain 'intro_ensemble_p1'. ",
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
name_prompt_request <- "naive_intro_R10"

cat(crayon::green(paste0("Loaded prompt: prompt_naive  [", name_prompt_request, "]\n\n")))

runs_dir   <- "../intermediate_data/intro_ensemble_p1/runs"
parsed_dir <- "../intermediate_data/intro_ensemble_p1/parsed"
log_file   <- "../intermediate_data/intro_ensemble_p1/failed_calls.log"

dir.create(runs_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(parsed_dir, recursive = TRUE, showWarnings = FALSE)

tic("Total script")

# ==============================================================================
# 2. FILE-SYSTEM INVENTORY
# ==============================================================================

cat(crayon::blue("=== FILE-SYSTEM INVENTORY ===\n"))

full_cache_dir <- "../intermediate_data/full_ensemble_p1/runs"
texts_dir      <- "../intermediate_data/texts"
intro_dir      <- "../intermediate_data/texts/introductory_statements"

cat("\nChecking required paths:\n")
for (d in c(full_cache_dir, texts_dir, intro_dir)) {
  if (file.exists(d)) {
    cat(crayon::green(paste0("  OK:      ", d, "\n")))
  } else {
    cat(crayon::red(paste0("  MISSING: ", d, "\n")))
  }
}

cat(crayon::green(paste0(
  "\nIntroductory statement files: ",
  length(list.files(intro_dir, pattern = "\\.txt$")), "\n\n"
)))

# ==============================================================================
# 3. DATA LOAD
# ==============================================================================

# Derive the date universe from full_ensemble_p1/runs/ filenames.
# Pattern: {date}_{run:02d}.rds — extract the date portion.
cat(crayon::blue("Deriving conference dates from full_ensemble_p1/runs/ ...\n"))

full_cache_files <- list.files(
  full_cache_dir,
  pattern = "^\\d{4}-\\d{2}-\\d{2}_\\d+\\.rds$"
)
the_dates <- sort(unique(as.Date(
  stringr::str_extract(full_cache_files, "\\d{4}-\\d{2}-\\d{2}")
)))
the_dates <- the_dates[!is.na(the_dates)]

n_dates <- length(the_dates)
cat(crayon::green(paste0(
  "  ", n_dates, " unique conference dates found in full_ensemble_p1/runs/.\n\n"
)))

# Load full transcripts -------------------------------------------------------
cat(crayon::blue("Loading full transcripts from texts/...\n"))

text_files <- list.files(texts_dir, pattern = "\\.txt$", full.names = TRUE,
                         recursive = FALSE)
file_dates <- str_extract(basename(text_files), "\\d{4}-\\d{2}-\\d{2}")
valid      <- !is.na(file_dates)
text_map   <- set_names(text_files[valid], file_dates[valid])

full_texts <- map_chr(as.character(the_dates), function(d) {
  if (d %in% names(text_map)) {
    return(tryCatch(readtext::readtext(text_map[[d]])$text,
                    error = function(e) NA_character_))
  }
  d_date <- as.Date(d)
  nearby <- names(text_map)[abs(as.Date(names(text_map)) - d_date) <= 3]
  if (length(nearby) > 0) {
    cat(crayon::yellow(paste0(
      "  Near-date match (full): ", d, " -> ", nearby[1], " (diff ",
      as.integer(abs(as.Date(nearby[1]) - d_date)), " day(s))\n"
    )))
    return(tryCatch(readtext::readtext(text_map[[nearby[1]]])$text,
                    error = function(e) NA_character_))
  }
  NA_character_
})
full_texts <- set_names(full_texts, as.character(the_dates))

# Load introductory statements ------------------------------------------------
cat(crayon::blue("Loading introductory statements from texts/introductory_statements/...\n"))

intro_files <- list.files(intro_dir, pattern = "\\.txt$", full.names = TRUE)
intro_dates <- str_extract(basename(intro_files), "\\d{4}-\\d{2}-\\d{2}")
intro_valid <- !is.na(intro_dates)
intro_map   <- set_names(intro_files[intro_valid], intro_dates[intro_valid])

intro_texts <- map_chr(as.character(the_dates), function(d) {
  if (d %in% names(intro_map)) {
    return(tryCatch(readtext::readtext(intro_map[[d]])$text,
                    error = function(e) NA_character_))
  }
  d_date <- as.Date(d)
  nearby <- names(intro_map)[abs(as.Date(names(intro_map)) - d_date) <= 3]
  if (length(nearby) > 0) {
    cat(crayon::yellow(paste0(
      "  Near-date match (intro): ", d, " -> ", nearby[1], " (diff ",
      as.integer(abs(as.Date(nearby[1]) - d_date)), " day(s))\n"
    )))
    return(tryCatch(readtext::readtext(intro_map[[nearby[1]]])$text,
                    error = function(e) NA_character_))
  }
  NA_character_
})
intro_texts <- set_names(intro_texts, as.character(the_dates))

# Exclude dates missing either transcript -------------------------------------
missing_full  <- as.character(the_dates)[is.na(full_texts)  | nchar(trimws(full_texts))  == 0]
missing_intro <- as.character(the_dates)[is.na(intro_texts) | nchar(trimws(intro_texts)) == 0]
drop_dates    <- union(missing_full, missing_intro)

if (length(drop_dates) > 0) {
  cat(crayon::yellow(paste0(
    "  WARNING: Missing transcript for ", length(drop_dates), " date(s): ",
    paste(drop_dates, collapse = ", "),
    "\n  These dates will be excluded from the grid.\n\n"
  )))
  keep        <- !(as.character(the_dates) %in% drop_dates)
  the_dates   <- the_dates[keep]
  full_texts  <- full_texts[keep]
  intro_texts <- intro_texts[keep]
  n_dates     <- length(the_dates)
}

stopifnot(
  "full_texts length mismatch"  = length(full_texts)  == n_dates,
  "intro_texts length mismatch" = length(intro_texts) == n_dates,
  "remaining NA in full_texts"  = !any(is.na(full_texts)),
  "remaining NA in intro_texts" = !any(is.na(intro_texts))
)

cat(crayon::green(paste0("All data assertions passed. Running on ", n_dates, " dates.\n\n")))

# Sample sanity-check ---------------------------------------------------------
check_date <- as.character(the_dates[1])
cat(crayon::cyan(paste0("=== Sample: ", check_date, " — FULL (first 200 chars) ===\n")))
cat(substr(full_texts[[check_date]], 1, 200), "\n\n")
cat(crayon::cyan(paste0("=== Sample: ", check_date, " — INTRO (first 200 chars) ===\n")))
cat(substr(intro_texts[[check_date]], 1, 200), "\n\n")

# ==============================================================================
# 4. GRID
# ==============================================================================

grid <- expand_grid(
  date    = as.character(the_dates),
  version = c("full", "intro"),
  run     = 1:10
) %>%
  sample_n(nrow(.))

n_expected <- n_dates * 2 * 10

cat(crayon::green(paste0(
  "Grid ready: ", nrow(grid), " rows (",
  n_distinct(grid$date), " dates x 2 versions x 10 runs)\n\n"
)))

# ==============================================================================
# 5. OPENROUTER CALLER
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
    Sys.sleep(5 * attempt)

    result <- tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_headers(
          "Authorization" = paste("Bearer", api_key),
          "HTTP-Referer"  = "http://localhost",
          "X-Title"       = "ECB-intro-ensemble",
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

process_one <- function(date, version, run, full_texts, intro_texts,
                        prompt_template) {
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

  text <- if (version == "full") {
    full_texts[[as.character(date)]]
  } else {
    intro_texts[[as.character(date)]]
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
# 5. SEED FROM CACHE (full_ensemble_p1)
# ==============================================================================
# The "full" version uses the same text and prompt as the original full-ensemble
# run. Copy any already-completed files from full_ensemble_p1/runs/ so we don't
# re-call the API for those 390 runs.
# Source pattern: {date}_{run:02d}.rds  (no version in filename)
# Target pattern: {date}_full_{run:02d}.rds
# ==============================================================================

seed_from_cache <- function(runs_dir, cache_dir, target_version,
                            the_dates, n_runs = 10) {
  if (!dir.exists(cache_dir)) {
    cat(crayon::yellow(paste0(
      "  Cache dir not found, skipping: ", cache_dir, "\n"
    )))
    return(invisible(0L))
  }

  n_seeded <- 0L
  for (d in as.character(the_dates)) {
    for (r in seq_len(n_runs)) {
      target <- sprintf("%s/%s_%s_%02d.rds", runs_dir, d, target_version, r)
      source <- sprintf("%s/%s_%02d.rds",    cache_dir, d, r)

      if (!file.exists(target) && file.exists(source) && file.size(source) > 0) {
        file.copy(source, target)
        n_seeded <- n_seeded + 1L
      }
    }
  }

  cat(crayon::green(paste0(
    "  Cache seed complete: ", n_seeded, " files copied",
    " (version='", target_version, "' from ", basename(cache_dir), "/)\n"
  )))
  invisible(n_seeded)
}

cat(crayon::blue("Seeding 'full' runs from full_ensemble_p1 cache...\n"))
seed_from_cache(
  runs_dir       = runs_dir,
  cache_dir      = full_cache_dir,
  target_version = "full",
  the_dates      = the_dates
)
cat("\n")

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
  full_texts      = full_texts,
  intro_texts     = intro_texts,
  prompt_template = prompt_template,
  .progress = TRUE,
  .options  = furrr_options(seed = TRUE)
)

toc()

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
    regex = "^(\\d{4}-\\d{2}-\\d{2})_(full|intro)_(\\d+)$"
  ) %>%
  mutate(run = as.integer(run))

full_grid_check <- expand_grid(
  date    = as.character(the_dates),
  version = c("full", "intro"),
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
    "../intermediate_data/intro_ensemble_p1/missing_grid.rds"
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
# 9. PARSING
# ==============================================================================

cat(crayon::blue("Parsing responses...\n"))

parse_one_run <- function(filepath) {
  tryCatch({

    response <- readRDS(filepath)
    if (is.null(response) || nchar(trimws(response)) == 0) return(NULL)

    stem  <- tools::file_path_sans_ext(basename(filepath))
    parts <- stringr::str_match(
      stem,
      "^(\\d{4}-\\d{2}-\\d{2})_(full|intro)_(\\d+)$"
    )
    if (is.na(parts[1, 1])) return(NULL)

    conf_date    <- parts[1, 2]
    conf_version <- parts[1, 3]
    run_id       <- as.integer(parts[1, 4])

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
      select(date, version, run, agent_id, tenor, direction, rate, confidence) %>%
      distinct()

    return(result)

  }, error = function(e) {
    message("Failed parsing: ", basename(filepath), " -- ", e$message)
    return(NULL)
  })
}

run_files <- list.files(runs_dir, pattern = "\\.rds$", full.names = TRUE)

intro_runs_long <- purrr::map(run_files, parse_one_run) %>%
  purrr::compact() %>%
  dplyr::bind_rows()

cat(crayon::green(paste0(
  "Parsed: ", nrow(intro_runs_long), " rows from ", length(run_files), " files\n",
  "Dates: ", dplyr::n_distinct(intro_runs_long$date), "\n",
  "Versions: ", paste(unique(intro_runs_long$version), collapse = ", "), "\n",
  "Runs: ", dplyr::n_distinct(intro_runs_long$run), "\n\n"
)))

print(intro_runs_long %>% count(version, tenor))

# ==============================================================================
# 10. ENSEMBLE AGGREGATION
# ==============================================================================

cat(crayon::blue("Computing ensemble aggregation...\n"))

min_agents_required <- 15

intro_runs_clean <- intro_runs_long %>%
  group_by(date, version, run, tenor) %>%
  filter(n() >= min_agents_required) %>%
  ungroup()

cat(crayon::yellow(paste0(
  "Filtered sparse cells: ",
  nrow(intro_runs_long) - nrow(intro_runs_clean), " rows removed\n"
)))

within_run <- intro_runs_clean %>%
  group_by(date, version, run, tenor) %>%
  summarise(
    within_sd = sd(rate, na.rm = TRUE),
    mean_rate = mean(rate, na.rm = TRUE),
    n_agents  = n(),
    .groups   = "drop"
  )

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

# Treatment effect: gap = full - intro (positive = Q&A adds disagreement)
treatment_effect <- ensemble %>%
  select(date, tenor, version, sd_mean, sd_se) %>%
  pivot_wider(
    id_cols     = c(date, tenor),
    names_from  = version,
    values_from = c(sd_mean, sd_se),
    names_glue  = "{.value}_{version}"
  ) %>%
  mutate(
    delta_bps = sd_mean_full - sd_mean_intro,
    pct_increase = ifelse(
      sd_mean_intro > 0,
      100 * delta_bps / sd_mean_intro,
      NA_real_
    ),
    delta_se = sqrt(sd_se_full^2 + sd_se_intro^2)
  )

# ==============================================================================
# 11. SAVE OUTPUTS
# ==============================================================================

saveRDS(intro_runs_long,  file.path(parsed_dir, "intro_runs_long.rds"))
writexl::write_xlsx(intro_runs_long,  file.path(parsed_dir, "intro_runs_long.xlsx"))

saveRDS(within_run,       file.path(parsed_dir, "intro_within_run.rds"))
writexl::write_xlsx(within_run,       file.path(parsed_dir, "intro_within_run.xlsx"))

saveRDS(ensemble,         file.path(parsed_dir, "intro_ensemble.rds"))
writexl::write_xlsx(ensemble,         file.path(parsed_dir, "intro_ensemble.xlsx"))

saveRDS(treatment_effect, file.path(parsed_dir, "intro_treatment_effect.rds"))
writexl::write_xlsx(treatment_effect, file.path(parsed_dir, "intro_treatment_effect.xlsx"))

cat(crayon::green(paste0(
  "Saved: within_run (", nrow(within_run), " rows), ",
  "ensemble (", nrow(ensemble), " rows), ",
  "treatment_effect (", nrow(treatment_effect), " rows)\n\n"
)))

# ==============================================================================
# 12. SANITY CHECKS
# ==============================================================================

cat(strrep("-", 60), "\n")
cat("SANITY CHECKS\n")
cat(strrep("-", 60), "\n\n")

cat("\nRate value ranges by tenor:\n")
intro_runs_clean %>%
  group_by(tenor) %>%
  summarise(
    min  = min(rate, na.rm = TRUE),
    mean = mean(rate, na.rm = TRUE),
    max  = max(rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\nMean SD by version and tenor:\n")
within_run %>%
  group_by(version, tenor) %>%
  summarise(mean_sd = mean(within_sd, na.rm = TRUE), .groups = "drop") %>%
  print()

cat("\nMean pct_increase (full vs intro) by tenor:\n")
treatment_effect %>%
  group_by(tenor) %>%
  summarise(
    mean_pct_increase = mean(pct_increase, na.rm = TRUE),
    mean_delta_bps    = mean(delta_bps, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  print()

toc()

cat("\n", strrep("=", 80), "\n")
cat(crayon::green("INTRO ENSEMBLE P1 COMPLETE (all conferences)\n"))
cat(strrep("=", 80), "\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
