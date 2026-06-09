#===============================================================================
# GENERATE CLARITY EDITS (EXP B) — Option A Counterfactual
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Re-edit the 39 high-vol ECB press conference transcripts with a prompt
#   that targets LINGUISTIC clarity while explicitly preserving all intentional
#   policy ambiguity and data-dependence. Exp-a edits were light stylistic
#   touches; exp-b edits target the specific mechanism: forward guidance
#   readability without removing the ECB's deliberate conditionality.
#
# Editing model: openai/gpt-4o via OpenRouter (strong instruction follower)
# Output:        exp_b/{date}_clarity_edit.txt  (one file per conference)
#
# Resumable: files that already exist are skipped.
#
# Usage: source("generate_clarity_edits_p1.R")  from code/ directory
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("GENERATE CLARITY EDITS (EXP B)\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# 1. SETUP
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr2, crayon, stringr, purrr, readr, readtext, tidyverse, tictoc)

api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (nchar(api_key) == 0) stop("OPENROUTER_API_KEY not set.")

cf_dir     <- "../intermediate_data/counterfactual_cross_llm_high_vol"
dates_file <- file.path(cf_dir, "sample_conferences_high_vol.csv")
texts_dir  <- "../intermediate_data/texts"
exp_b_dir  <- file.path(cf_dir, "raw_data", "exp_b")
log_file   <- file.path(exp_b_dir, "failed_edits.log")

dir.create(exp_b_dir, recursive = TRUE, showWarnings = FALSE)

tic("Total editing run")

# ==============================================================================
# 2. EDITING PROMPT
# ==============================================================================
# Goal: reduce LINGUISTIC noise that causes unnecessary trader disagreement
# while keeping ALL intentional ECB ambiguity intact.
#
# What we target:
#   - Redundant qualifications stacked around a single claim
#   - Key signals buried inside long dependent clauses
#   - Sentences where the main point is unclear until the very end
#   - Inconsistencies between equivalent sentences in the same paragraph
#
# What we must NOT touch:
#   - Data-dependence language ("we remain data-dependent", "if the data confirm")
#   - Forward guidance conditionality (any "if/provided/subject to" rate language)
#   - Economic assessments, indicator references, risk balance judgements
#   - The actual policy decision and its rationale
# ==============================================================================

EDITING_SYSTEM_PROMPT <- "You are an expert editor for central bank communication.
Your task is to improve the LINGUISTIC CLARITY of ECB press conference statements
without changing their economic content or policy stance.

RULES:
1. PRESERVE all data-dependent language and conditionality exactly.
   If the original says 'we remain data-dependent' or 'subject to incoming data',
   keep that phrase. Do not replace policy optionality with false certainty.

2. PRESERVE the ECB's actual forward guidance, risk assessment, and policy rationale.
   Do not add, remove, or alter any policy signals.

3. SIMPLIFY sentence structure:
   - Break up sentences where the main point is buried after multiple subordinate clauses.
   - Remove redundant restatements of the same point within one paragraph.
   - Place the key message at the start of sentences, not the end.

4. REMOVE linguistic filler:
   - Boilerplate phrases that add no information (e.g. 'Let me now turn to...',
     'As I mentioned earlier...', navigation phrases).
   - Redundant hedges that merely repeat a qualification already stated once.

5. OUTPUT only the edited text. No commentary, no preamble, no explanation.
   Keep the same paragraph structure as the original."

make_editing_prompt <- function(date, text) {
  paste0(
    "Edit the following ECB press conference transcript from ", date, ".\n\n",
    "Apply the editing rules: improve sentence clarity and remove linguistic noise, ",
    "but preserve ALL data-dependence, conditionality, and policy signals exactly.\n\n",
    "TRANSCRIPT:\n", text
  )
}

# ==============================================================================
# 3. LOAD DATES AND TRANSCRIPTS
# ==============================================================================

the_dates <- readr::read_csv(dates_file, show_col_types = FALSE) %>%
  filter(!is.na(date)) %>%
  pull(date) %>%
  as.Date()

cat(crayon::green(paste0(length(the_dates), " dates loaded.\n\n")))

text_files <- list.files(texts_dir, pattern = "\\.txt$",
                          full.names = TRUE, recursive = FALSE)
file_dates <- str_extract(basename(text_files), "\\d{4}-\\d{2}-\\d{2}")
valid      <- !is.na(file_dates)
text_map   <- set_names(text_files[valid], file_dates[valid])

originals <- map_chr(as.character(the_dates), function(d) {
  if (d %in% names(text_map)) {
    return(tryCatch(readtext::readtext(text_map[[d]])$text,
                    error = function(e) NA_character_))
  }
  d_date <- as.Date(d)
  nearby <- names(text_map)[abs(as.Date(names(text_map)) - d_date) <= 3]
  if (length(nearby) > 0) {
    cat(crayon::yellow(paste0(
      "  Near-date match: ", d, " -> ", nearby[1], "\n"
    )))
    return(tryCatch(readtext::readtext(text_map[[nearby[1]]])$text,
                    error = function(e) NA_character_))
  }
  NA_character_
})
originals <- set_names(originals, as.character(the_dates))

missing <- names(originals)[is.na(originals) | nchar(trimws(originals)) == 0]
if (length(missing) > 0) {
  cat(crayon::yellow(paste0(
    "WARNING: No transcript for ", length(missing), " date(s): ",
    paste(missing, collapse = ", "), "\n"
  )))
  originals <- originals[!names(originals) %in% missing]
  the_dates <- as.Date(names(originals))
}

cat(crayon::green(paste0(
  "Transcripts loaded: ", length(originals), " / ", length(the_dates), "\n\n"
)))

# ==============================================================================
# 4. OPENROUTER CALLER (chat/completions with system prompt)
# ==============================================================================

call_edit <- function(date, text, model = "openai/gpt-4o") {
  url  <- "https://openrouter.ai/api/v1/chat/completions"
  body <- list(
    model       = model,
    messages    = list(
      list(role = "system", content = EDITING_SYSTEM_PROMPT),
      list(role = "user",   content = make_editing_prompt(date, text))
    ),
    temperature = 0,   # low temperature: deterministic editing, not creative
    max_tokens  = 16000
  )

  for (attempt in 1:4) {
    Sys.sleep(3 * attempt)

    result <- tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_headers(
          "Authorization" = paste("Bearer", api_key),
          "HTTP-Referer"  = "http://localhost",
          "X-Title"       = "ECB-clarity-edit",
          "Content-Type"  = "application/json"
        ) |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(180) |>
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
# 5. EDIT LOOP (sequential — editing is fast, no parallelism needed)
# ==============================================================================

dates_to_edit <- as.character(the_dates)
n_total       <- length(dates_to_edit)
n_done        <- 0
n_skip        <- 0
n_fail        <- 0

cat(crayon::blue(paste0("Editing ", n_total, " transcripts...\n\n")))

for (d in dates_to_edit) {
  out_path <- file.path(exp_b_dir, paste0(d, "_clarity_edit.txt"))

  if (file.exists(out_path) && file.size(out_path) > 100) {
    cat(crayon::yellow(paste0("  Skip: ", d, " (exists)\n")))
    n_skip <- n_skip + 1
    next
  }

  text <- originals[[d]]
  if (is.na(text) || nchar(trimws(text)) == 0) {
    cat(crayon::red(paste0("  Skip: ", d, " (no text)\n")))
    next
  }

  cat(crayon::cyan(paste0("  Editing: ", d, " (",
                           round(nchar(text) / 1000, 1), "k chars)...\n")))

  edited_text <- call_edit(d, text)

  if (!is.null(edited_text) && nchar(trimws(edited_text)) > 100) {
    readr::write_file(edited_text, out_path)
    cat(crayon::green(paste0(
      "  Saved: ", basename(out_path),
      " (", round(nchar(edited_text) / 1000, 1), "k chars)\n"
    )))
    n_done <- n_done + 1
  } else {
    cat(
      file   = log_file,
      append = TRUE,
      paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             " | date=", d, " | error=null or empty response\n")
    )
    cat(crayon::red(paste0("  FAILED: ", d, "\n")))
    n_fail <- n_fail + 1
  }

  Sys.sleep(1)
}

# ==============================================================================
# 6. SUMMARY
# ==============================================================================

toc()

cat("\n", strrep("=", 60), "\n")
cat(crayon::green(paste0(
  "Done:    ", n_done, "\n",
  "Skipped: ", n_skip, "\n",
  "Failed:  ", n_fail, "\n"
)))

existing <- list.files(exp_b_dir, pattern = "_clarity_edit\\.txt$")
cat(crayon::green(paste0(
  "Total clarity edits in exp_b/: ", length(existing), " / ", n_total, "\n"
)))

if (n_fail > 0) {
  cat(crayon::red(paste0(
    "Re-run the script to retry failed dates. Log: ", log_file, "\n"
  )))
}

cat(strrep("=", 60), "\n\n")

cat(crayon::green(paste0(
  "Next step: run run_clarity_ensemble_p1.R to run the trader ensemble\n",
  "on original vs exp_b/ clarity-edited transcripts.\n"
)))

#===============================================================================
# END OF SCRIPT
#===============================================================================
