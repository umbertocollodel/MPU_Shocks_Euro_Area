#===============================================================================
# MICRO PILOT P1 — Ensemble Averaging Noise Diagnostic
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Micro-pilot testing whether ensemble averaging across R runs reduces
#   temperature noise enough to justify a larger pilot.
#   Gemini 2.5-Flash via OpenRouter; 5 conferences x 10 seeds = 50 API calls.
#
# Outputs:
#   ../intermediate_data/micro_pilot/dates.rds
#   ../intermediate_data/micro_pilot/runs/{date}_{run}.rds
#   ../output/micro_pilot_diagnostics.xlsx   (sheets: within_run_sd, reliability_G_R)
#   ../output/micro_pilot_reliability.pdf
#
# Usage:
#   Rscript code/micro_pilot_p1.R
#   source("micro_pilot_p1.R")   # from code/ directory
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("MICRO PILOT P1 — Ensemble Averaging Noise Diagnostic\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# 1. SETUP
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  httr2, readtext, crayon, stringr, purrr,
  tidyverse, writexl, lubridate
)

api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (nchar(api_key) == 0) {
  stop("OPENROUTER_API_KEY not set. Add it to your .Renviron file.")
}

dir.create("../intermediate_data/micro_pilot/runs", recursive = TRUE, showWarnings = FALSE)
dir.create("../output", recursive = TRUE, showWarnings = FALSE)

cat(crayon::green("Environment OK\n\n"))

# ==============================================================================
# 2. PROMPT  (reuse existing prompt_naive unchanged)
# ==============================================================================

source("config/prompts.R")
prompt_template <- prompt_naive
cat(crayon::green("Loaded prompt: prompt_naive\n\n"))

# ==============================================================================
# 3. SELECT 5 CONFERENCES
# ==============================================================================

cat(crayon::blue("Selecting 5 conferences from range_difference_df...\n"))

range_df <- readRDS("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor))

vol_per_date <- range_df %>%
  filter(!is.na(correct_post_mean_3)) %>%
  group_by(date) %>%
  summarise(mean_vol = mean(correct_post_mean_3, na.rm = TRUE), .groups = "drop") %>%
  arrange(mean_vol)

med_vol <- median(vol_per_date$mean_vol)

top2 <- vol_per_date %>% slice_tail(n = 2) %>% pull(date)
bot2 <- vol_per_date %>% slice_head(n = 2) %>% pull(date)
mid1 <- vol_per_date %>%
  filter(!date %in% c(top2, bot2)) %>%
  mutate(dist = abs(mean_vol - med_vol)) %>%
  slice_min(dist, n = 1) %>%
  pull(date)

selected_dates <- sort(c(bot2, mid1, top2))

cat(crayon::green(paste0(
  "Selected dates:\n",
  "  Low vol:    ", paste(sort(bot2), collapse = ", "), "\n",
  "  Median vol: ", mid1, "\n",
  "  High vol:   ", paste(sort(top2), collapse = ", "), "\n\n"
)))

saveRDS(selected_dates, "../intermediate_data/micro_pilot/dates.rds")

# ==============================================================================
# 4. OPENROUTER CALLER
# ==============================================================================

#' Call OpenRouter API (Gemini 2.5-Flash)
#'
#' @param prompt   Full prompt string
#' @param seed     Integer seed for reproducibility
#' @param temperature Numeric (default 1)
#' @param model    OpenRouter model id
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
# 5. RUN PILOT
# ==============================================================================

cat(crayon::blue("Loading ECB press conference transcripts...\n"))

texts_dir  <- "../intermediate_data/texts"
text_files <- list.files(texts_dir, pattern = "\\.txt$", full.names = TRUE)
file_dates <- str_extract(basename(text_files), "\\d{4}-\\d{2}-\\d{2}")

# Named vector: date -> file path (drop any that didn't match)
valid      <- !is.na(file_dates)
text_map   <- set_names(text_files[valid], file_dates[valid])

cat(crayon::green(paste0("Found ", length(text_map), " transcripts.\n\n")))

cat(crayon::blue(paste0(
  "Running pilot: ", length(selected_dates),
  " dates x 10 seeds (", length(selected_dates) * 10, " API calls total)\n\n"
)))

for (conf_date in as.character(selected_dates)) {

  if (!conf_date %in% names(text_map)) {
    cat(crayon::red(paste0("Transcript not found for ", conf_date, " — skipping.\n")))
    next
  }

  conf_text   <- readtext::readtext(text_map[[conf_date]])$text
  full_prompt <- gsub("\\[date\\]", conf_date, prompt_template)
  full_prompt <- paste0(
    full_prompt,
    "Press Conference on ", conf_date, "\n",
    "Text:", conf_text, "\n\n"
  )

  for (run_id in 1:10) {
    out_path <- sprintf("../intermediate_data/micro_pilot/runs/%s_%02d.rds",
                        conf_date, run_id)

    if (file.exists(out_path)) {
      cat(crayon::yellow(paste0("  Skip ", conf_date, " run ", run_id,
                                " (exists)\n")))
      next
    }

    cat(crayon::yellow(paste0("  ", conf_date, " — seed ", run_id, "...\n")))

    response <- call_openrouter(full_prompt, seed = run_id)

    if (!is.null(response)) {
      saveRDS(response, out_path)
      cat(crayon::green(paste0("  Saved: ", basename(out_path), "\n")))
    } else {
      cat(crayon::red(paste0("  FAILED: ", conf_date, " run ", run_id, "\n")))
    }
  }
}

cat(crayon::green("\nAll API calls complete.\n\n"))

# ==============================================================================
# 6. PARSE
# ==============================================================================

cat(crayon::blue("Parsing raw responses...\n"))

#' Parse one markdown-table LLM response into a tibble
#'
#' @param response  Raw character string from LLM
#' @param conf_date Conference date string YYYY-MM-DD
#' @param run_id    Integer run index
#'
#' @return tibble with columns date/run/agent_id/tenor/direction/rate/confidence
parse_response <- function(response, conf_date, run_id) {
  tryCatch({
    if (is.null(response) || nchar(response) == 0) return(NULL)

    lines      <- strsplit(response, "\n")[[1]]
    data_lines <- lines[grepl("\\d{4}-\\d{2}-\\d{2}", lines)]

    if (length(data_lines) == 0) return(NULL)

    rows <- lapply(data_lines, function(line) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      parts <- parts[parts != ""]
      if (length(parts) >= 6) {
        data.frame(
          date       = conf_date,
          run        = run_id,
          agent_id   = parts[2],
          tenor      = parts[3],
          direction  = parts[4],
          rate       = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", parts[5]))),
          confidence = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", parts[6]))),
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })

    bind_rows(rows) %>%
      filter(tenor %in% c("3M", "2Y", "10Y"), !is.na(rate)) %>%
      as_tibble()

  }, error = function(e) NULL)
}

run_files <- list.files(
  "../intermediate_data/micro_pilot/runs",
  pattern = "\\.rds$",
  full.names = TRUE
)

parsed_list <- lapply(run_files, function(f) {
  stem  <- tools::file_path_sans_ext(basename(f))
  parts <- str_match(stem, "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)$")
  if (is.na(parts[1, 1])) return(NULL)
  parse_response(readRDS(f), parts[1, 2], as.integer(parts[1, 3]))
})

parsed_df <- bind_rows(parsed_list)

cat(crayon::green(paste0(
  "Parsed ", nrow(parsed_df), " rows from ", length(run_files), " files",
  " (", n_distinct(parsed_df$date), " dates, ",
  n_distinct(parsed_df$run), " runs, ",
  n_distinct(parsed_df$tenor), " tenors).\n\n"
)))

# ==============================================================================
# 7. DIAGNOSTICS
# ==============================================================================

cat(crayon::blue("Computing diagnostics...\n"))

# Step 1: within-run SD — SD of rate across 30 agents per (date, run, tenor)
within_run_sd_df <- parsed_df %>%
  group_by(date, run, tenor) %>%
  summarise(
    n_agents      = n(),
    within_run_sd = sd(rate, na.rm = TRUE),
    mean_rate     = mean(rate, na.rm = TRUE),
    .groups       = "drop"
  )

# Step 2: per (date, tenor) — mean and variance of within-run SDs across 10 runs
date_tenor_stats <- within_run_sd_df %>%
  group_by(date, tenor) %>%
  summarise(
    n_runs             = n(),
    mean_within_run_sd = mean(within_run_sd, na.rm = TRUE),
    var_within_run_sd  = var(within_run_sd,  na.rm = TRUE),
    sd_within_run_sd   = sd(within_run_sd,   na.rm = TRUE),
    .groups            = "drop"
  )

# Step 3: per tenor — sigma_between and sigma_within_pooled across 5 dates
reliability_params <- date_tenor_stats %>%
  group_by(tenor) %>%
  summarise(
    sigma_between       = sd(mean_within_run_sd, na.rm = TRUE),
    sigma_within_pooled = sqrt(mean(var_within_run_sd, na.rm = TRUE)),
    .groups             = "drop"
  )

# Step 4: G(R) for R = 1..20 per tenor
g_df <- crossing(
  reliability_params,
  R = 1:20
) %>%
  mutate(
    G_R = sigma_between^2 /
      (sigma_between^2 + sigma_within_pooled^2 / R)
  ) %>%
  select(tenor, R, G_R, sigma_between, sigma_within_pooled)

cat(crayon::green("Diagnostics complete.\n\n"))
cat("Reliability parameters by tenor:\n")
print(reliability_params)
cat("\nG(R=1) and G(R=10) by tenor:\n")
print(
  g_df %>%
    filter(R %in% c(1, 5, 10)) %>%
    select(tenor, R, G_R) %>%
    pivot_wider(names_from = R, values_from = G_R,
                names_prefix = "G_R")
)

# ==============================================================================
# 8. OUTPUTS
# ==============================================================================

cat(crayon::blue("\nWriting outputs...\n"))

# Sheet (a): per-(date, run, tenor) within-run SD
sheet_a <- within_run_sd_df %>%
  arrange(date, tenor, run)

# Sheet (b): G(R) reliability table, R = 1..20 per tenor
sheet_b <- g_df %>%
  arrange(tenor, R)

writexl::write_xlsx(
  list("within_run_sd" = sheet_a, "reliability_G_R" = sheet_b),
  path = "../output/micro_pilot_diagnostics.xlsx"
)
cat(crayon::green("Saved: output/micro_pilot_diagnostics.xlsx\n"))

# --- G(R) reliability figure --------------------------------------------------
color_palette <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

p_reliability <- g_df %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) %>%
  ggplot(aes(x = R, y = G_R, color = tenor)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "grey50",
             linewidth = 0.6) +
  geom_hline(yintercept = 0.90, linetype = "dotted", color = "grey50",
             linewidth = 0.6) +
  annotate("text", x = 20.5, y = 0.80, label = "0.80", size = 3,
           color = "grey40", hjust = 0) +
  annotate("text", x = 20.5, y = 0.90, label = "0.90", size = 3,
           color = "grey40", hjust = 0) +
  scale_color_manual(values = color_palette) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), limits = c(1, 22)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title    = "Ensemble Reliability G(R) by Tenor",
    subtitle = paste0(
      length(selected_dates), " conferences × 10 seeds | ",
      "Gemini 2.5-Flash | temperature = 1"
    ),
    x       = "Number of runs averaged (R)",
    y       = "G(R)",
    color   = "Tenor",
    caption = "Dashed = 0.80 threshold; dotted = 0.90 threshold"
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 10, color = "grey40"),
    legend.position = "bottom"
  )

ggsave("../output/micro_pilot_reliability.pdf", p_reliability,
       width = 8, height = 6, dpi = 320)
cat(crayon::green("Saved: output/micro_pilot_reliability.pdf\n\n"))

cat("\n", strrep("=", 80), "\n")
cat(crayon::green("MICRO PILOT P1 COMPLETE\n"))
cat(strrep("=", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
