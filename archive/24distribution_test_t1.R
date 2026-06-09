#===============================================================================
# SCRIPT 24: DISTRIBUTION TEST FOR QWEN3 t=1 CORRELATIONS ACROSS SEEDS
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Run Qwen3 t=1 with multiple seeds on a representative subset of conferences
#   to characterise the sampling distribution of Spearman correlations with
#   market MPU. A tight, centred distribution means temperature noise is small
#   relative to the cross-agent signal; the original result is not a lucky draw.
#
#   The existing seed=120 run (Script 21) is treated as one observation from
#   this distribution and is included automatically.
#
# Design:
#   - 20 conferences sampled to span the full range of market MPU (5 quantile
#     bins × 4 conferences each), so the subset is representative.
#   - 4 additional seeds: 42, 100, 200, 300  (plus seed=120 already done)
#   - Total new API calls: 20 × 4 = 80. Existing run contributes 20 for free.
#
# Outputs:
#   ../intermediate_data/distribution_test/seed_<seed>/YYYY-MM-DD.rds
#   ../output/figures/distribution_test/
#     correlation_distribution.pdf   — histogram of ρ across seeds, per tenor
#     seed_vs_seed_scatter.pdf        — pairwise scatter of synthetic SDs
#
# Cost: ~10-15% of a full Qwen3 run (80 API calls on 20 conferences)
#
# Usage:
#   source("24distribution_test_t1.R")
#   # Then call step by step or run full pipeline:
#   run_distribution_test()     # Step 1: API calls
#   analyze_distribution_test() # Step 2: Parse + plot (no API calls)
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("DISTRIBUTION TEST — Qwen3 t=1 correlation stability across seeds\n")
cat(strrep("=", 80), "\n\n")

# Packages ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  httr2, readtext, crayon, stringr, purrr,
  tidyverse, boot, showtext, patchwork, future, furrr
)

# Font -------------------------------------------------------------------------
if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) font_add("Segoe UI", regular = font_path)
}
showtext_auto()

# Configuration ----------------------------------------------------------------
TEMPERATURE      <- 1
MODEL            <- "qwen/qwen3-235b-a22b-2507"
ADDITIONAL_SEEDS <- c(42, 100, 200, 300)   # seed=120 already exists
N_SUBSET         <- 20                      # conferences to run
N_QUANTILE_BINS  <- 5                       # stratification bins
PARALLEL_WORKERS <- 4
MAX_ATTEMPTS     <- 5
N_BOOTSTRAP      <- 1500
CI_LEVEL         <- 0.88

original_dir   <- "../intermediate_data/openrouter_result/qwen3_naive"
base_output    <- "../intermediate_data/distribution_test"
figures_dir    <- "../output/figures/distribution_test"
texts_dir      <- "../intermediate_data/texts"
mpu_path       <- "../intermediate_data/range_difference_df.rds"

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

color_palette <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

theme_paper <- theme_minimal(base_family = "Segoe UI") +
  theme(
    axis.title.x       = element_text(margin = margin(t = 10), size = 14),
    axis.title.y       = element_text(margin = margin(r = 10), size = 14),
    axis.text          = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# API key ----------------------------------------------------------------------
api_key <- Sys.getenv("OPENROUTER_API_KEY")
if (api_key == "") stop("OPENROUTER_API_KEY not set in .Renviron.")

source("config/prompts.R")
source("src/llm_api/openrouter_api.R")

#===============================================================================
# HELPERS
#===============================================================================

#' Parse a single markdown-table response into a tibble
parse_llm_response <- function(response, conf_date) {
  tryCatch({
    if (is.null(response) || nchar(response) == 0) return(NULL)
    lines      <- strsplit(response, "\n")[[1]]
    data_lines <- lines[grepl("\\d{4}-\\d{2}-\\d{2}", lines)]
    if (length(data_lines) == 0) return(NULL)

    parsed_list <- lapply(data_lines, function(line) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      parts <- parts[parts != ""]
      if (length(parts) >= 6) {
        data.frame(
          date      = parts[1],
          id        = parts[2],
          tenor     = parts[3],
          direction = parts[4],
          rate      = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", parts[5]))),
          stringsAsFactors = FALSE
        )
      } else NULL
    })

    bind_rows(parsed_list) %>%
      filter(tenor %in% c("3M", "2Y", "10Y"), !is.na(rate)) %>%
      mutate(date = as.Date(conf_date)) %>%
      as_tibble()

  }, error = function(e) NULL)
}

#' Compute synthetic SD from parsed rows
compute_sd <- function(parsed_df) {
  parsed_df %>%
    group_by(date, tenor) %>%
    summarise(std_rate = sd(rate, na.rm = TRUE), n_traders = n(),
              .groups = "drop")
}

#' Bootstrap Spearman correlation with CI
boot_spearman <- function(data, indices) {
  d <- data[indices, ]
  cor(d$std_rate, d$correct_post_mean, method = "spearman", use = "complete.obs")
}

compute_spearman_ci <- function(df, n_boot = N_BOOTSTRAP, conf = CI_LEVEL) {
  df %>%
    group_by(tenor) %>%
    nest() %>%
    mutate(
      point_est = map_dbl(data, ~ cor(.x$std_rate, .x$correct_post_mean,
                                     method = "spearman", use = "complete.obs")),
      boot_obj  = map(data, ~ boot(.x, boot_spearman, R = n_boot)),
      ci        = map(boot_obj, ~ boot.ci(.x, type = "perc", conf = conf)),
      ci_lower  = map_dbl(ci, ~ .x$percent[4]),
      ci_upper  = map_dbl(ci, ~ .x$percent[5])
    ) %>%
    select(tenor, correlation = point_est, ci_lower, ci_upper) %>%
    ungroup() %>%
    mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))
}

#' Load transcript by date
load_transcript <- function(conf_date) {
  files <- list.files(texts_dir, pattern = "\\.txt$", full.names = TRUE)
  match <- files[str_detect(files, conf_date)]
  if (length(match) == 0) stop(paste("Transcript not found:", conf_date))
  readtext::readtext(match[1])$text
}

#===============================================================================
# SELECT REPRESENTATIVE SUBSET
#===============================================================================

#' Select 20 conferences stratified by market MPU at 2Y (most informative tenor)
select_subset <- function() {
  cat(crayon::blue("Selecting representative subset of conferences...\n"))

  range_df <- read_rds(mpu_path) %>%
    mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
    filter(tenor == "2Y") %>%
    select(date, correct_post_mean_1) %>%
    rename(mpu = correct_post_mean_1)

  # Keep only dates with existing t=1 original run
  done <- list.files(original_dir, pattern = "\\.rds$") %>%
    str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
    as.Date()

  range_df <- range_df %>%
    filter(date %in% done, !is.na(mpu)) %>%
    arrange(date)

  # Stratified sample: N_SUBSET conferences spread across N_QUANTILE_BINS MPU bins
  n_per_bin <- N_SUBSET %/% N_QUANTILE_BINS

  subset_dates <- range_df %>%
    mutate(bin = ntile(mpu, N_QUANTILE_BINS)) %>%
    group_by(bin) %>%
    slice_sample(n = n_per_bin, replace = FALSE) %>%
    ungroup() %>%
    arrange(date) %>%
    pull(date) %>%
    as.character()

  cat(crayon::green(paste0("Selected ", length(subset_dates),
                           " conferences across ", N_QUANTILE_BINS,
                           " MPU quantile bins.\n\n")))
  subset_dates
}

#===============================================================================
# STEP 1: API CALLS
#===============================================================================

#' Run all additional seeds on the subset
run_distribution_test <- function(subset_dates = NULL) {

  if (is.null(subset_dates)) subset_dates <- select_subset()

  cat(crayon::blue("===================================================\n"))
  cat(crayon::blue("STEP 1: API CALLS\n"))
  cat(crayon::blue("===================================================\n\n"))
  cat(paste0("Conferences in subset: ", length(subset_dates), "\n"))
  cat(paste0("Additional seeds: ", paste(ADDITIONAL_SEEDS, collapse = ", "), "\n"))
  cat(paste0("Total new API calls: ",
             length(subset_dates) * length(ADDITIONAL_SEEDS), "\n\n"))

  # Save subset for later use by analyze step
  dir.create(base_output, recursive = TRUE, showWarnings = FALSE)
  saveRDS(subset_dates,
          file.path(base_output, "subset_dates.rds"))

  plan(multisession, workers = PARALLEL_WORKERS)

  for (seed in ADDITIONAL_SEEDS) {
    seed_dir <- file.path(base_output, paste0("seed_", seed))
    dir.create(seed_dir, recursive = TRUE, showWarnings = FALSE)

    already_done <- list.files(seed_dir, pattern = "\\.rds$") %>%
      str_extract("\\d{4}-\\d{2}-\\d{2}")
    to_run <- subset_dates[!subset_dates %in% already_done]

    if (length(to_run) == 0) {
      cat(crayon::green(paste0("Seed ", seed, ": all conferences already done.\n")))
      next
    }

    cat(crayon::cyan(paste0("\nRunning seed = ", seed,
                            " (", length(to_run), " conferences)...\n")))

    texts_to_run <- map(to_run, load_transcript)

    future_map2(
      to_run, texts_to_run,
      ~ process_single_conference_openrouter(
        conf_date       = .x,
        conf_text       = .y,
        prompt_template = prompt_naive,
        output_dir      = seed_dir,
        log_file_path   = file.path(base_output,
                                    paste0("errors_seed_", seed, ".log")),
        model           = MODEL,
        seed            = seed,
        temperature     = TEMPERATURE,
        max_attempts    = MAX_ATTEMPTS
      ),
      .options = furrr_options(seed = TRUE)
    )

    cat(crayon::green(paste0("Seed ", seed, " complete.\n")))
  }

  plan(sequential)
  cat(crayon::green("\nAll seeds complete. Run analyze_distribution_test() next.\n\n"))
}

#===============================================================================
# STEP 2: PARSE + ANALYZE + PLOT
#===============================================================================

#' Parse one seed directory and return combined + market joined tibble
parse_seed_dir <- function(seed_dir, subset_dates, range_df) {
  rds_files <- list.files(seed_dir, pattern = "\\.rds$", full.names = TRUE)
  rds_files <- rds_files[str_extract(basename(rds_files),
                                     "\\d{4}-\\d{2}-\\d{2}") %in% subset_dates]

  dates <- str_extract(basename(rds_files), "\\d{4}-\\d{2}-\\d{2}")

  parsed <- map2(rds_files, dates, function(f, d) {
    resp <- tryCatch(readRDS(f), error = function(e) NULL)
    parse_llm_response(resp, d)
  }) %>% compact() %>% bind_rows()

  if (nrow(parsed) == 0) return(NULL)

  compute_sd(parsed) %>%
    inner_join(range_df, by = c("date", "tenor"))
}

#' Full analysis and figures — no API calls required
analyze_distribution_test <- function() {

  cat(crayon::blue("===================================================\n"))
  cat(crayon::blue("STEP 2: ANALYSIS AND FIGURES\n"))
  cat(crayon::blue("===================================================\n\n"))

  # Load subset dates
  subset_path <- file.path(base_output, "subset_dates.rds")
  if (!file.exists(subset_path))
    stop("subset_dates.rds not found. Run run_distribution_test() first.")
  subset_dates <- readRDS(subset_path)

  # Load market data
  range_df <- read_rds(mpu_path) %>%
    mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
    select(tenor, date, correct_post_mean_1) %>%
    rename(correct_post_mean = correct_post_mean_1) %>%
    filter(tenor %in% c("3M", "2Y", "10Y"))

  # ── Collect all seeds ────────────────────────────────────────────────────────
  # Seed 120: extract subset from the full original run
  cat(crayon::blue("Loading seed=120 (original run, subset)...\n"))
  orig_files <- list.files(original_dir, pattern = "\\.rds$", full.names = TRUE)
  orig_dates <- str_extract(basename(orig_files), "\\d{4}-\\d{2}-\\d{2}")
  orig_sub   <- orig_files[orig_dates %in% subset_dates]
  orig_dates_sub <- orig_dates[orig_dates %in% subset_dates]

  parsed_120 <- map2(orig_sub, orig_dates_sub, function(f, d) {
    resp <- tryCatch(readRDS(f), error = function(e) NULL)
    parse_llm_response(resp, d)
  }) %>% compact() %>% bind_rows()

  combined_120 <- compute_sd(parsed_120) %>%
    inner_join(range_df, by = c("date", "tenor"))

  all_seeds <- list("120" = combined_120)

  # Additional seeds
  for (seed in ADDITIONAL_SEEDS) {
    seed_dir <- file.path(base_output, paste0("seed_", seed))
    if (!dir.exists(seed_dir)) {
      cat(crayon::yellow(paste0("Seed ", seed, " directory not found — skipping.\n")))
      next
    }
    cat(crayon::blue(paste0("Loading seed=", seed, "...\n")))
    df <- parse_seed_dir(seed_dir, subset_dates, range_df)
    if (!is.null(df)) all_seeds[[as.character(seed)]] <- df
  }

  seeds_available <- names(all_seeds)
  cat(crayon::green(paste0("\nSeeds loaded: ", paste(seeds_available, collapse = ", "), "\n\n")))

  # ── Spearman correlation per seed ────────────────────────────────────────────
  set.seed(42)
  corr_by_seed <- map_dfr(seeds_available, function(s) {
    compute_spearman_ci(all_seeds[[s]]) %>%
      mutate(seed = as.integer(s))
  })

  cat(strrep("=", 60), "\n")
  cat("SPEARMAN CORRELATIONS BY SEED\n")
  cat(strrep("=", 60), "\n")
  print(corr_by_seed %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

  # Summary: mean and SD of correlation across seeds per tenor
  stability <- corr_by_seed %>%
    group_by(tenor) %>%
    summarise(
      mean_corr = mean(correlation),
      sd_corr   = sd(correlation),
      min_corr  = min(correlation),
      max_corr  = max(correlation),
      n_seeds   = n(),
      .groups = "drop"
    )

  cat("\n", strrep("=", 60), "\n")
  cat("STABILITY SUMMARY (across seeds)\n")
  cat(strrep("=", 60), "\n")
  print(stability %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

  # Verdict
  max_sd <- max(stability$sd_corr, na.rm = TRUE)
  cat("\n", strrep("=", 60), "\n")
  cat("VERDICT\n")
  cat(strrep("=", 60), "\n\n")

  if (max_sd < 0.05) {
    cat(crayon::green(
      "ROBUST: SD of correlation across seeds < 0.05 for all tenors.\n"
    ))
    cat("Temperature noise is small. The t=1 result is stable and publishable.\n")
  } else if (max_sd < 0.10) {
    cat(crayon::yellow(
      "MODERATE: SD of correlation across seeds between 0.05 and 0.10.\n"
    ))
    cat("Report the distribution in the paper (mean ± SD across seeds).\n")
  } else {
    cat(crayon::red(
      "FRAGILE: SD of correlation across seeds > 0.10.\n"
    ))
    cat("Temperature noise is large. Consider averaging across seeds or\n")
    cat("reporting t=0 as the primary result.\n")
  }

  # ── Figure 1: Distribution of correlations across seeds ─────────────────────
  # Highlight seed=120 (original result)
  corr_plot <- corr_by_seed %>%
    mutate(is_original = seed == 120)

  p1 <- ggplot(corr_plot, aes(x = correlation, fill = tenor)) +
    geom_histogram(bins = max(length(seeds_available), 5),
                   alpha = 0.75, color = "white") +
    geom_vline(
      data = corr_plot %>% filter(is_original),
      aes(xintercept = correlation, color = tenor),
      linetype = "dashed", linewidth = 0.9, show.legend = FALSE
    ) +
    geom_vline(
      data = stability %>%
        mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))),
      aes(xintercept = mean_corr, color = tenor),
      linetype = "solid", linewidth = 1.1, show.legend = FALSE
    ) +
    scale_fill_manual(values  = color_palette, guide = "none") +
    scale_color_manual(values = color_palette) +
    facet_wrap(~ tenor, ncol = 3, scales = "free_y") +
    labs(
      x        = "Spearman Correlation vs Market",
      y        = "Count",
      caption  = paste0("Dashed = seed 120 (original). Solid = mean across seeds.\n",
                        "N = ", length(seeds_available), " seeds, ",
                        length(subset_dates), " conferences.")
    ) +
    theme_paper +
    theme(strip.text = element_text(size = 13, face = "bold"))

  ggsave(file.path(figures_dir, "correlation_distribution.pdf"),
         p1, dpi = 320, width = 12, height = 5, bg = "white")
  cat(crayon::green("\nSaved: correlation_distribution.pdf\n"))

  # ── Figure 2: Pairwise scatter of synthetic SDs (seed 120 vs each other) ────
  if (length(all_seeds) >= 2) {
    sd_120 <- all_seeds[["120"]] %>%
      select(date, tenor, std_rate) %>%
      rename(sd_120 = std_rate)

    scatter_list <- map(
      seeds_available[seeds_available != "120"],
      function(s) {
        all_seeds[[s]] %>%
          select(date, tenor, std_rate) %>%
          rename(sd_other = std_rate) %>%
          inner_join(sd_120, by = c("date", "tenor")) %>%
          mutate(seed_other = s,
                 tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))
      }
    ) %>% bind_rows()

    rho_labels <- scatter_list %>%
      group_by(seed_other, tenor) %>%
      summarise(
        r = cor(sd_120, sd_other, method = "spearman", use = "complete.obs"),
        .groups = "drop"
      ) %>%
      mutate(label = sprintf("rho == %.2f", r))

    p2 <- ggplot(scatter_list,
                 aes(x = sd_120, y = sd_other, color = tenor)) +
      geom_point(alpha = 0.6, size = 1.8, show.legend = FALSE) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "grey50") +
      geom_text(
        data = rho_labels,
        aes(label = label, color = tenor),
        x = -Inf, y = Inf, hjust = -0.15, vjust = 1.5,
        size = 3.5, parse = TRUE, show.legend = FALSE
      ) +
      scale_color_manual(values = color_palette) +
      facet_grid(seed_other ~ tenor, scales = "free",
                 labeller = labeller(seed_other = ~ paste0("seed = ", .x))) +
      labs(
        x = "Synthetic SD  (seed = 120)",
        y = "Synthetic SD  (other seed)"
      ) +
      theme_paper +
      theme(strip.text = element_text(size = 11, face = "bold"))

    ggsave(file.path(figures_dir, "seed_vs_seed_scatter.pdf"),
           p2, dpi = 320,
           width  = 4 * 3,
           height = 3 * length(seeds_available[seeds_available != "120"]) + 1,
           bg = "white")
    cat(crayon::green("Saved: seed_vs_seed_scatter.pdf\n"))
  }

  # Save summary table
  saveRDS(list(corr_by_seed = corr_by_seed, stability = stability),
          file.path(base_output, "distribution_test_results.rds"))

  cat(crayon::blue(paste0("\nAll outputs saved to: ", figures_dir, "\n")))
  cat("\n", strrep("=", 80), "\n\n")

  invisible(list(corr_by_seed = corr_by_seed, stability = stability))
}

#===============================================================================
# CONVENIENCE WRAPPER
#===============================================================================

#' Run full pipeline: API calls + analysis
run_full_distribution_test <- function() {
  subset_dates <- select_subset()
  run_distribution_test(subset_dates)
  analyze_distribution_test()
}

#===============================================================================
# EXECUTION INSTRUCTIONS
#===============================================================================

if (interactive()) {
  cat(crayon::blue("Distribution test ready. Options:\n\n"))
  cat("  Full pipeline (API + analysis):\n")
  cat("    run_full_distribution_test()\n\n")
  cat("  Step by step:\n")
  cat("    subset_dates <- select_subset()      # choose 20 conferences\n")
  cat("    run_distribution_test(subset_dates)  # API calls (~80 requests)\n")
  cat("    analyze_distribution_test()          # parse + plot (free)\n\n")
  cat("  Analysis only (if API calls already done):\n")
  cat("    analyze_distribution_test()\n\n")
}

# Uncomment to run automatically:
run_full_distribution_test()

#===============================================================================
# END OF SCRIPT
#===============================================================================
