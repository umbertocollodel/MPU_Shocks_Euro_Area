# ============================================================================
# BOOTSTRAP AND MULTIPLE CORRELATION ANALYSIS — ENSEMBLE (R=10) DISAGREEMENT
# ============================================================================
# Replicates 12run_bootstrap_robustness.R using the ensemble-averaged
# disagreement (sd_mean) from the R=10 full ensemble (post_zero_shot_p1)
# instead of the single-draw LLM SD.
#
# Inputs:
#   ../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds
#       (falls back to inline parsing from full_ensemble_p1/runs/ if absent)
#   ../intermediate_data/range_difference_df.rds
#
# Outputs:
#   ../output/figures/p1/p1_ensemble_3day_vol_corr_ordered.pdf/.png
#   ../output/figures/p1/p1_ensemble_vol_windows_spearman_facet_ordered.pdf/.png
#
# Run from the code/ directory.
# ============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, writexl, readr, boot, RColorBrewer, glue)

if (!file.exists("../raw_data") || !file.exists("../intermediate_data")) {
  stop("Please run this script from the 'code/' directory.\n",
       "Current directory: ", getwd())
}

fig_dir <- "../output/figures/p1"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

tenor_levels <- c("3M", "2Y", "10Y")

# --------------------------------------------------------------------------
# 1) LOAD ENSEMBLE DATA  (mirrors post_zero_shot_p1.R §2)
# --------------------------------------------------------------------------

all_runs_path <- "../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds"
runs_dir_path <- "../intermediate_data/full_ensemble_p1/runs"

if (!file.exists(all_runs_path)) {

  cat("all_runs_long.rds not found — parsing inline from runs/ directory...\n")
  run_files <- list.files(runs_dir_path, pattern = "\\.rds$", full.names = TRUE)
  cat(glue("  Found {length(run_files)} run files.\n"))
  if (length(run_files) == 0)
    stop(glue("No .rds files in {runs_dir_path}. Run run_full_ensemble_p1.R first."))

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
      response %>%
        readr::read_delim(delim = "|", trim_ws = TRUE, skip = 1,
                          show_col_types = FALSE, name_repair = "minimal") %>%
        select(-1, -ncol(.)) %>%
        slice(-nrow(.)) %>%
        setNames(names_col) %>%
        slice(-1) %>%
        mutate(date = as.character(date),
               across(contains("rate"),       as.numeric),
               across(contains("confidence"), as.numeric)) %>%
        filter(tenor %in% c("3M", "2Y", "10Y")) %>%
        mutate(date = conf_date, run = run_id, agent_id = id) %>%
        select(date, run, agent_id, tenor, direction, rate, confidence)
    }, error = function(e) NULL)
  }

  parsed_list  <- map(run_files, parse_one_run)
  all_runs_raw <- keep(parsed_list, ~ !is.null(.x) && nrow(.x) > 0) %>% bind_rows()
  dir.create(dirname(all_runs_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(all_runs_raw, all_runs_path)
  writexl::write_xlsx(all_runs_raw,
                      sub("\\.rds$", ".xlsx", all_runs_path))
  cat(glue("  Parsed {nrow(all_runs_raw)} rows; saved to {all_runs_path}\n"))
  all_runs_raw_loaded <- all_runs_raw

} else {
  cat(glue("  Loading {all_runs_path}\n"))
  all_runs_raw_loaded <- readRDS(all_runs_path)
}

all_runs <- all_runs_raw_loaded %>%
  mutate(
    date  = as.Date(date),
    tenor = factor(if_else(tenor == "3mnt", "3M", tenor), levels = tenor_levels)
  ) %>%
  filter(tenor %in% tenor_levels)

cat(glue(
  "all_runs: {nrow(all_runs)} rows | {n_distinct(all_runs$date)} dates | ",
  "{n_distinct(all_runs$run)} runs | {n_distinct(all_runs$tenor)} tenors\n"
))

# --------------------------------------------------------------------------
# 2) COMPUTE ENSEMBLE-AVERAGED DISAGREEMENT  (mirrors post_zero_shot_p1.R §3)
# --------------------------------------------------------------------------

within_run <- all_runs %>%
  group_by(date, run, tenor) %>%
  summarise(within_sd = sd(rate, na.rm = TRUE), .groups = "drop")

ensemble <- within_run %>%
  group_by(date, tenor) %>%
  summarise(sd_mean = mean(within_sd, na.rm = TRUE), .groups = "drop")

cat(glue(
  "ensemble: {nrow(ensemble)} rows across {n_distinct(ensemble$date)} dates\n"
))

# --------------------------------------------------------------------------
# 3) LOAD MARKET VOLATILITY — three windows  (mirrors script 12 §2)
# --------------------------------------------------------------------------

market_vol_data <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date,
         vol_3d = correct_post_mean_3,
         vol_2d = correct_post_mean_2,
         vol_1d = correct_post_mean_1)

analysis_data <- ensemble %>%
  inner_join(market_vol_data, by = c("date", "tenor")) %>%
  filter(!is.na(sd_mean))

cat(glue(
  "Analysis dataset: {nrow(analysis_data)} obs | ",
  "{n_distinct(analysis_data$date)} unique dates\n"
))

# --------------------------------------------------------------------------
# 4) BOOTSTRAP FUNCTIONS  (identical to script 12)
# --------------------------------------------------------------------------

bootstrap_correlation <- function(data, indices, x_var, y_var, method) {
  sample_data <- data[indices, ]
  cor(sample_data[[x_var]], sample_data[[y_var]],
      method = method, use = "complete.obs")
}

compute_bootstrap_ci <- function(data, tenor_name, method, vol_var,
                                 n_bootstrap = 5000) {
  tenor_data <- data %>% filter(tenor == tenor_name)
  method <- as.character(method)

  if (nrow(tenor_data) < 10) {
    return(data.frame(
      tenor = tenor_name, method = method, vol_var = vol_var,
      n_obs = nrow(tenor_data),
      correlation = NA, ci_lower = NA, ci_upper = NA
    ))
  }

  original_corr <- cor(tenor_data$sd_mean, tenor_data[[vol_var]],
                       method = method, use = "complete.obs")

  boot_result <- boot(
    data      = tenor_data,
    statistic = bootstrap_correlation,
    R         = n_bootstrap,
    x_var     = "sd_mean",
    y_var     = vol_var,
    method    = method
  )

  ci <- boot.ci(boot_result, type = "perc", conf = 0.95)

  data.frame(
    tenor       = tenor_name,
    method      = method,
    vol_var     = vol_var,
    n_obs       = nrow(tenor_data),
    correlation = original_corr,
    ci_lower    = ci$percent[4],
    ci_upper    = ci$percent[5]
  )
}

# --------------------------------------------------------------------------
# 5) PARAMETERS  (identical to script 12)
# --------------------------------------------------------------------------

methods  <- c("spearman", "pearson", "kendall")
tenors   <- c("3M", "2Y", "10Y")
vol_vars <- c("vol_3d", "vol_2d", "vol_1d")

color_palette_methods <- c(
  "spearman" = "#d73027",
  "pearson"  = "#4575b4",
  "kendall"  = "#91bfdb"
)

# ============================================================================
# PLOT 1: BASELINE — 3-DAY VOLATILITY, ALL METHODS, TENOR ORDERED
# ============================================================================

cat("Computing baseline bootstrap results (1-day window, all methods)...\n")

baseline_results <- expand.grid(tenor = tenors, method = methods,
                                stringsAsFactors = FALSE) %>%
  pmap_dfr(~ compute_bootstrap_ci(analysis_data, ..1, ..2, vol_var = "vol_1d"))

baseline_results$tenor <- factor(baseline_results$tenor, levels = c("3M", "2Y", "10Y"))

p_baseline <- ggplot(baseline_results,
                     aes(x = tenor, y = correlation, fill = method)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, group = method),
                position = position_dodge(width = 0.7), width = 0.2, linewidth = 0.4) +
  scale_fill_manual(
    values = color_palette_methods,
    labels = c(expression("Spearman's " ~ rho),
               expression("Pearson's " ~ r),
               expression("Kendall's " ~ tau))
  ) +
  ylim(0,0.8) +
  labs(x = "Tenor", y = "Correlation", fill = "Method") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title    = element_text(size = 18),
    axis.text     = element_text(size = 16),
    legend.title  = element_text(size = 18),
    legend.text   = element_text(size = 16),
    legend.position = "bottom"
  )

for (ext in c("pdf", "png")) {
  ggsave(
    file.path(fig_dir, paste0("p1_ensemble_1day_vol_corr_ordered.", ext)),
    plot = p_baseline, dpi = 320, width = 10, height = 6, bg = "white"
  )
}
cat("  Saved: p1_ensemble_3day_vol_corr_ordered.pdf/.png\n")

# ============================================================================
# PLOT 2: WINDOW COMPARISON — SPEARMAN ONLY, FACET BY TENOR
# ============================================================================

cat("Computing window comparison results (Spearman, all windows)...\n")

window_results_spearman <- expand.grid(tenor = tenors, method = "spearman",
                                       vol_var = vol_vars,
                                       stringsAsFactors = FALSE) %>%
  pmap_dfr(~ compute_bootstrap_ci(analysis_data, ..1, ..2, ..3))

window_results_spearman <- window_results_spearman %>%
  mutate(window_label = case_when(
    vol_var == "vol_1d" ~ "1-Day",
    vol_var == "vol_2d" ~ "2-Day",
    vol_var == "vol_3d" ~ "3-Day"
  ))

window_results_spearman$window_label <- factor(
  window_results_spearman$window_label, levels = c("1-Day", "2-Day", "3-Day"))
window_results_spearman$tenor <- factor(
  window_results_spearman$tenor, levels = c("3M", "2Y", "10Y"))

tenor_colors <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

p_windows <- ggplot(window_results_spearman,
                    aes(x = window_label, y = correlation, fill = tenor)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, linewidth = 0.4, color = "black") +
  facet_wrap(~ tenor, nrow = 1) +
  scale_fill_manual(values = tenor_colors) +
  labs(x = "Volatility Window", y = "Spearman Correlation", fill = "Tenor") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title    = element_text(size = 18),
    axis.text.x   = element_text(size = 16, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y   = element_text(size = 16),
    legend.title  = element_text(size = 18),
    legend.text   = element_text(size = 16),
    legend.position = "bottom",
    strip.text    = element_text(size = 16)
  )

for (ext in c("pdf", "png")) {
  ggsave(
    file.path(fig_dir, paste0("p1_ensemble_vol_windows_spearman_facet_ordered.", ext)),
    plot = p_windows, dpi = 320, width = 12, height = 6, bg = "white"
  )
}
cat("  Saved: p1_ensemble_vol_windows_spearman_facet_ordered.pdf/.png\n")

cat("\nDone.\n")
