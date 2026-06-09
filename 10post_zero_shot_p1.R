#===============================================================================
# POST ZERO-SHOT P1 — Full Ensemble Analysis
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Generates all paper figures and quantitative results for the R=10 ensemble
#   (283 ECB conferences). Mirrors v6 Figures 3, 4, 5 plus reliability G(R)
#   and stabilization. All outputs prefixed "p1_".
#
# Inputs (READ ONLY):
#   ../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds
#   ../intermediate_data/range_difference_df.rds
#   ../intermediate_data/aggregate_gemini_result/prompt_naive/2.5flash_2025-07-21.xlsx
#
# Outputs:
#   output/figures/p1/     p1_fig3_direction, p1_fig4_sd_timeseries,
#                          p1_fig_single_vs_ensemble, p1_fig5_correlations,
#                          p1_fig_correlation_uplift, p1_fig_GR_full,
#                          p1_fig_sd_stabilization  (each as .pdf + .png)
#   intermediate_data/p1/  p1_within_run_sd, p1_ensemble_sd,
#                          p1_single_vs_ensemble, p1_correlations,
#                          p1_correlation_uplift, p1_GR_full  (each as .rds + .xlsx)
#
# Usage: source("post_zero_shot_p1.R")  from code/ directory
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("POST ZERO-SHOT P1 — Full Ensemble Analysis\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# 1. SETUP
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, readxl, writexl, readr, scales, showtext,
  boot, RColorBrewer, glue, sandwich, lmtest, patchwork
)

# Font — same guard pattern as 09plot_llm_results.R
if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) {
    font_add("Segoe UI", regular = font_path)
  } else {
    warning("Segoe UI not found; place segoeui.ttf in code/ directory. Using default.")
  }
}
showtext_auto()

fig_dir  <- "../output/figures/p1"
data_dir <- "../intermediate_data/p1"
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# Hard assert: every planned output filename must start with "p1_"
assert_p1 <- function(path) {
  bn <- basename(path)
  if (!startsWith(bn, "p1_")) {
    stop(glue("Output filename violates p1_ prefix rule: '{bn}'"))
  }
  invisible(path)
}

planned_stems <- c(
  file.path(data_dir, "p1_within_run_sd"),
  file.path(data_dir, "p1_ensemble_sd"),
  file.path(data_dir, "p1_single_vs_ensemble"),
  file.path(data_dir, "p1_correlations"),
  file.path(data_dir, "p1_correlation_uplift"),
  file.path(data_dir, "p1_GR_full"),
  file.path(fig_dir,  "p1_fig3_direction"),
  file.path(fig_dir,  "p1_fig4_sd_timeseries"),
  file.path(fig_dir,  "p1_fig_single_vs_ensemble"),
  file.path(fig_dir,  "p1_fig5_correlations"),
  file.path(fig_dir,  "p1_fig_correlation_uplift"),
  file.path(fig_dir,  "p1_fig_GR_full"),
  file.path(fig_dir,  "p1_fig_sd_stabilization"),
  file.path(data_dir, "p1_comparison_table"),
  file.path(data_dir, "p1_directional_analysis")
)
invisible(lapply(planned_stems, assert_p1))
cat("Prefix assertion passed for all", length(planned_stems), "output paths.\n\n")

# Style constants
tenor_levels  <- c("3M", "2Y", "10Y")
tenor_colours <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

# Shared base theme (spec §12) — each plot adds plot-specific overrides with +theme(...)
p1_theme <- theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title       = element_blank(),
    plot.caption     = element_text(hjust = 0, size = 9, colour = "grey30"),
    strip.text       = element_text(face = "bold"),
    axis.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

# Save helpers: one call writes both PDF and PNG
save_plot <- function(plot, stem, w = 12, h = 8) {
  assert_p1(paste0(stem, ".pdf"))
  for (ext in c("pdf", "png")) {
    ggsave(
      filename = file.path(fig_dir, paste0(stem, ".", ext)),
      plot     = plot,
      dpi      = 320, width = w, height = h, bg = "white"
    )
  }
  cat(glue("  Saved: {stem}.pdf/.png\n"))
  invisible(plot)
}

# Save data: RDS + XLSX
save_table <- function(df, stem) {
  assert_p1(paste0(stem, ".rds"))
  saveRDS(df, file.path(data_dir, paste0(stem, ".rds")))
  writexl::write_xlsx(df, file.path(data_dir, paste0(stem, ".xlsx")))
  cat(glue("  Saved: {stem}.rds/.xlsx\n"))
  invisible(df)
}

# Bootstrap functions — same structure as 12run_bootstrap_robustness.R
bootstrap_correlation <- function(data, indices, x_var, y_var, method) {
  d <- data[indices, ]
  cor(d[[x_var]], d[[y_var]], method = method, use = "complete.obs")
}

compute_bootstrap_ci <- function(data, x_var, y_var, method, n_bootstrap = 5000) {
  if (nrow(data) < 10) {
    return(data.frame(rho = NA_real_, ci_low = NA_real_, ci_high = NA_real_, n = nrow(data)))
  }
  rho <- cor(data[[x_var]], data[[y_var]], method = method, use = "complete.obs")
  boot_res <- boot(data, bootstrap_correlation, R = n_bootstrap,
                   x_var = x_var, y_var = y_var, method = method)
  ci <- boot.ci(boot_res, type = "perc", conf = 0.90)
  data.frame(rho = rho, ci_low = ci$percent[4], ci_high = ci$percent[5], n = nrow(data))
}

cat("Setup complete.\n\n")

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

cat("Loading data...\n")

all_runs_path  <- "../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds"
all_runs_xlsx  <- "../intermediate_data/full_ensemble_p1/parsed/all_runs_long.xlsx"
runs_dir_path  <- "../intermediate_data/full_ensemble_p1/runs"

# Fallback: if all_runs_long.rds is absent, parse directly from runs/ directory.
# This happens when run_full_ensemble_p1.R completed its API calls but the
# n_missing > 0 guard prevented the parsing step from executing.
if (!file.exists(all_runs_path)) {

  cat("all_runs_long.rds not found — parsing inline from runs/ directory...\n")

  run_files <- list.files(runs_dir_path, pattern = "\\.rds$", full.names = TRUE)
  cat(glue("  Found {length(run_files)} run files in runs/.\n"))

  if (length(run_files) == 0) {
    stop(glue(
      "No .rds files found in {runs_dir_path}. ",
      "Run run_full_ensemble_p1.R first to generate the API responses."
    ))
  }

  # Exact parsing logic from 08clean_llm_result.R
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
        filter(tenor %in% c("3M", "2Y", "10Y")) %>%
        mutate(date = conf_date, run = run_id, agent_id = id) %>%
        select(date, run, agent_id, tenor, direction, rate, confidence)

      result
    }, error = function(e) NULL)
  }

  parsed_list  <- map(run_files, parse_one_run)
  all_runs_raw <- keep(parsed_list, ~ !is.null(.x) && nrow(.x) > 0) %>%
    bind_rows()

  n_failed <- length(run_files) - sum(map_lgl(parsed_list, ~ !is.null(.x) && nrow(.x) > 0))

  cat(glue(
    "  Parsed: {nrow(all_runs_raw)} rows from {length(run_files)} files ",
    "({n_failed} failed/empty).\n"
  ))

  dir.create(dirname(all_runs_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(all_runs_raw,  all_runs_path)
  writexl::write_xlsx(all_runs_raw, all_runs_xlsx)
  cat(glue("  Saved: {all_runs_path}\n\n"))

  all_runs_raw_loaded <- all_runs_raw

} else {

  cat(glue("  Found: {all_runs_path}\n\n"))
  all_runs_raw_loaded <- readRDS(all_runs_path)

}

all_runs <- all_runs_raw_loaded %>%
  mutate(
    date  = as.Date(date),
    tenor = factor(if_else(tenor == "3mnt", "3M", tenor), levels = tenor_levels)
  ) %>%
  filter(tenor %in% tenor_levels)

market_vol <- readRDS("../intermediate_data/range_difference_df.rds") %>%
  mutate(
    tenor = if_else(tenor == "3mnt", "3M", tenor),
    tenor = factor(tenor, levels = tenor_levels),
    date  = as.Date(date)
  ) %>%
  select(tenor, date, vol_1d = correct_post_mean_1) %>%
  filter(!is.na(vol_1d), tenor %in% tenor_levels)

cat(glue(
  "all_runs:   {nrow(all_runs)} rows | {n_distinct(all_runs$date)} dates | ",
  "{n_distinct(all_runs$run)} runs | {n_distinct(all_runs$tenor)} tenors\n"
))
cat(glue("market_vol: {nrow(market_vol)} rows | {n_distinct(market_vol$date)} dates\n"))

agents_per_cell <- all_runs %>%
  group_by(date, run, tenor) %>%
  summarise(n = n(), .groups = "drop")
cat(glue(
  "Agents per cell — median: {median(agents_per_cell$n)}, ",
  "min: {min(agents_per_cell$n)}, max: {max(agents_per_cell$n)}\n\n"
))

# ==============================================================================
# 3. ENSEMBLE-AVERAGED DISAGREEMENT
# ==============================================================================

cat("Computing ensemble disagreement...\n")

within_run <- all_runs %>%
  group_by(date, run, tenor) %>%
  summarise(
    within_sd = sd(rate, na.rm = TRUE),
    mean_rate = mean(rate, na.rm = TRUE),
    n_agents  = n(),
    .groups   = "drop"
  )

ensemble <- within_run %>%
  group_by(date, tenor) %>%
  summarise(
    sd_mean   = mean(within_sd, na.rm = TRUE),
    sd_se     = sd(within_sd,   na.rm = TRUE) / sqrt(n()),
    mean_rate = mean(mean_rate, na.rm = TRUE),
    n_runs    = n(),
    .groups   = "drop"
  )

save_table(within_run %>% mutate(across(where(is.factor), as.character)), "p1_within_run_sd")
save_table(ensemble   %>% mutate(across(where(is.factor), as.character)), "p1_ensemble_sd")

cat(glue(
  "within_run: {nrow(within_run)} rows (expect {n_distinct(all_runs$date)*10*3})\n",
  "ensemble:   {nrow(ensemble)} rows (expect {n_distinct(all_runs$date)*3})\n\n"
))

# ==============================================================================
# 4. DIRECTIONAL FORECASTS — Figure 3 equivalent
# ==============================================================================

cat("Figure 3: directional forecasts...\n")

direction_pct <- all_runs %>%
  mutate(
    d = str_to_lower(trimws(as.character(direction))),
    direction_clean = case_when(
      d %in% c("up", "higher", "increase", "upward")    ~ "Up",
      d %in% c("down", "lower", "decrease", "downward") ~ "Down",
      d %in% c("unchanged", "flat", "same", "stable")   ~ "Unchanged",
      TRUE                                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(direction_clean)) %>%
  mutate(direction_clean = factor(direction_clean, levels = c("Up", "Unchanged", "Down"))) %>%
  group_by(date, tenor, direction_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(date, tenor) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

fig3 <- ggplot(direction_pct,
               aes(x = as.factor(date), y = percentage, fill = direction_clean)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  facet_wrap(~ tenor, ncol = 1) +
  scale_fill_manual(
    values = c("Up" = "#d73027", "Unchanged" = "grey70", "Down" = "#4575b4"),
    name   = ""
  ) +
  scale_x_discrete(
    breaks = function(x) x[seq(1, length(x), by = 12)],
    labels = function(x) format(as.Date(x), "%Y-%m")
  ) +
  labs(
    x       = NULL,
    y       = "Percentage (%)",
    caption = ""
  ) +
  p1_theme +
  theme(
    axis.text.x        = element_text(angle = 90, vjust = 0.5, size = 12),
    axis.text.y= element_text(size=12),
    axis.title = element_text(size=14),
    legend.title = element_text(size=16),
    legend.text=element_text(size=14),
    strip.text=element_text(size=16),
    panel.grid.major.x = element_blank(),
    panel.border       = element_rect(colour = "grey80", fill = NA)
  )

save_plot(fig3, "p1_fig3_direction", w = 12, h = 8)

# ==============================================================================
# 5. ENSEMBLE SD TIME SERIES — Figure 4 equivalent
# ==============================================================================

cat("Figure 4: ensemble SD time series...\n")

p95_thresholds <- ensemble %>%
  filter(!(date %in% c("2005-01-20") & tenor == "10Y")) |> 
  group_by(tenor) %>%
  summarise(p95 = quantile(sd_mean, 0.95, na.rm = TRUE), .groups = "drop")

ensemble_ts <- ensemble %>%
  filter(!(date %in% c("2005-01-20") & tenor == "10Y")) |> 
  left_join(p95_thresholds, by = "tenor") %>%
  mutate(
    highlight = sd_mean > p95,
    ribbon_lo = pmax(0, sd_mean - 1.96 * sd_se),
    ribbon_hi = sd_mean + 1.96 * sd_se
  )

main_plot <- ggplot(ensemble_ts, aes(x = date, colour = tenor, fill = tenor)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ribbon_lo, ymax = ribbon_hi),
              colour = NA, alpha = 0.15) +
  geom_line(aes(y = sd_mean), linewidth = 0.8) +
  geom_point(
    data  = filter(ensemble_ts, highlight),
    aes(y = sd_mean), shape = 21, size = 2.5, stroke = 1
  ) +
  facet_wrap(~ tenor, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = tenor_colours, guide = "none") +
  scale_fill_manual(values = tenor_colours, guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x       = NULL,
    y       = "SD of Predicted Rate"
  ) +
  p1_theme +
  theme(
    axis.text.x        = element_text(angle = 90, vjust = 0.5, size = 14),
    axis.text.y= element_text(size=12),
    axis.title = element_text(size=14),
    legend.title = element_text(size=16),
    legend.text=element_text(size=14),
    strip.text=element_text(size=16),
    panel.border       = element_rect(colour = "grey80", fill = NA)
  )
ensemble_ts_zoom <- ensemble_ts %>%
  filter(date >= as.Date("2022-01-01"), date <= as.Date("2024-01-01"))

zoom_plot <- ggplot(ensemble_ts_zoom, aes(x = date, colour = tenor, fill = tenor)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.4) +
  geom_ribbon(aes(ymin = ribbon_lo, ymax = ribbon_hi),
              colour = NA, alpha = 0.15) +
  geom_line(aes(y = sd_mean), linewidth = 1.0) +
  geom_point(
    data  = filter(ensemble_ts_zoom, highlight),
    aes(y = sd_mean), shape = 21, size = 2.5, stroke = 1
  ) +
  facet_wrap(~ tenor, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = tenor_colours, guide = "none") +
  scale_fill_manual(values = tenor_colours, guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title   = "2022–2023",
    x       = NULL,
    y       = NULL,
    caption = ""
  ) +
  p1_theme +
  theme(
    axis.text.x        = element_text(angle = 90, vjust = 0.5, size = 14),
    axis.text.y= element_text(size=12),
    axis.title = element_text(size=16),
    legend.title = element_text(size=16),
    legend.text=element_text(size=14),
    strip.text=element_text(size=16),
    panel.border       = element_rect(colour = "grey80", fill = NA)
  )

fig4 <- main_plot + zoom_plot +
  plot_layout(widths = c(2.5, 1))

save_plot(fig4, "p1_fig4_sd_timeseries", w = 14, h = 9)

# ==============================================================================
# 6. SINGLE-RUN vs ENSEMBLE COMPARISON
# ==============================================================================

cat("Figure: single run vs ensemble...\n")

comparison_df <- within_run %>%
  filter(run == 1) %>%
  select(date, tenor, sd_run1 = within_sd) %>%
  inner_join(select(ensemble, date, tenor, sd_ensemble = sd_mean),
             by = c("date", "tenor"))

save_table(comparison_df %>% mutate(across(where(is.factor), as.character)),
           "p1_single_vs_ensemble")

fig_sve <- ggplot(comparison_df, aes(x = sd_run1, y = sd_ensemble)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey50", linewidth = 0.6) +
  geom_point(aes(colour = tenor), alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey30", linewidth = 0.7) +
  facet_wrap(~ tenor, ncol = 3) +
  scale_colour_manual(values = tenor_colours, guide = "none") +
  labs(
    x       = "Single-draw SD (run = 1)",
    y       = "Ensemble SD (R = 10 mean)",
    caption = "45-degree dashed line = perfect agreement. Solid line = OLS fit."
  ) +
  p1_theme

save_plot(fig_sve, "p1_fig_single_vs_ensemble", w = 12, h = 5)

# ==============================================================================
# 7. CORRELATIONS WITH MARKET VOL — Figure 5 equivalent
# ==============================================================================

cat("Figure 5: correlations with market vol...\n")

corr_data <- ensemble %>%
  inner_join(market_vol, by = c("date", "tenor")) %>%
  filter(!is.na(sd_mean), !is.na(vol_1d))

cat(glue("Merged for correlation: {nrow(corr_data)} obs ({n_distinct(corr_data$date)} dates)\n"))

set.seed(42)

corr_results <- corr_data %>%
  mutate(tenor = as.character(tenor)) %>%
  group_by(tenor) %>%
  nest() %>%
  mutate(
    spearman = map(data, ~ compute_bootstrap_ci(.x, "sd_mean", "vol_1d", "spearman")),
    pearson  = map(data, ~ compute_bootstrap_ci(.x, "sd_mean", "vol_1d", "pearson")),
    kendall  = map(data, ~ compute_bootstrap_ci(.x, "sd_mean", "vol_1d", "kendall"))
  ) %>%
  select(-data) %>%
  pivot_longer(c(spearman, pearson, kendall), names_to = "method", values_to = "ci") %>%
  unnest(ci) %>%
  select(tenor, method, rho, ci_low, ci_high, n) %>%
  ungroup()

save_table(corr_results, "p1_correlations")

spearman_corr <- corr_results %>%
  filter(method == "spearman") %>%
  mutate(tenor = factor(tenor, levels = tenor_levels))

fig5 <- ggplot(spearman_corr, aes(x = tenor, y = rho, fill = tenor)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.15, colour = "grey20", linewidth = 0.8) +
  geom_text(aes(label = round(rho, 2), y = rho + 0.15),
            size = 5, fontface = "bold") +
  scale_fill_manual(values = tenor_colours) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  labs(
    x       = "OIS Tenor",
    y       = "Spearman ρ",
    caption = "") +
  p1_theme +
  theme(panel.grid.major.x = element_blank()) +
    theme(
    axis.text.x        = element_text(size = 14),
    axis.text.y= element_text(size=12),
    axis.title = element_text(size=16),
    legend.title = element_text(size=16),
    legend.text=element_text(size=14),
    strip.text=element_text(size=16),
    panel.border       = element_rect(colour = "grey80", fill = NA)
  )

save_plot(fig5, "p1_fig5_correlations", w = 8, h = 6)

# ==============================================================================
# 8. SIDE-BY-SIDE: V6 SINGLE DRAW vs ENSEMBLE R=10
# ==============================================================================

cat("Figure: correlation uplift (v6 vs ensemble)...\n")

v6_raw <- read_xlsx(
  "../intermediate_data/aggregate_gemini_result/prompt_naive/2.5flash_2025-07-21.xlsx"
)

v6_sd <- v6_raw %>%
  mutate(
    date  = as.Date(as.character(date)),
    tenor = if_else(as.character(tenor) == "3mnt", "3M", as.character(tenor)),
    tenor = factor(tenor, levels = tenor_levels),
    rate  = suppressWarnings(as.numeric(rate))
  ) %>%
  filter(tenor %in% tenor_levels, !is.na(rate)) %>%
  group_by(date, tenor) %>%
  summarise(sd_v6 = sd(rate, na.rm = TRUE), .groups = "drop")

# Common sample across ensemble, market_vol, and v6 for apples-to-apples comparison
uplift_data <- corr_data %>%
  inner_join(v6_sd, by = c("date", "tenor")) %>%
  filter(!is.na(sd_v6))

cat(glue(
  "Uplift comparison: {n_distinct(uplift_data$date)} dates, ",
  "{nrow(uplift_data)} obs\n"
))

set.seed(42)

uplift_results <- uplift_data %>%
  mutate(tenor = as.character(tenor)) %>%
  group_by(tenor) %>%
  nest() %>%
  mutate(
    ensemble = map(data, ~ compute_bootstrap_ci(.x, "sd_mean", "vol_1d", "spearman")),
    v6_draw  = map(data, ~ compute_bootstrap_ci(.x, "sd_v6",   "vol_1d", "spearman"))
  ) %>%
  select(-data) %>%
  pivot_longer(c(ensemble, v6_draw), names_to = "design", values_to = "ci") %>%
  unnest(ci) %>%
  mutate(
    design = recode(design,
      "ensemble" = "Ensemble R=10",
      "v6_draw"  = "Single draw (v6)"
    )
  ) %>%
  select(tenor, design, rho, ci_low, ci_high, n) %>%
  ungroup()

save_table(uplift_results, "p1_correlation_uplift")

fig_uplift <- uplift_results %>%
  mutate(
    tenor  = factor(tenor,  levels = tenor_levels),
    design = factor(design, levels = c("Single draw (v6)", "Ensemble R=10"))
  ) %>%
  ggplot(aes(x = tenor, y = rho, fill = design)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.6),
                width = 0.2, colour = "grey20", linewidth = 0.7) +
  scale_fill_manual(
    values = c("Single draw (v6)" = "grey60", "Ensemble R=10" = "#4575b4"),
    name   = NULL
  ) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 0.8, 0.2)) +
  labs(
    x       = "OIS Tenor",
    y       = "Spearman ρ",
    caption = paste0(
      "Bootstrap 95% CI (5000 reps). Both designs evaluated on the same ",
      "(date, tenor) sample (ensemble ∩ market vol ∩ v6)."
    )
  ) +
  p1_theme +
  theme(panel.grid.major.x = element_blank())

save_plot(fig_uplift, "p1_fig_correlation_uplift", w = 8, h = 6)

# ==============================================================================
# 9. RELIABILITY G(R) — FULL 283-CONFERENCE SAMPLE
# ==============================================================================

cat("Figure: G(R) reliability on full sample...\n")

date_tenor_stats <- within_run %>%
  group_by(date, tenor) %>%
  summarise(
    mean_wr_sd = mean(within_sd, na.rm = TRUE),
    var_wr_sd  = var(within_sd,  na.rm = TRUE),
    n_runs     = n(),
    .groups    = "drop"
  )

reliability_params <- date_tenor_stats %>%
  group_by(tenor) %>%
  summarise(
    sigma_between = sd(mean_wr_sd, na.rm = TRUE),
    sigma_within  = sqrt(mean(var_wr_sd, na.rm = TRUE)),
    .groups       = "drop"
  )

gr_df <- crossing(reliability_params, R = 1:20) %>%
  mutate(G_R = sigma_between^2 / (sigma_between^2 + sigma_within^2 / R)) %>%
  select(tenor, R, G_R, sigma_between, sigma_within) %>%
  mutate(tenor = factor(tenor, levels = tenor_levels))

save_table(gr_df %>% mutate(across(where(is.factor), as.character)), "p1_GR_full")

n_dates_gr <- n_distinct(date_tenor_stats$date)

fig_gr <- ggplot(gr_df, aes(x = R, y = G_R, colour = tenor)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = 10, linetype = "dotdash", colour = "grey40",
             linewidth = 0.6) +
  geom_hline(yintercept = 0.60, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_hline(yintercept = 0.75, linetype = "dotted", colour = "grey50",
             linewidth = 0.5) +
  annotate("text", x = 10.4, y = 0.12, label = "R = 10", size = 3,
           colour = "grey40", hjust = 0) +
  scale_colour_manual(values = tenor_colours, name = "Tenor") +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), limits = c(1, 22)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    x       = "Number of runs averaged (R)",
    y       = "G(R)"
  ) +
  p1_theme

save_plot(fig_gr, "p1_fig_GR_full", w = 8, h = 6)

# ==============================================================================
# 10. SD STABILIZATION ACROSS RUNS — quantile ribbon version
# ==============================================================================
# Shows how the spread of estimates across conferences narrows as R grows.
# At R = 10 every conference is at exactly 1.0 (normalised by its own terminal
# value), so the IQR and 90% range collapse to a single point — convergence is
# self-evident. The interesting question is how quickly this happens.

cat("Figure: SD stabilization (quantile ribbons)...\n")

stab_df <- within_run %>%
  arrange(date, tenor, run) %>%
  group_by(date, tenor) %>%
  mutate(
    cum_mean_sd = cummean(within_sd),
    terminal_sd = last(cum_mean_sd)
  ) %>%
  ungroup() %>%
  mutate(norm_cum_mean = cum_mean_sd / terminal_sd) %>%
  filter(!is.na(norm_cum_mean), is.finite(norm_cum_mean)) %>%
  mutate(tenor = factor(tenor, levels = tenor_levels))

n_conf_stab <- n_distinct(stab_df$date)

# Summarise across conferences at each R
stab_quantiles <- stab_df %>%
  group_by(tenor, run) %>%
  summarise(
    p05 = quantile(norm_cum_mean, 0.05, na.rm = TRUE),
    p25 = quantile(norm_cum_mean, 0.25, na.rm = TRUE),
    p50 = median(norm_cum_mean,   na.rm = TRUE),
    p75 = quantile(norm_cum_mean, 0.75, na.rm = TRUE),
    p95 = quantile(norm_cum_mean, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

fig_stab <- ggplot(stab_quantiles, aes(x = run)) +
  geom_hline(yintercept = 1.00, linetype = "dashed",
             colour = "grey30", linewidth = 0.6) +
  geom_hline(yintercept = c(0.95, 1.05), linetype = "dotted",
             colour = "grey60", linewidth = 0.4) +
  geom_ribbon(aes(ymin = p05, ymax = p95, fill = tenor), alpha = 0.15) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = tenor), alpha = 0.30) +
  geom_line(aes(y = p50, colour = tenor), linewidth = 1.2) +
  facet_wrap(~ tenor, ncol = 3) +
  scale_colour_manual(values = tenor_colours, guide = "none") +
  scale_fill_manual(values = tenor_colours,   guide = "none") +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(
    breaks = c(0.80, 0.90, 0.95, 1.00, 1.05, 1.10, 1.20),
    labels = label_number(accuracy = 0.01)
  ) +
  coord_cartesian(ylim = c(0.75, 1.25)) +
  labs(
    x       = "Runs averaged (R)",
    y       = "Ensemble mean SD / terminal value (R = 10)") +
  p1_theme +
  theme(panel.border = element_rect(colour = "grey80", fill = NA))

save_plot(fig_stab, "p1_fig_sd_stabilization", w = 12, h = 5)

# ==============================================================================
# 11. SANITY OUTPUT (printed only)
# ==============================================================================

cat("\n", strrep("-", 60), "\n")
cat("SANITY OUTPUT\n")
cat(strrep("-", 60), "\n\n")

cat("Mean and median ensemble SD by tenor:\n")
ensemble %>%
  group_by(tenor) %>%
  summarise(
    mean_sd   = round(mean(sd_mean,   na.rm = TRUE), 4),
    median_sd = round(median(sd_mean, na.rm = TRUE), 4),
    .groups   = "drop"
  ) %>%
  print()

cat("\nDates merged with market vol by tenor:\n")
corr_data %>%
  group_by(tenor) %>%
  summarise(n_dates = n_distinct(date), .groups = "drop") %>%
  print()

cat("\nCorrelation table (all methods):\n")
print(corr_results)

cat("\nCorrelation uplift — Δρ (Ensemble R=10 minus Single draw v6):\n")
uplift_results %>%
  select(tenor, design, rho) %>%
  pivot_wider(names_from = design, values_from = rho) %>%
  mutate(delta_rho = round(`Ensemble R=10` - `Single draw (v6)`, 4)) %>%
  print()

# ==============================================================================
# 12. COMPARISON TABLE — text measures + pre-vol baseline + LLM
# ==============================================================================
# Rows: FK Complexity, Word Count, Hedging Words, LM Uncertainty,
#       Net Hawkish-Dovish, Lagged market vol (pre-vol baseline),
#       LLM single draw (v6), LLM ensemble (R = 10)
# Columns: 3M, 2Y, 10Y
# Values: Spearman ρ with market vol (correct_post_mean_1), with significance stars
# All correlations are Spearman; p-values from cor.test.

cat("Comparison table: text + pre-vol + LLM...\n")

# ---- helpers ----
add_stars <- function(p) {
  case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
}
fmt_rho <- function(rho, pval) {
  sprintf("%.3f%s", rho, add_stars(pval))
}

# ---- 1. Text measure correlations (from script 05) ----
text_corr_path <- "../intermediate_data/text_measure_correlations.rds"
if (!file.exists(text_corr_path)) {
  stop(glue(
    "text_measure_correlations.rds not found. ",
    "Run 05calculate_complexity_documents.R first."
  ))
}
text_corr <- readRDS(text_corr_path) %>%
  mutate(tenor = factor(tenor, levels = tenor_levels))

# ---- 2. LLM correlations ----
# Re-use uplift_results from section 8 (ensemble R=10 and single draw v6)
# Both on the common sample (ensemble ∩ market_vol ∩ v6)
llm_corr <- uplift_results %>%
  mutate(tenor = factor(tenor, levels = tenor_levels)) %>%
  select(tenor, design, rho, ci_low, ci_high, n)

# Approximate p-values from bootstrap CIs (CI excludes 0 ↔ p < 0.10 at 90% CI)
# Use cor.test on the raw data instead for proper stars
corr_data_full <- ensemble %>%
  inner_join(market_vol, by = c("date", "tenor")) %>%
  filter(!is.na(sd_mean), !is.na(vol_1d))

llm_pvals <- bind_rows(
  corr_data_full %>%
    filter(date %in% uplift_data$date) %>%
    inner_join(
      v6_sd %>% mutate(tenor = factor(tenor, levels = tenor_levels)),
      by = c("date", "tenor")
    ) %>%
    group_by(tenor) %>%
    summarise(
      pval_ensemble = cor.test(~ sd_mean + vol_1d,
                               data = cur_data(), method = "spearman",
                               exact = FALSE)$p.value,
      pval_v6       = cor.test(~ sd_v6 + vol_1d,
                               data = cur_data(), method = "spearman",
                               exact = FALSE)$p.value,
      .groups = "drop"
    )
) %>%
  mutate(tenor = factor(tenor, levels = tenor_levels))

# ---- 4. Assemble table ----
make_row <- function(label, rho_3m, p_3m, rho_2y, p_2y, rho_10y, p_10y,
                     n_3m = NA, n_2y = NA, n_10y = NA) {
  tibble(
    Measure = label,
    `3M`    = fmt_rho(rho_3m,  p_3m),
    `2Y`    = fmt_rho(rho_2y,  p_2y),
    `10Y`   = fmt_rho(rho_10y, p_10y),
    n_3m = n_3m, n_2y = n_2y, n_10y = n_10y
  )
}

get_tc <- function(var_prefix, tenor_val) {
  row <- text_corr %>% filter(tenor == tenor_val)
  list(
    rho  = row[[paste0(var_prefix, "_spearman")]],
    pval = row[[paste0(var_prefix, "_pval")]]
  )
}

tc <- function(var_prefix) {
  list(
    `3M`  = get_tc(var_prefix, "3M"),
    `2Y`  = get_tc(var_prefix, "2Y"),
    `10Y` = get_tc(var_prefix, "10Y")
  )
}

n_text <- text_corr %>% filter(tenor == "3M") %>% pull(n)

llm_ens <- function(tenor_val) {
  rho  <- uplift_results %>% filter(tenor == tenor_val, design == "Ensemble R=10") %>% pull(rho)
  pval <- llm_pvals %>% filter(tenor == tenor_val) %>% pull(pval_ensemble)
  n    <- uplift_results %>% filter(tenor == tenor_val, design == "Ensemble R=10") %>% pull(n)
  list(rho = rho, pval = pval, n = n)
}
llm_v6 <- function(tenor_val) {
  rho  <- uplift_results %>% filter(tenor == tenor_val, design == "Single draw (v6)") %>% pull(rho)
  pval <- llm_pvals %>% filter(tenor == tenor_val) %>% pull(pval_v6)
  n    <- uplift_results %>% filter(tenor == tenor_val, design == "Single draw (v6)") %>% pull(n)
  list(rho = rho, pval = pval, n = n)
}

fk  <- tc("fk");  wc  <- tc("wc");  hg  <- tc("hedge")
unc <- tc("unc"); hd  <- tc("hd")

comp_table <- bind_rows(
  make_row("FK Complexity",
           fk$`3M`$rho,  fk$`3M`$pval,
           fk$`2Y`$rho,  fk$`2Y`$pval,
           fk$`10Y`$rho, fk$`10Y`$pval,
           n_text, n_text, n_text),
  make_row("Word Count",
           wc$`3M`$rho,  wc$`3M`$pval,
           wc$`2Y`$rho,  wc$`2Y`$pval,
           wc$`10Y`$rho, wc$`10Y`$pval,
           n_text, n_text, n_text),
  make_row("Hedging Words",
           hg$`3M`$rho,  hg$`3M`$pval,
           hg$`2Y`$rho,  hg$`2Y`$pval,
           hg$`10Y`$rho, hg$`10Y`$pval,
           n_text, n_text, n_text),
  make_row("LM Uncertainty Density",
           unc$`3M`$rho,  unc$`3M`$pval,
           unc$`2Y`$rho,  unc$`2Y`$pval,
           unc$`10Y`$rho, unc$`10Y`$pval,
           n_text, n_text, n_text),
  make_row("Net Hawkish-Dovish",
           hd$`3M`$rho,  hd$`3M`$pval,
           hd$`2Y`$rho,  hd$`2Y`$pval,
           hd$`10Y`$rho, hd$`10Y`$pval,
           n_text, n_text, n_text),
  make_row("LLM ensemble (R = 10)",
           llm_ens("3M")$rho,  llm_ens("3M")$pval,
           llm_ens("2Y")$rho,  llm_ens("2Y")$pval,
           llm_ens("10Y")$rho, llm_ens("10Y")$pval,
           llm_ens("3M")$n,    llm_ens("2Y")$n,    llm_ens("10Y")$n)
)

cat("\nComparison table (Spearman ρ with market vol):\n")
print(comp_table %>% select(Measure, `3M`, `2Y`, `10Y`))

# Save as RDS + XLSX
assert_p1(file.path(data_dir, "p1_comparison_table.rds"))
save_table(comp_table %>% mutate(across(where(is.factor), as.character)),
           "p1_comparison_table")

# Write LaTeX table
dir.create("../output/tables", recursive = TRUE, showWarnings = FALSE)

latex_rows <- comp_table %>%
  mutate(
    sep = Measure %in% c("LLM ensemble (R = 10)")
  )

latex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Spearman $\\rho$ with post-announcement OIS volatility}",
  "\\label{tab:p1_comparison}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Measure & 3M & 2Y & 10Y \\\\",
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Text-based measures}} \\\\[2pt]"
)

text_block <- c("FK Complexity","Word Count","Hedging Words",
                "LM Uncertainty Density","Net Hawkish-Dovish")
llm_block  <- c("LLM ensemble (R = 10)")

emit_rows <- function(measures) {
  comp_table %>%
    filter(Measure %in% measures) %>%
    mutate(line = glue("{Measure} & {`3M`} & {`2Y`} & {`10Y`} \\\\")) %>%
    pull(line)
}

latex_lines <- c(latex_lines,
  emit_rows(text_block),
  "[4pt]\\multicolumn{4}{l}{\\textit{LLM synthetic disagreement}} \\\\[2pt]",
  emit_rows(llm_block),
  "\\bottomrule",
  "\\multicolumn{4}{p{10cm}}{\\footnotesize",
  "  Spearman rank correlation with post-announcement OIS volatility.",
  "  Text measures use all available conferences;",
  "  LLM correlations use the common sample (ensemble $\\cap$ market vol $\\cap$ v6).",
  "  $^{***}$p$<$0.01, $^{**}$p$<$0.05, $^{*}$p$<$0.10.",
  "} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(latex_lines, "../output/tables/p1_comparison_table.tex")
cat("  Saved: p1_comparison_table.tex\n")

cat("\n", strrep("=", 80), "\n")
cat("POST ZERO-SHOT P1 COMPLETE\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# 13. DIRECTIONAL ANALYSIS — CONSOLIDATED SUMMARY TABLE
# ==============================================================================

cat("Section 13: directional analysis (consolidated table)...\n")

assign_regime <- function(d) {
  case_when(
    d >= as.Date("1999-01-01") & d <= as.Date("2008-09-01") ~ "Pre-crisis tightening",
    d >= as.Date("2009-01-01") & d <= as.Date("2021-12-31") ~ "Easing/ZLB",
    d >= as.Date("2022-01-01") & d <= as.Date("2023-12-31") ~ "Tightening cycle",
    TRUE ~ NA_character_
  )
}

modal_all <- direction_pct %>%
  group_by(date, tenor) %>%
  slice_max(percentage, n = 1, with_ties = FALSE) %>%
  ungroup()

# ---- Panel A: mean directional share by regime ----
panel_a <- direction_pct %>%
  mutate(
    regime = assign_regime(date),
    tenor  = as.character(tenor)
  ) %>%
  filter(!is.na(regime)) %>%
  group_by(regime, tenor, direction_clean) %>%
  summarise(
    value = as.character(round(mean(percentage, na.rm = TRUE), 1)),
    .groups = "drop"
  ) %>%
  mutate(
    section = "A: Regime shares",
    metric  = paste0(regime, " - ", as.character(direction_clean), " (%)")
  ) %>%
  select(section, metric, tenor, value) %>%
  pivot_wider(names_from = tenor, values_from = value)

# ---- Panel B: post-Lehman reversal speed (2Y) ----
dates_2y   <- direction_pct %>% filter(tenor == "2Y") %>%
  pull(date) %>% unique() %>% sort()
shock_date <- dates_2y[which.min(abs(dates_2y - as.Date("2008-09-15")))]
shock_idx  <- which(dates_2y == shock_date)

first_down_n <- modal_all %>%
  filter(as.character(tenor) == "2Y") %>%
  arrange(date) %>%
  mutate(m = match(date, dates_2y) - shock_idx) %>%
  filter(m > 0, as.character(direction_clean) == "Down") %>%
  slice_min(m, n = 1, with_ties = FALSE) %>%
  pull(m)

lehman_note <- if (length(first_down_n) > 0) {
  glue("Shock meeting (closest to 2008-09-15): {shock_date}. ",
       "For 2Y, 'Down' first becomes modal {first_down_n} meeting(s) after the shock.")
} else {
  glue("Shock meeting: {shock_date}. ",
       "'Down' did not become modal within 10 meetings after the shock (2Y).")
}

panel_b <- tibble(
  section = "B: Post-Lehman reversal (2Y)",
  metric  = "Meetings until 'Down' is modal direction",
  `3M`    = "--",
  `2Y`    = if (length(first_down_n) > 0) as.character(first_down_n) else "n.a.",
  `10Y`   = "--"
)

# ---- Panel C: modal reversal frequency ----
panel_c <- modal_all %>%
  arrange(tenor, date) %>%
  group_by(tenor) %>%
  mutate(
    prev = lag(direction_clean),
    flip = !is.na(prev) & direction_clean != prev
  ) %>%
  filter(!is.na(prev)) %>%
  summarise(
    value   = as.character(round(100 * sum(flip) / n(), 1)),
    .groups = "drop"
  ) %>%
  mutate(
    section = "C: Modal reversal frequency",
    metric  = "Direction changes (pct of consecutive pairs)",
    tenor   = as.character(tenor)
  ) %>%
  select(section, metric, tenor, value) %>%
  pivot_wider(names_from = tenor, values_from = value)

# ---- Panel D: 2022 tightening acceleration ----
acc_flags <- map_dfr(tenor_levels, function(t) {
  sub <- direction_pct %>%
    filter(
      as.character(direction_clean) == "Up",
      as.character(tenor) == t,
      date >= as.Date("2022-01-01"),
      date <= as.Date("2022-12-31")
    ) %>%
    arrange(date)
  if (nrow(sub) < 2) return(tibble())
  deltas   <- diff(sub$percentage)
  best_idx <- which.max(deltas)
  tibble(
    tenor        = t,
    delta_up_pp  = round(deltas[best_idx], 1),
    flagged_pair = paste0(
      format(sub$date[best_idx],     "%b %Y"),
      " -> ",
      format(sub$date[best_idx + 1], "%b %Y")
    )
  )
})

panel_d <- acc_flags %>%
  mutate(delta_up_pp = as.character(delta_up_pp)) %>%
  pivot_longer(c(delta_up_pp, flagged_pair), names_to = "type", values_to = "value") %>%
  mutate(
    section = "D: 2022 tightening acceleration",
    metric  = if_else(type == "delta_up_pp",
                      "Largest consecutive Up-share jump (pp)",
                      "Flagged meeting pair")
  ) %>%
  select(section, metric, tenor, value) %>%
  pivot_wider(names_from = tenor, values_from = value)

# ---- Assemble consolidated table ----
dir_summary <- bind_rows(panel_a, panel_b, panel_c, panel_d) %>%
  select(section, metric, any_of(tenor_levels))

cat("\n", strrep("=", 70), "\n")
cat("DIRECTIONAL ANALYSIS - SUMMARY TABLE\n")
cat(strrep("=", 70), "\n")
print(dir_summary, n = Inf)
cat(glue("\nNote: {lehman_note}\n\n"))

save_table(dir_summary, "p1_directional_analysis")

# ---- LaTeX ----
latex_dir <- "../output/tables"
dir.create(latex_dir, recursive = TRUE, showWarnings = FALSE)

panel_headers <- c(
  "A: Regime shares"                = "\\textit{Panel A: Mean directional share by regime}",
  "B: Post-Lehman reversal (2Y)"    = "\\textit{Panel B: Post-Lehman reversal speed (2Y tenor only)}",
  "C: Modal reversal frequency"     = "\\textit{Panel C: Modal direction reversal frequency}",
  "D: 2022 tightening acceleration" = "\\textit{Panel D: 2022 tightening acceleration}"
)

tex_esc <- function(x) str_replace_all(as.character(x), "%", "\\\\%")
na_dash <- function(x) ifelse(is.na(x), "--", x)

latex_body <- character()
prev_sec   <- ""
for (i in seq_len(nrow(dir_summary))) {
  row <- dir_summary[i, ]
  sec <- row$section
  if (sec != prev_sec) {
    if (nchar(prev_sec) > 0) latex_body <- c(latex_body, "\\addlinespace[6pt]")
    latex_body <- c(latex_body,
      glue("\\multicolumn{{4}}{{l}}{{{panel_headers[sec]}}} \\\\[2pt]"))
    prev_sec <- sec
  }
  latex_body <- c(latex_body, glue(
    "\\quad {tex_esc(row$metric)} & {tex_esc(na_dash(row$`3M`))} & ",
    "{tex_esc(na_dash(row$`2Y`))} & {tex_esc(na_dash(row$`10Y`))} \\\\"
  ))
}

latex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\small",
  "\\caption{Directional forecast analysis by tenor}",
  "\\label{tab:p1_directional_analysis}",
  "\\begin{tabular}{p{8cm}ccc}",
  "\\toprule",
  " & \\textbf{3M} & \\textbf{2Y} & \\textbf{10Y} \\\\",
  "\\midrule",
  latex_body,
  "\\bottomrule",
  glue("\\multicolumn{{4}}{{p{{13cm}}}}{{\\footnotesize \\textit{{Note:}} {lehman_note}}} \\\\"),
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(latex_lines, file.path(latex_dir, "p1_directional_analysis.tex"))
cat(glue("  Saved: p1_directional_analysis (.rds / .xlsx / .tex)\n\n"))

cat("Section 13 complete.\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
