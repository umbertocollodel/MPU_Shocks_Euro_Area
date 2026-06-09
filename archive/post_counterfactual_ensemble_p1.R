#===============================================================================
# POST COUNTERFACTUAL ENSEMBLE P1 — Figures & Stability Diagnostics
#===============================================================================
# Project: Interpreting the Interpreter — ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Replicates the script-16 counterfactual figures using the R=10 ensemble
#   outputs from run_counterfactual_ensemble_p1.R, and adds stability
#   diagnostics for R=10. All outputs prefixed "cf_".
#
# Inputs (READ ONLY):
#   ../intermediate_data/counterfactual_ensemble_p1/parsed/cf_runs_long.rds
#   ../intermediate_data/counterfactual_ensemble_p1/parsed/cf_within_run.rds
#   ../intermediate_data/counterfactual_ensemble_p1/parsed/cf_ensemble.rds
#   ../intermediate_data/counterfactual_ensemble_p1/parsed/cf_treatment_effect.rds
#
# Outputs:
#   output/figures/cf_p1/
#     cf_fig_mean_reduction_pct    — mean % reduction by tenor (bar + SE)
#     cf_fig_mean_reduction_bps    — mean bps reduction by tenor (bar + CI)
#     cf_fig_change_distribution   — stacked bar: reduction/no change/increase
#     cf_fig_time_series           — pct_reduction over time, faceted by tenor
#     cf_fig_heatmap               — categorical heatmap (conferences × tenors)
#     cf_fig_sd_overlay            — ensemble SD: original vs edited over time
#     cf_fig_GR_by_version         — G(R) reliability curves by version
#     cf_fig_sd_stabilization      — SD stabilization by version
#     cf_fig_within_sd_boxplot     — within-SD distribution across runs
#     cf_fig_ensemble_cv           — ensemble CV = sd_se / sd_mean scatter
#     (each as .pdf + .png)
#   intermediate_data/cf_p1/
#     cf_summary_stats, cf_t_tests_pct, cf_t_tests_bps,
#     cf_change_counts, cf_GR_by_version   (each as .rds + .xlsx)
#     cf_summary_tables.xlsx
#
# Usage: source("post_counterfactual_ensemble_p1.R")  from code/ directory
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("POST COUNTERFACTUAL ENSEMBLE P1 — Figures & Stability Diagnostics\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# 1. SETUP
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, readxl, writexl, readr, scales, showtext, glue
)

if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) {
    font_add("Segoe UI", regular = font_path)
  } else {
    warning("Segoe UI not found; place segoeui.ttf in code/ directory. Using default.")
  }
}
showtext_auto()

fig_dir  <- "../output/figures/cf_p1"
data_dir <- "../intermediate_data/cf_p1"
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

assert_cf <- function(path) {
  bn <- basename(path)
  if (!startsWith(bn, "cf_")) stop(glue("Output violates cf_ prefix rule: '{bn}'"))
  invisible(path)
}

planned_stems <- c(
  file.path(fig_dir,  "cf_fig_mean_reduction_pct"),
  file.path(fig_dir,  "cf_fig_mean_reduction_bps"),
  file.path(fig_dir,  "cf_fig_change_distribution"),
  file.path(fig_dir,  "cf_fig_time_series"),
  file.path(fig_dir,  "cf_fig_heatmap"),
  file.path(fig_dir,  "cf_fig_sd_overlay"),
  file.path(fig_dir,  "cf_fig_GR_by_version"),
  file.path(fig_dir,  "cf_fig_sd_stabilization"),
  file.path(fig_dir,  "cf_fig_within_sd_boxplot"),
  file.path(fig_dir,  "cf_fig_ensemble_cv"),
  file.path(fig_dir,  "cf_fig_sd_convergence_by_version"),
  file.path(fig_dir,  "cf_fig_gap_convergence"),
  file.path(data_dir, "cf_summary_stats"),
  file.path(data_dir, "cf_t_tests_pct"),
  file.path(data_dir, "cf_t_tests_bps"),
  file.path(data_dir, "cf_change_counts"),
  file.path(data_dir, "cf_GR_by_version")
)
invisible(lapply(planned_stems, assert_cf))
cat("Prefix assertion passed for all", length(planned_stems), "output paths.\n\n")

tenor_levels  <- c("3M", "2Y", "10Y")
tenor_colours <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")
threshold_pct <- 5

cf_theme <- theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title       = element_blank(),
    plot.caption     = element_text(hjust = 0, size = 9, colour = "grey30"),
    strip.text       = element_text(face = "bold"),
    axis.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

save_plot <- function(plot, stem, w = 10, h = 7) {
  assert_cf(paste0(stem, ".pdf"))
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

save_table <- function(df, stem) {
  assert_cf(paste0(stem, ".rds"))
  saveRDS(df, file.path(data_dir, paste0(stem, ".rds")))
  writexl::write_xlsx(df, file.path(data_dir, paste0(stem, ".xlsx")))
  cat(glue("  Saved: {stem}.rds/.xlsx\n"))
  invisible(df)
}

# Returns list(stat, pval, ci_lo, ci_hi); all NA when < 2 non-NA obs.
safe_ttest <- function(x) {
  x <- na.omit(x)
  if (length(x) < 2) {
    return(list(stat = NA_real_, pval = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_))
  }
  g  <- t.test(x, mu = 0, alternative = "greater")
  tw <- t.test(x, mu = 0)
  list(stat = g$statistic[[1]], pval = g$p.value,
       ci_lo = tw$conf.int[1],  ci_hi = tw$conf.int[2])
}

cat("Setup complete.\n\n")

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

cat("Loading data...\n")

parsed_dir <- "../intermediate_data/counterfactual_ensemble_p1/parsed"

cf_within_run    <- readRDS(file.path(parsed_dir, "cf_within_run.rds"))
cf_ensemble      <- readRDS(file.path(parsed_dir, "cf_ensemble.rds"))
cf_treatment_eff <- readRDS(file.path(parsed_dir, "cf_treatment_effect.rds"))

cf_treatment_eff <- cf_treatment_eff %>%
  mutate(
    date  = as.Date(date),
    tenor = factor(tenor, levels = tenor_levels)
  ) %>%
  filter(tenor %in% tenor_levels)

cf_ensemble <- cf_ensemble %>%
  mutate(
    date    = as.Date(date),
    tenor   = factor(tenor,   levels = tenor_levels),
    version = factor(version, levels = c("original", "edited"))
  ) %>%
  filter(tenor %in% tenor_levels)

cf_within_run <- cf_within_run %>%
  mutate(
    date    = as.Date(date),
    tenor   = factor(tenor,   levels = tenor_levels),
    version = factor(version, levels = c("original", "edited"))
  ) %>%
  filter(tenor %in% tenor_levels)

cat(glue(
  "cf_treatment_eff: {nrow(cf_treatment_eff)} rows | ",
  "{n_distinct(cf_treatment_eff$date)} dates | ",
  "{n_distinct(cf_treatment_eff$tenor)} tenors\n"
))
cat(glue(
  "cf_ensemble:   {nrow(cf_ensemble)} rows | ",
  "versions: {paste(levels(cf_ensemble$version), collapse = ', ')}\n"
))
cat(glue("cf_within_run: {nrow(cf_within_run)} rows\n\n"))

# ==============================================================================
# 3. SUMMARY STATISTICS AND T-TESTS
# ==============================================================================

cat("Computing summary statistics and t-tests...\n")

summary_stats <- cf_treatment_eff %>%
  group_by(tenor) %>%
  summarise(
    n_conferences        = n(),
    mean_reduction_pct   = mean(pct_reduction,    na.rm = TRUE),
    median_reduction_pct = median(pct_reduction,  na.rm = TRUE),
    sd_reduction         = sd(pct_reduction,      na.rm = TRUE),
    se                   = sd_reduction / sqrt(n_conferences),
    mean_baseline_sd     = mean(sd_mean_original, na.rm = TRUE),
    mean_edited_sd       = mean(sd_mean_edited,   na.rm = TRUE),
    mean_delta_bps       = mean(delta_bps * 100,  na.rm = TRUE),
    .groups = "drop"
  )

change_counts <- cf_treatment_eff %>%
  mutate(
    has_reduction = pct_reduction >  threshold_pct,
    no_change     = abs(pct_reduction) <= threshold_pct,
    has_increase  = pct_reduction < -threshold_pct
  ) %>%
  group_by(tenor) %>%
  summarise(
    n_total             = n(),
    n_with_reduction    = sum(has_reduction),
    pct_with_reduction  = 100 * n_with_reduction / n_total,
    n_no_change         = sum(no_change),
    pct_no_change       = 100 * n_no_change / n_total,
    n_with_increase     = sum(has_increase),
    pct_with_increase   = 100 * n_with_increase / n_total,
    .groups = "drop"
  )

t_test_pct <- cf_treatment_eff %>%
  group_by(tenor) %>%
  summarise(
    result   = list(safe_ttest(pct_reduction)),
    mean_pct = mean(pct_reduction, na.rm = TRUE),
    n_obs    = sum(!is.na(pct_reduction)),
    .groups  = "drop"
  ) %>%
  mutate(
    t_stat   = map_dbl(result, ~ .x$stat),
    p_value  = map_dbl(result, ~ .x$pval),
    ci_lower = map_dbl(result, ~ .x$ci_lo),
    ci_upper = map_dbl(result, ~ .x$ci_hi)
  ) %>%
  select(-result)

t_test_bps <- cf_treatment_eff %>%
  mutate(delta_bps_100 = delta_bps * 100) %>%
  group_by(tenor) %>%
  summarise(
    result   = list(safe_ttest(delta_bps_100)),
    mean_bps = mean(delta_bps_100, na.rm = TRUE),
    n_obs    = sum(!is.na(delta_bps_100)),
    .groups  = "drop"
  ) %>%
  mutate(
    t_stat   = map_dbl(result, ~ .x$stat),
    p_value  = map_dbl(result, ~ .x$pval),
    ci_lower = map_dbl(result, ~ .x$ci_lo),
    ci_upper = map_dbl(result, ~ .x$ci_hi)
  ) %>%
  select(-result)

cat("Summary statistics:\n"); print(summary_stats)
cat(glue("\nChange distribution (threshold = {threshold_pct}%):\n")); print(change_counts)
cat("\nT-test results (pct_reduction > 0):\n"); print(t_test_pct)

save_table(summary_stats, "cf_summary_stats")
save_table(t_test_pct,    "cf_t_tests_pct")
save_table(t_test_bps,    "cf_t_tests_bps")
save_table(change_counts, "cf_change_counts")

# ==============================================================================
# 4. FIGURE A — MEAN REDUCTION % BY TENOR
# ==============================================================================

cat("\nFigure A: mean reduction % by tenor...\n")

fig_a <- summary_stats %>%
  mutate(tenor = factor(tenor, levels = tenor_levels)) %>%
  ggplot(aes(x = tenor, y = mean_reduction_pct, fill = tenor)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_reduction_pct - 1.96 * se,
        ymax = mean_reduction_pct + 1.96 * se),
    width = 0.25, linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_fill_manual(values = tenor_colours) +
  labs(
    x       = "",
    y       = "%",
    caption = glue(
      "Mean reduction in synthetic disagreement (original minus edited, ",
      "as % of baseline ensemble SD). Error bars = ±1.96 SE across ",
      "{n_distinct(cf_treatment_eff$date)} conferences."
    )
  ) +
  cf_theme +
  theme(
    axis.title         = element_text(size = 20, face = "bold"),
    axis.text          = element_text(size = 18),
    panel.grid.major.x = element_blank()
  )

save_plot(fig_a, "cf_fig_mean_reduction_pct", w = 10, h = 7)

# ==============================================================================
# 5. FIGURE B — MEAN REDUCTION IN BPS BY TENOR
# ==============================================================================

cat("Figure B: mean reduction bps by tenor...\n")

fig_b <- t_test_bps %>%
  mutate(tenor = factor(tenor, levels = tenor_levels)) %>%
  ggplot(aes(x = tenor, y = mean_bps, fill = tenor)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.25, linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_fill_manual(values = tenor_colours) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    x       = "",
    y       = "Bps",
    caption = paste0(
      "Mean reduction in synthetic disagreement (bps). ",
      "Error bars = 95% CI from one-sample t-test (H1: mu > 0)."
    )
  ) +
  cf_theme +
  theme(
    axis.title         = element_text(size = 20, face = "bold"),
    axis.text          = element_text(size = 18),
    panel.grid.major.x = element_blank()
  )

save_plot(fig_b, "cf_fig_mean_reduction_bps", w = 10, h = 7)

# ==============================================================================
# 6. FIGURE C — CHANGE DISTRIBUTION
# ==============================================================================

cat("Figure C: change distribution...\n")

change_long <- change_counts %>%
  select(tenor,
         Reduction = n_with_reduction,
         `No Change` = n_no_change,
         Increase = n_with_increase) %>%
  pivot_longer(c(Reduction, `No Change`, Increase),
               names_to = "change_type", values_to = "count") %>%
  mutate(change_type = factor(change_type,
                              levels = c("Increase", "No Change", "Reduction")))

fig_c <- change_long %>%
  mutate(tenor = factor(tenor, levels = tenor_levels)) %>%
  ggplot(aes(x = tenor, y = count, fill = change_type)) +
  geom_col(width = 0.6) +
  geom_text(
    data = change_counts,
    aes(x = tenor, y = n_total, label = n_total, fill = NULL),
    vjust = -0.5, size = 5, fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Reduction" = "#2ca25f",
    "No Change" = "#fee5d9",
    "Increase"  = "#de2d26"
  )) +
  labs(
    x       = "OIS Tenor",
    y       = "Number of Conferences",
    fill    = "Change Type",
    caption = glue(
      "Threshold = {threshold_pct}% for meaningful change. ",
      "Top labels = total conferences per tenor."
    )
  ) +
  cf_theme +
  theme(
    axis.title         = element_text(size = 16, face = "bold"),
    axis.text          = element_text(size = 14),
    legend.title       = element_text(size = 14, face = "bold"),
    legend.text        = element_text(size = 12),
    panel.grid.major.x = element_blank()
  )

save_plot(fig_c, "cf_fig_change_distribution", w = 10, h = 7)

# ==============================================================================
# 7. FIGURE D — TIME SERIES OF REDUCTION %
# ==============================================================================

cat("Figure D: time series of pct_reduction...\n")

fig_d <- cf_treatment_eff %>%
  ggplot(aes(x = date, y = pct_reduction, colour = tenor)) +
  geom_hline(yintercept = 0,               linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept =  threshold_pct,  linetype = "dotted", colour = "grey60", alpha = 0.7) +
  geom_hline(yintercept = -threshold_pct,  linetype = "dotted", colour = "grey60", alpha = 0.7) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 1.8, alpha = 0.8) +
  facet_wrap(~ tenor, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = tenor_colours, guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x       = NULL,
    y       = "Disagreement Reduction (%)",
    caption = glue(
      "Ensemble-averaged (R=10) reduction in synthetic disagreement per conference. ",
      "Dotted lines = ±{threshold_pct}% threshold. Positive = reduction."
    )
  ) +
  cf_theme +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(colour = "grey80", fill = NA)
  )

save_plot(fig_d, "cf_fig_time_series", w = 12, h = 10)

# ==============================================================================
# 8. FIGURE E — HEATMAP OF REDUCTION CATEGORY
# ==============================================================================

cat("Figure E: heatmap of reduction category...\n")

category_levels <- c(
  "Strong Increase (≥20%)",
  "Moderate Increase (10-20%)",
  "Mild Increase (5-10%)",
  "No Change (±5%)",
  "Mild Reduction (5-10%)",
  "Moderate Reduction (10-20%)",
  "Strong Reduction (≥20%)"
)

results_labelled <- cf_treatment_eff %>%
  arrange(date) %>%
  mutate(
    conf_num   = as.integer(factor(date, levels = unique(date))),
    date_label = format(date, "%Y-%m"),
    reduction_category = case_when(
      pct_reduction >=  20 ~ "Strong Reduction (≥20%)",
      pct_reduction >=  10 ~ "Moderate Reduction (10-20%)",
      pct_reduction >=   5 ~ "Mild Reduction (5-10%)",
      pct_reduction >   -5 ~ "No Change (±5%)",
      pct_reduction >  -10 ~ "Mild Increase (5-10%)",
      pct_reduction >  -20 ~ "Moderate Increase (10-20%)",
      TRUE                 ~ "Strong Increase (≥20%)"
    ),
    reduction_category = factor(reduction_category, levels = category_levels)
  )

date_metadata <- results_labelled %>%
  distinct(date, date_label, conf_num) %>%
  arrange(conf_num) %>%
  mutate(show_label = row_number() %% 2 == 1)

fig_e <- results_labelled %>%
  ggplot(aes(x = reorder(date_label, conf_num), y = tenor,
             fill = reduction_category)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c(
      "Strong Increase (≥20%)"      = "#b2182b",
      "Moderate Increase (10-20%)"  = "#ef8a62",
      "Mild Increase (5-10%)"       = "#f7bf87ff",
      "No Change (±5%)"             = "#f7f7f7",
      "Mild Reduction (5-10%)"      = "#d1e5f0",
      "Moderate Reduction (10-20%)" = "#67a9cf",
      "Strong Reduction (≥20%)"     = "#2166ac"
    ),
    drop = FALSE
  ) +
  scale_x_discrete(
    breaks = date_metadata$date_label[date_metadata$show_label],
    labels = date_metadata$date_label[date_metadata$show_label]
  ) +
  labs(
    x       = "Press Conference Date",
    y       = "",
    fill    = "",
    caption = "Ensemble-averaged (R=10) reduction category per conference. Every other date label shown."
  ) +
  cf_theme +
  theme(
    axis.title        = element_text(size = 20, face = "bold"),
    axis.text.y       = element_text(size = 18),
    axis.text.x       = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(1.5, "cm"),
    panel.grid        = element_blank(),
    panel.border      = element_rect(colour = "grey80", fill = NA)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

save_plot(fig_e, "cf_fig_heatmap", w = 16, h = 6)

# ==============================================================================
# 9. FIGURE F — ENSEMBLE SD OVERLAY: ORIGINAL vs EDITED
# ==============================================================================

cat("Figure F: ensemble SD overlay (original vs edited)...\n")

cf_ens_ts <- cf_ensemble %>%
  mutate(
    ribbon_lo = pmax(0, sd_mean - 1.96 * sd_se),
    ribbon_hi = sd_mean + 1.96 * sd_se
  )

fig_f <- ggplot(cf_ens_ts,
                aes(x = date, colour = version, fill = version)) +
  geom_ribbon(aes(ymin = ribbon_lo, ymax = ribbon_hi),
              colour = NA, alpha = 0.12) +
  geom_line(aes(y = sd_mean), linewidth = 0.7) +
  facet_wrap(~ tenor, ncol = 1, scales = "free_y") +
  scale_colour_manual(
    values = c("original" = "#4575b4", "edited" = "#d73027"),
    name   = "Version",
    labels = c("original" = "Original", "edited" = "Edited")
  ) +
  scale_fill_manual(
    values = c("original" = "#4575b4", "edited" = "#d73027"),
    name   = "Version",
    labels = c("original" = "Original", "edited" = "Edited")
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x       = NULL,
    y       = "Ensemble SD of Predicted Rate",
    caption = paste0(
      "Band = mean ± 1.96 SE across 10 runs. ",
      "Downward shift from original to edited indicates successful treatment."
    )
  ) +
  cf_theme +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, size = 9),
    panel.border = element_rect(colour = "grey80", fill = NA)
  )

save_plot(fig_f, "cf_fig_sd_overlay", w = 10, h = 9)

# ==============================================================================
# 10. STABILITY DIAGNOSTIC: G(R) BY VERSION
# ==============================================================================

cat("Figure G: G(R) reliability by version...\n")

gr_params <- cf_within_run %>%
  group_by(version, date, tenor) %>%
  summarise(
    mean_wr_sd = mean(within_sd, na.rm = TRUE),
    var_wr_sd  = var(within_sd,  na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  group_by(version, tenor) %>%
  summarise(
    sigma_between = sd(mean_wr_sd,          na.rm = TRUE),
    sigma_within  = sqrt(mean(var_wr_sd,    na.rm = TRUE)),
    .groups       = "drop"
  )

gr_df <- crossing(gr_params, R = 1:20) %>%
  mutate(
    G_R     = sigma_between^2 / (sigma_between^2 + sigma_within^2 / R),
    tenor   = factor(tenor,   levels = tenor_levels),
    version = factor(version, levels = c("original", "edited"))
  )

save_table(gr_df %>% mutate(across(where(is.factor), as.character)), "cf_GR_by_version")

n_dates_gr <- n_distinct(cf_within_run$date)

fig_g <- ggplot(gr_df,
                aes(x = R, y = G_R, colour = tenor, linetype = version)) +
  geom_line(linewidth = 1.0) +
  geom_vline(xintercept = 10, linetype = "dotdash", colour = "grey40",
             linewidth = 0.6) +
  geom_hline(yintercept = 0.80, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_hline(yintercept = 0.90, linetype = "dotted", colour = "grey50",
             linewidth = 0.5) +
  annotate("text", x = 20.5, y = 0.80, label = "0.80", size = 3,
           colour = "grey40", hjust = 0) +
  annotate("text", x = 20.5, y = 0.90, label = "0.90", size = 3,
           colour = "grey40", hjust = 0) +
  annotate("text", x = 10.4, y = 0.12, label = "R = 10", size = 3,
           colour = "grey40", hjust = 0) +
  scale_colour_manual(values = tenor_colours, name = "Tenor") +
  scale_linetype_manual(
    values = c("original" = "solid", "edited" = "longdash"),
    name   = "Version",
    labels = c("original" = "Original", "edited" = "Edited")
  ) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20), limits = c(1, 22)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    x       = "Number of runs averaged (R)",
    y       = "G(R)",
    caption = glue(
      "{n_dates_gr} ECB conferences × 2 versions. ",
      "G(R) = between-conference variance / (between + within/R). ",
      "Dashed = 0.80 (Cicchetti good); dotted = 0.90. Vertical dot-dash at R = 10."
    )
  ) +
  cf_theme

save_plot(fig_g, "cf_fig_GR_by_version", w = 9, h = 6)

# ==============================================================================
# 11. STABILITY DIAGNOSTIC: SD STABILIZATION BY VERSION
# ==============================================================================

cat("Figure H: SD stabilization by version...\n")

stab_df <- cf_within_run %>%
  arrange(date, version, tenor, run) %>%
  group_by(date, version, tenor) %>%
  mutate(
    cum_mean_sd = cummean(within_sd),
    terminal_sd = last(cum_mean_sd)
  ) %>%
  ungroup() %>%
  mutate(norm_cum_mean = cum_mean_sd / terminal_sd) %>%
  filter(!is.na(norm_cum_mean), is.finite(norm_cum_mean))

stab_median <- stab_df %>%
  group_by(version, tenor, run) %>%
  summarise(median_norm = median(norm_cum_mean, na.rm = TRUE), .groups = "drop")

band_df <- crossing(
  version = factor(c("original", "edited"), levels = c("original", "edited")),
  tenor   = factor(tenor_levels, levels = tenor_levels),
  run     = 1:10
) %>%
  mutate(ymin = 0.95, ymax = 1.05)

n_conf_stab <- n_distinct(stab_df$date)

fig_h <- ggplot() +
  geom_ribbon(
    data    = band_df,
    mapping = aes(x = run, ymin = ymin, ymax = ymax),
    fill = "grey60", alpha = 0.1, inherit.aes = FALSE
  ) +
  geom_line(
    data    = stab_df,
    mapping = aes(x = run, y = norm_cum_mean,
                  group = interaction(date, version, tenor),
                  colour = version),
    linewidth = 0.2, alpha = 0.25
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey30",
             linewidth = 0.5) +
  geom_line(
    data    = stab_median,
    mapping = aes(x = run, y = median_norm, colour = version),
    linewidth = 1.3
  ) +
  facet_grid(version ~ tenor,
             labeller = labeller(
               version = c("original" = "Original", "edited" = "Edited")
             )) +
  scale_colour_manual(
    values = c("original" = "#4575b4", "edited" = "#d73027"),
    guide  = "none"
  ) +
  scale_x_continuous(breaks = 1:10) +
  coord_cartesian(ylim = c(0.7, 1.3)) +
  labs(
    x       = "Number of runs (R)",
    y       = "Normalised cumulative mean SD",
    caption = glue(
      "Each line = one ECB conference ({n_conf_stab} total) × version. ",
      "Bold line = median. Normalised by R=10 terminal value. Shaded band = ±5%."
    )
  ) +
  cf_theme +
  theme(panel.border = element_rect(colour = "grey80", fill = NA))

save_plot(fig_h, "cf_fig_sd_stabilization", w = 12, h = 7)

# ==============================================================================
# 12. STABILITY DIAGNOSTIC: WITHIN-SD VARIABILITY ACROSS RUNS
# ==============================================================================

cat("Figure I: within-SD boxplot across runs...\n")

fig_i <- cf_within_run %>%
  ggplot(aes(x = version, y = within_sd, fill = version)) +
  geom_boxplot(width = 0.5, outlier.size = 0.8, outlier.alpha = 0.4,
               show.legend = FALSE) +
  facet_wrap(~ tenor, ncol = 3, scales = "free_y") +
  scale_fill_manual(
    values = c("original" = "#4575b4", "edited" = "#d73027")
  ) +
  scale_x_discrete(labels = c("original" = "Original", "edited" = "Edited")) +
  labs(
    x       = NULL,
    y       = "Within-run SD (predicted rate)",
    caption = glue(
      "Distribution of within_sd across all date × run cells (n = ",
      "{n_distinct(cf_within_run$date)} conferences × 10 runs). ",
      "Each cell = SD across 30 agents."
    )
  ) +
  cf_theme +
  theme(panel.grid.major.x = element_blank())

save_plot(fig_i, "cf_fig_within_sd_boxplot", w = 12, h = 5)

# ==============================================================================
# 13. STABILITY DIAGNOSTIC: ENSEMBLE COEFFICIENT OF VARIATION
# ==============================================================================

cat("Figure J: ensemble CV (sd_se / sd_mean)...\n")

cv_df <- cf_ensemble %>%
  mutate(cv = sd_se / sd_mean) %>%
  filter(!is.na(cv), is.finite(cv))

fig_j <- ggplot(cv_df, aes(x = sd_mean, y = cv, colour = version)) +
  geom_point(alpha = 0.45, size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ tenor, ncol = 3, scales = "free_x") +
  scale_colour_manual(
    values = c("original" = "#4575b4", "edited" = "#d73027"),
    name   = "Version",
    labels = c("original" = "Original", "edited" = "Edited")
  ) +
  labs(
    x       = "Ensemble SD (mean across 10 runs)",
    y       = "CV = SE / Mean",
    caption = paste0(
      "Coefficient of variation = Monte Carlo SE relative to ensemble mean. ",
      "Lower CV indicates more stable R=10 estimates."
    )
  ) +
  cf_theme

save_plot(fig_j, "cf_fig_ensemble_cv", w = 12, h = 5)

# ==============================================================================
# 14. CONVERGENCE OF AVERAGE SD OVER R BY VERSION
# ==============================================================================
# Diagnostic for the counterintuitive R=10 result: with R=1 editing reduced
# volatility, but R=10 shows no reduction. This section reveals whether the
# gap closes because the original SD falls, the edited SD rises, or both.
# ==============================================================================

cat("Figure K: average SD convergence over R by version...\n")

# For each R in 1:10: compute the cumulative-average within_sd up to run R for
# each (date, version, tenor), then average across all conferences.
sd_conv <- cf_within_run %>%
  arrange(date, version, tenor, run) %>%
  group_by(date, version, tenor) %>%
  mutate(cum_mean_sd = cummean(within_sd)) %>%
  ungroup() %>%
  group_by(version, tenor, run) %>%
  summarise(
    mean_sd = mean(cum_mean_sd, na.rm = TRUE),
    se_sd   = sd(cum_mean_sd,   na.rm = TRUE) / sqrt(sum(!is.na(cum_mean_sd))),
    .groups = "drop"
  ) %>%
  mutate(
    ribbon_lo = mean_sd - 1.96 * se_sd,
    ribbon_hi = mean_sd + 1.96 * se_sd,
    tenor     = factor(tenor,   levels = tenor_levels),
    version   = factor(version, levels = c("original", "edited"))
  )

n_conf_conv <- n_distinct(cf_within_run$date)

fig_k <- ggplot(sd_conv, aes(x = run, colour = version, fill = version)) +
  geom_ribbon(aes(ymin = ribbon_lo, ymax = ribbon_hi),
              colour = NA, alpha = 0.12) +
  geom_line(aes(y = mean_sd), linewidth = 0.9) +
  geom_point(aes(y = mean_sd), size = 1.8) +
  facet_wrap(~ tenor, ncol = 3, scales = "free_y") +
  scale_colour_manual(
    values = c("original" = "#4575b4", "edited" = "#d73027"),
    name   = "Version",
    labels = c("original" = "Original", "edited" = "Edited")
  ) +
  scale_fill_manual(
    values = c("original" = "#4575b4", "edited" = "#d73027"),
    name   = "Version",
    labels = c("original" = "Original", "edited" = "Edited")
  ) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x       = "Number of runs averaged (R)",
    y       = "Mean SD of predicted rate",
    caption = glue(
      "Each point = average across {n_conf_conv} conferences of the cumulative ",
      "mean SD up to run R. Band = ±1.96 SE across conferences. ",
      "Reveals whether the original/edited gap changes as R grows."
    )
  ) +
  cf_theme

save_plot(fig_k, "cf_fig_sd_convergence_by_version", w = 13, h = 5)

# Companion: gap (original - edited, in bps) as a function of R
gap_conv <- sd_conv %>%
  select(version, tenor, run, mean_sd) %>%
  pivot_wider(names_from = version, values_from = mean_sd) %>%
  mutate(
    gap   = (original - edited) * 100,
    tenor = factor(tenor, levels = tenor_levels)
  )

fig_k2 <- ggplot(gap_conv, aes(x = run, y = gap, colour = tenor)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.0) +
  scale_colour_manual(values = tenor_colours, name = "Tenor") +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x       = "Number of runs averaged (R)",
    y       = "Gap: original − edited (bps)",
    caption = glue(
      "Gap = mean SD (original) − mean SD (edited), in bps, as R grows from 1 to 10. ",
      "Positive = editing reduces disagreement; downward slope = gap shrinks with more runs. ",
      "{n_conf_conv} conferences."
    )
  ) +
  cf_theme

save_plot(fig_k2, "cf_fig_gap_convergence", w = 10, h = 6)

# ==============================================================================
# 15. EXPORT SUMMARY TABLES
# ==============================================================================

cat("\nExporting combined summary tables...\n")

writexl::write_xlsx(
  list(
    "Summary_Stats"       = summary_stats %>%
                              mutate(across(where(is.factor), as.character)),
    "T_Tests_Pct"         = t_test_pct %>%
                              mutate(across(where(is.factor), as.character)),
    "T_Tests_Bps"         = t_test_bps %>%
                              mutate(across(where(is.factor), as.character)),
    "Change_Distribution" = change_counts %>%
                              mutate(across(where(is.factor), as.character)),
    "GR_By_Version"       = gr_df %>%
                              mutate(across(where(is.factor), as.character))
  ),
  file.path(data_dir, "cf_summary_tables.xlsx")
)
cat(glue("  Saved: cf_summary_tables.xlsx\n"))

# ==============================================================================
# 16. FINAL SUMMARY PRINT
# ==============================================================================

cat("\n", strrep("-", 60), "\n")
cat("SUMMARY REPORT\n")
cat(strrep("-", 60), "\n\n")

cat("Mean reduction by tenor:\n")
summary_stats %>%
  select(tenor, n_conferences, mean_reduction_pct, se, mean_delta_bps) %>%
  print()

cat("\nStatistical significance (pct_reduction > 0, one-sided t-test):\n")
t_test_pct %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  ) %>%
  select(tenor, mean_pct, t_stat, p_value, sig) %>%
  print()

cat("\nG(R=10) by version and tenor:\n")
gr_df %>%
  filter(R == 10) %>%
  select(version, tenor, G_R, sigma_between, sigma_within) %>%
  arrange(version, tenor) %>%
  print()

cat("\n", strrep("=", 80), "\n")
cat("POST COUNTERFACTUAL ENSEMBLE P1 COMPLETE\n")
cat(strrep("=", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
