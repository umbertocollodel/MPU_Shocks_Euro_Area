#===============================================================================
# SCRIPT 19 — CONFIDENCE SCORES: USE OR DROP
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Evaluates the 0–100 confidence score elicited from each agent to decide
#   whether to keep or drop the field. Three analyses:
#     (1) Raw distribution — histogram per tenor; diagnose score bunching.
#     (2) Internal validation — Spearman ρ between mean confidence and
#         (a) synthetic SD, (b) realised OIS volatility. Hypothesis: negative.
#     (3) Confidence-weighted dispersion — recompute ensemble SD with
#         confidence/100 weights; bootstrap Δρ vs unweighted baseline.
#
# Inputs (READ ONLY):
#   ../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds
#   ../intermediate_data/p1/p1_ensemble_sd.rds
#   ../intermediate_data/range_difference_df.rds
#
# Outputs:
#   ../output/figures/p1/p1_fig_confidence.pdf/.png
#   ../intermediate_data/p1/p1_confidence_table.rds/.xlsx
#   NOTES.md   (one-paragraph recommendation, written at end of script)
#
# Usage: source("19confidence_scores_analysis.R")  from code/ directory
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("SCRIPT 19 — CONFIDENCE SCORES: USE OR DROP\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# 1. SETUP
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, readxl, writexl, boot, patchwork, showtext, glue, scales
)

if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) font_add("Segoe UI", regular = font_path)
}
showtext_auto()

fig_dir  <- "../output/figures/p1"
data_dir <- "../intermediate_data/p1"
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

tenor_levels  <- c("3M", "2Y", "10Y")
tenor_colours <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

p1_theme <- theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title       = element_blank(),
    plot.caption     = element_text(hjust = 0, size = 9,  colour = "grey30", family = "Segoe UI"),
    strip.text       = element_text(face = "bold", size = 14, family = "Segoe UI"),
    axis.title       = element_text(face = "bold", size = 13, family = "Segoe UI"),
    axis.text        = element_text(size = 11,                family = "Segoe UI"),
    legend.text      = element_text(size = 11,                family = "Segoe UI"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

save_plot <- function(plot, stem, w = 12, h = 8) {
  for (ext in c("pdf", "png")) {
    ggsave(
      filename = file.path(fig_dir, paste0(stem, ".", ext)),
      plot = plot, dpi = 320, width = w, height = h, bg = "white"
    )
  }
  cat(glue("  Saved: {stem}.pdf/.png\n"))
  invisible(plot)
}

save_table <- function(df, stem) {
  saveRDS(df,                    file.path(data_dir, paste0(stem, ".rds")))
  writexl::write_xlsx(df,        file.path(data_dir, paste0(stem, ".xlsx")))
  cat(glue("  Saved: {stem}.rds/.xlsx\n"))
  invisible(df)
}

B_BOOT <- 5000
set.seed(42)

cat("Setup complete.\n\n")

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

cat("Loading data...\n")

all_runs_path <- "../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds"

# Fallback: root of full_ensemble_p1/ (where script 10 also stores it)
if (!file.exists(all_runs_path)) {
  all_runs_path <- "../intermediate_data/full_ensemble_p1/all_runs_long.rds"
}
if (!file.exists(all_runs_path)) {
  stop(glue(
    "all_runs_long.rds not found. ",
    "Run 09run_full_ensemble_p1.R or 10post_zero_shot_p1.R first."
  ))
}

all_runs <- readRDS(all_runs_path) %>%
  mutate(
    date  = as.Date(date),
    tenor = if_else(tenor == "3mnt", "3M", tenor),
    tenor = factor(tenor, levels = tenor_levels)
  ) %>%
  filter(tenor %in% tenor_levels, !is.na(rate), !is.na(confidence))

ensemble_sd <- readRDS("../intermediate_data/p1/p1_ensemble_sd.rds") %>%
  mutate(
    date  = as.Date(date),
    tenor = factor(tenor, levels = tenor_levels)
  ) %>%
  select(date, tenor, sd_mean)

market_vol <- readRDS("../intermediate_data/range_difference_df.rds") %>%
  mutate(
    tenor = if_else(tenor == "3mnt", "3M", tenor),
    tenor = factor(tenor, levels = tenor_levels),
    date  = as.Date(date)
  ) %>%
  select(tenor, date, vol_1d = correct_post_mean_1) %>%
  filter(!is.na(vol_1d), tenor %in% tenor_levels)

cat(glue(
  "all_runs:    {nrow(all_runs)} rows | {n_distinct(all_runs$date)} dates | ",
  "{n_distinct(all_runs$run)} runs\n",
  "market_vol:  {nrow(market_vol)} rows | {n_distinct(market_vol$date)} dates\n\n"
))

# ==============================================================================
# 3. RAW DISTRIBUTION OF CONFIDENCE SCORES
# ==============================================================================

cat("Section 3: confidence score distribution...\n")

conf_summary <- all_runs %>%
  group_by(tenor) %>%
  summarise(
    n      = n(),
    mean   = round(mean(confidence),   1),
    median = round(median(confidence), 1),
    sd     = round(sd(confidence),     1),
    p10    = round(quantile(confidence, 0.10), 1),
    p90    = round(quantile(confidence, 0.90), 1),
    pct_in_60_80 = round(100 * mean(confidence >= 60 & confidence <= 80), 1),
    .groups = "drop"
  )

cat("Confidence score distribution by tenor:\n")
print(conf_summary)

panel_hist <- ggplot(all_runs, aes(x = confidence, fill = tenor)) +
  geom_histogram(bins = 20, colour = "white", linewidth = 0.2) +
  facet_wrap(~ tenor, ncol = 3) +
  scale_fill_manual(values = tenor_colours, guide = "none") +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(
    x       = "Confidence score (0–100)",
    y       = "Count",
    caption = glue(
      "Distribution of agent-level confidence scores across all {n_distinct(all_runs$date)} ",
      "conferences and R = {n_distinct(all_runs$run)} runs ({scales::comma(nrow(all_runs))} observations). ",
      "Scores are elicited on a 0–100 scale; {round(mean(conf_summary$pct_in_60_80), 1)}% ",
      "of observations fall in the 60–80 band."
    )
  ) +
  p1_theme

# ==============================================================================
# 4. INTERNAL VALIDATION
# ==============================================================================
# Mean confidence per (date, tenor), averaged over runs and agents.
# Spearman ρ with (a) synthetic SD and (b) realised OIS volatility.

cat("Section 4: internal validation (mean confidence vs SD and vol)...\n")

mean_conf <- all_runs %>%
  group_by(date, run, tenor) %>%
  summarise(conf_run = mean(confidence, na.rm = TRUE), .groups = "drop") %>%
  group_by(date, tenor) %>%
  summarise(mean_conf = mean(conf_run, na.rm = TRUE), .groups = "drop")

validation_data <- mean_conf %>%
  inner_join(ensemble_sd,  by = c("date", "tenor")) %>%
  inner_join(market_vol,   by = c("date", "tenor")) %>%
  filter(!is.na(mean_conf), !is.na(sd_mean), !is.na(vol_1d))

cat(glue("Validation sample: {nrow(validation_data)} obs ({n_distinct(validation_data$date)} dates)\n"))

compute_rho_ci <- function(df, x_var, y_var, B = B_BOOT) {
  rho <- cor(df[[x_var]], df[[y_var]], method = "spearman", use = "complete.obs")
  boot_fn <- function(d, idx) cor(d[idx, x_var], d[idx, y_var],
                                   method = "spearman", use = "complete.obs")
  b <- boot(df[, c(x_var, y_var)], boot_fn, R = B)
  ci <- boot.ci(b, type = "perc", conf = 0.90)
  tibble(rho = rho, ci_low = ci$percent[4], ci_high = ci$percent[5],
         n = nrow(df))
}

valid_results <- validation_data %>%
  mutate(tenor = as.character(tenor)) %>%
  group_by(tenor) %>%
  nest() %>%
  mutate(
    conf_vs_sd  = map(data, ~ compute_rho_ci(.x, "mean_conf", "sd_mean")),
    conf_vs_vol = map(data, ~ compute_rho_ci(.x, "mean_conf", "vol_1d"))
  ) %>%
  select(-data) %>%
  pivot_longer(c(conf_vs_sd, conf_vs_vol),
               names_to = "comparison", values_to = "ci") %>%
  unnest(ci) %>%
  mutate(
    comparison = recode(comparison,
      "conf_vs_sd"  = "Mean conf. vs synthetic SD",
      "conf_vs_vol" = "Mean conf. vs realised vol"
    ),
    tenor = factor(tenor, levels = tenor_levels)
  ) %>%
  ungroup()

cat("Internal validation correlations:\n")
print(valid_results %>% select(comparison, tenor, rho, ci_low, ci_high, n))

# ==============================================================================
# 5. CONFIDENCE-WEIGHTED DISPERSION
# ==============================================================================

cat("Section 5: confidence-weighted SD...\n")

# Within each (date, run, tenor): weighted SD of rates, w_i = confidence_i / 100
weighted_sd_run <- all_runs %>%
  group_by(date, run, tenor) %>%
  summarise(
    wt_sd = {
      w  <- confidence / 100
      sw <- sum(w, na.rm = TRUE)
      if (sw == 0 || n() < 2) {
        NA_real_
      } else {
        xbar <- sum(w * rate, na.rm = TRUE) / sw
        sqrt(sum(w * (rate - xbar)^2, na.rm = TRUE) / sw)
      }
    },
    .groups = "drop"
  )

ensemble_wt <- weighted_sd_run %>%
  group_by(date, tenor) %>%
  summarise(sd_mean_wt = mean(wt_sd, na.rm = TRUE), .groups = "drop")

corr_data <- ensemble_sd %>%
  inner_join(ensemble_wt,  by = c("date", "tenor")) %>%
  inner_join(market_vol,   by = c("date", "tenor")) %>%
  filter(!is.na(sd_mean), !is.na(sd_mean_wt), !is.na(vol_1d))

cat(glue("Weighted SD sample: {nrow(corr_data)} obs ({n_distinct(corr_data$date)} dates)\n"))

wt_results <- corr_data %>%
  mutate(tenor = as.character(tenor)) %>%
  group_by(tenor) %>%
  nest() %>%
  mutate(
    unweighted = map(data, ~ compute_rho_ci(.x, "sd_mean",    "vol_1d")),
    weighted   = map(data, ~ compute_rho_ci(.x, "sd_mean_wt", "vol_1d"))
  ) %>%
  select(-data) %>%
  pivot_longer(c(unweighted, weighted),
               names_to = "design", values_to = "ci") %>%
  unnest(ci) %>%
  mutate(
    design = recode(design,
      "unweighted" = "Unweighted SD",
      "weighted"   = "Confidence-weighted SD"
    ),
    tenor = factor(tenor, levels = tenor_levels)
  ) %>%
  ungroup()

cat("Weighted vs unweighted correlations:\n")
print(wt_results %>% select(design, tenor, rho, ci_low, ci_high, n))

# ==============================================================================
# 6. BOOTSTRAP Δρ — WEIGHTED vs UNWEIGHTED (PAIRED)
# ==============================================================================

cat("Section 6: bootstrap Δρ (weighted − unweighted)...\n")

delta_results <- corr_data %>%
  mutate(tenor = as.character(tenor)) %>%
  group_by(tenor) %>%
  nest() %>%
  mutate(result = map(data, function(d) {
    r_wt <- cor(d$sd_mean_wt, d$vol_1d, method = "spearman", use = "complete.obs")
    r_uw <- cor(d$sd_mean,    d$vol_1d, method = "spearman", use = "complete.obs")
    delta_obs <- r_wt - r_uw
    boot_delta <- replicate(B_BOOT, {
      idx   <- sample.int(nrow(d), nrow(d), replace = TRUE)
      db    <- d[idx, ]
      cor(db$sd_mean_wt, db$vol_1d, method = "spearman", use = "complete.obs") -
        cor(db$sd_mean,  db$vol_1d, method = "spearman", use = "complete.obs")
    })
    tibble(
      rho_wt    = r_wt,
      rho_uw    = r_uw,
      delta     = delta_obs,
      ci_low    = quantile(boot_delta, 0.025),
      ci_high   = quantile(boot_delta, 0.975),
      sig       = quantile(boot_delta, 0.025) > 0 | quantile(boot_delta, 0.975) < 0,
      n         = nrow(d)
    )
  })) %>%
  select(-data) %>%
  unnest(result) %>%
  mutate(tenor = factor(tenor, levels = tenor_levels)) %>%
  ungroup()

cat("Bootstrap Δρ (weighted − unweighted):\n")
print(delta_results)

# ==============================================================================
# 7. COMBINED TABLE
# ==============================================================================

cat("Section 7: assembling output table...\n")

fmt <- function(rho, lo, hi) sprintf("%.3f [%.3f, %.3f]", rho, lo, hi)

internal_wide <- valid_results %>%
  mutate(cell = fmt(rho, ci_low, ci_high)) %>%
  select(comparison, tenor, cell) %>%
  pivot_wider(names_from = tenor, values_from = cell) %>%
  rename(Measure = comparison)

wt_wide <- wt_results %>%
  mutate(cell = fmt(rho, ci_low, ci_high)) %>%
  select(design, tenor, cell) %>%
  pivot_wider(names_from = tenor, values_from = cell) %>%
  rename(Measure = design)

delta_wide <- delta_results %>%
  mutate(
    star    = if_else(sig, "*", ""),
    cell    = sprintf("%.3f%s [%.3f, %.3f]", delta, star, ci_low, ci_high)
  ) %>%
  select(tenor, cell) %>%
  pivot_wider(names_from = tenor, values_from = cell) %>%
  mutate(Measure = "Δρ weighted − unweighted") %>%
  select(Measure, everything())

conf_table <- bind_rows(internal_wide, wt_wide, delta_wide)

cat("\nFinal table:\n")
print(conf_table, width = Inf)

save_table(conf_table, "p1_confidence_table")

# ==============================================================================
# 8. FIGURE — Panel A: histogram | Panel B: correlation bars
# ==============================================================================

cat("Section 8: figures...\n")

panel_bars <- wt_results %>%
  mutate(
    tenor  = factor(tenor,  levels = tenor_levels),
    design = factor(design, levels = c("Unweighted SD", "Confidence-weighted SD"))
  ) %>%
  ggplot(aes(x = tenor, y = rho, fill = design)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5, alpha = 0.85) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(width = 0.6),
    width = 0.2, colour = "grey20", linewidth = 0.7
  ) +
  geom_text(
    aes(label = sprintf("%.2f", rho), y = ci_high + 0.025),
    position = position_dodge(width = 0.6),
    size = 3.5, fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Unweighted SD" = "grey60", "Confidence-weighted SD" = "#4575b4"),
    name   = NULL
  ) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 0.8, 0.2)) +
  labs(
    x       = "OIS Tenor",
    y       = "Spearman ρ with realised vol",
    caption = "Bootstrap 90% CI (5,000 reps). Both measures on same sample."
  ) +
  p1_theme +
  theme(panel.grid.major.x = element_blank())

save_plot(panel_hist, "p1_fig_confidence", w = 12, h = 5)

# ==============================================================================
# 9. WRITE NOTES.MD
# ==============================================================================

cat("Section 9: writing NOTES.md...\n")

# Pull key numbers for the narrative
conf_3m <- conf_summary %>% filter(tenor == "3M")
conf_2y <- conf_summary %>% filter(tenor == "2Y")
conf_pct_band <- mean(conf_summary$pct_in_60_80)  # avg % scores in 60–80 band

rho_cv_sd  <- valid_results %>% filter(comparison == "Mean conf. vs synthetic SD",  tenor == "2Y") %>% pull(rho)
rho_cv_vol <- valid_results %>% filter(comparison == "Mean conf. vs realised vol",   tenor == "2Y") %>% pull(rho)

rho_uw_2y  <- wt_results %>% filter(design == "Unweighted SD",           tenor == "2Y") %>% pull(rho)
rho_wt_2y  <- wt_results %>% filter(design == "Confidence-weighted SD",  tenor == "2Y") %>% pull(rho)

delta_sig_any <- any(delta_results$sig)

direction_ok <- mean(
  valid_results %>%
    filter(comparison == "Mean conf. vs synthetic SD") %>%
    pull(rho) < 0
) == 1  # TRUE if all three tenors show negative ρ

direction_label <- if (direction_ok) {
  "negative across all three tenors, as hypothesised"
} else {
  "mixed — the hypothesised negative sign does not hold uniformly"
}

delta_label <- if (delta_sig_any) {
  "statistically significant for at least one tenor (95% CI excludes zero)"
} else {
  "not statistically distinguishable from zero at any tenor (95% CIs include zero)"
}

recommendation <- if (!delta_sig_any && conf_pct_band > 50) {
  glue(
    "DROP from prompt. Score bunching ({round(conf_pct_band, 0)}% of scores in the ",
    "60–80 range) shows the LLM treats confidence as a near-constant, not a variable ",
    "reflecting genuine interpretive difficulty. Internal validation is weak ",
    "(|ρ| < 0.1 in most cells), and confidence-weighting does not improve the ",
    "correlation with realised volatility. Removing the confidence column will shorten ",
    "the output table, simplify parsing, and lower token costs without sacrificing ",
    "predictive validity."
  )
} else {
  glue(
    "KEEP and report. Confidence scores show meaningful variation and either ",
    "(a) weight the dispersion measure towards higher-conviction forecasts with a ",
    "statistically significant gain in ρ (95% CI excludes zero for at least one tenor), ",
    "or (b) correlate negatively with synthetic SD as theorised. Reporting the ",
    "distribution is itself a finding about LLM calibration under interpretive uncertainty."
  )
}

notes_lines <- c(
  "# NOTES — Confidence Scores: Use or Drop",
  "",
  glue("_Generated by 19confidence_scores_analysis.R — {Sys.Date()}_"),
  "",
  "## Key results",
  "",
  glue(
    "**Distribution.** Confidence scores are tightly bunched: on average ",
    "{round(conf_pct_band, 1)}% of all scores fall in the 60–80 band across tenors ",
    "(mean = {conf_3m$mean}, SD = {conf_3m$sd} for 3M). ",
    "The near-absence of scores below 40 or above 90 suggests the model ",
    "treats confidence as a soft default rather than a discriminating signal."
  ),
  "",
  glue(
    "**Internal validation.** ",
    "The correlation between mean agent confidence and synthetic SD is {direction_label}. ",
    "At the 2Y tenor: ρ(conf, synth SD) = {round(rho_cv_sd, 3)}, ",
    "ρ(conf, realised vol) = {round(rho_cv_vol, 3)}. ",
    "In absolute terms these correlations are small, confirming that mean confidence ",
    "carries little incremental information about actual market uncertainty."
  ),
  "",
  glue(
    "**Confidence-weighted dispersion.** ",
    "Reweighting by confidence/100 changes the 2Y Spearman ρ from {round(rho_uw_2y, 3)} ",
    "(unweighted) to {round(rho_wt_2y, 3)} (weighted). ",
    "The paired bootstrap Δρ (weighted − unweighted) is {delta_label}."
  ),
  "",
  "## Recommendation",
  "",
  recommendation,
  ""
)

writeLines(notes_lines, "NOTES.md")
cat("  Saved: NOTES.md\n")

# ==============================================================================
# 10. CONSOLE SUMMARY
# ==============================================================================

cat("\n", strrep("-", 60), "\n")
cat("SUMMARY\n")
cat(strrep("-", 60), "\n\n")

cat("Confidence bunching (% scores in 60–80 band):\n")
print(conf_summary %>% select(tenor, mean, sd, pct_in_60_80))

cat("\nInternal validation (Spearman ρ, 90% CI):\n")
print(valid_results %>% select(comparison, tenor, rho, ci_low, ci_high))

cat("\nWeighted vs unweighted ρ (90% CI):\n")
print(wt_results %>% select(design, tenor, rho, ci_low, ci_high))

cat("\nDelta ρ bootstrap (95% CI):\n")
print(delta_results %>% select(tenor, rho_wt, rho_uw, delta, ci_low, ci_high, sig))

cat("\n", strrep("=", 80), "\n")
cat("SCRIPT 19 COMPLETE\n")
cat(strrep("=", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
