#===============================================================================
# POST INTRO ENSEMBLE P1 — Full Transcript vs Introductory Statement: Results
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Generates all paper figures for Counterfactual C (intro vs full transcript).
#
#   Key question: is the bulk of trader disagreement driven by the Q&A session,
#   or is it already present in the introductory statement?
#
#   The central measure is intro_share = intro_SD / full_SD (ensemble mean across
#   10 runs for both versions). intro_share ≈ 1 → intro explains most disagreement;
#   intro_share ≈ 0 → Q&A is the dominant driver.
#
#   Vol regime classification: tenor-specific top tercile of full-transcript
#   ensemble SD → "High-vol"; bottom two thirds → "Normal". Classification uses
#   synthetic disagreement only (no actual market data).
#
# Inputs (READ ONLY):
#   ../intermediate_data/intro_ensemble_p1/parsed/intro_ensemble.rds
#   ../intermediate_data/intro_ensemble_p1/parsed/intro_treatment_effect.rds
#   ../intermediate_data/range_difference_df.rds
#
# Outputs:
#   output/figures/intro_ensemble/
#     intro_fig1_intro_share        — intro_SD/full_SD distribution, pooled
#     intro_fig2_level_comparison   — intro vs full SD distributions, pooled
#     intro_fig3_intro_share_regime — intro_SD/full_SD by vol regime × tenor
#     intro_fig4_level_regime       — level comparison by vol regime × tenor
#   intermediate_data/intro_ensemble_p1/post/
#     intro_share_summary  — pooled + by-regime stats  (rds + xlsx)
#     intro_correlations                               (rds + xlsx)
#
# Usage: source("post_intro_ensemble_p1.R")  from code/ directory
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("POST INTRO ENSEMBLE P1 — Full Transcript vs Introductory Statement\n")
cat(strrep("=", 80), "\n\n")

# ==============================================================================
# 1. SETUP
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, writexl, scales, showtext, boot, glue
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

fig_dir  <- "../output/figures/intro_ensemble"
data_dir <- "../intermediate_data/intro_ensemble_p1/post"
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

assert_intro <- function(path) {
  bn <- basename(path)
  if (!startsWith(bn, "intro_")) stop(glue("Output violates intro_ prefix: '{bn}'"))
  invisible(path)
}

planned_stems <- c(
  file.path(data_dir, "intro_share_summary"),
  file.path(data_dir, "intro_correlations"),
  file.path(fig_dir,  "intro_fig1_intro_share"),
  file.path(fig_dir,  "intro_fig2_level_comparison"),
  file.path(fig_dir,  "intro_fig3_intro_share_regime"),
  file.path(fig_dir,  "intro_fig4_level_regime")
)
invisible(lapply(planned_stems, assert_intro))
cat("Prefix assertion passed for all", length(planned_stems), "output paths.\n\n")

tenor_levels    <- c("3M", "2Y", "10Y")
tenor_colours   <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")
version_colours <- c("full" = "#4575b4", "intro" = "#d73027")
version_labels  <- c("full" = "Full transcript", "intro" = "Intro only")

p1_theme <- theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title       = element_blank(),
    plot.caption     = element_text(hjust = 0, size = 9, colour = "grey30"),
    strip.text       = element_text(face = "bold"),
    axis.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

save_plot <- function(plot, stem, w = 12, h = 8) {
  assert_intro(paste0(stem, ".pdf"))
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
  assert_intro(paste0(stem, ".rds"))
  saveRDS(df, file.path(data_dir, paste0(stem, ".rds")))
  writexl::write_xlsx(df, file.path(data_dir, paste0(stem, ".xlsx")))
  cat(glue("  Saved: {stem}.rds/.xlsx\n"))
  invisible(df)
}

bootstrap_correlation <- function(data, indices, x_var, y_var, method) {
  d <- data[indices, ]
  cor(d[[x_var]], d[[y_var]], method = method, use = "complete.obs")
}

compute_bootstrap_ci <- function(data, x_var, y_var, method, n_bootstrap = 5000) {
  if (nrow(data) < 10) {
    return(data.frame(rho = NA_real_, ci_low = NA_real_, ci_high = NA_real_, n = nrow(data)))
  }
  rho      <- cor(data[[x_var]], data[[y_var]], method = method, use = "complete.obs")
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

parsed_dir <- "../intermediate_data/intro_ensemble_p1/parsed"

# ensemble: one row per (date, version, tenor)
# sd_mean = average within-run SD across R = 10 runs — the ensemble mean
ensemble <- readRDS(file.path(parsed_dir, "intro_ensemble.rds")) %>%
  mutate(
    date    = as.Date(date),
    tenor   = factor(tenor, levels = tenor_levels),
    version = factor(version, levels = c("full", "intro"))
  ) %>%
  filter(tenor %in% tenor_levels)

# treatment_effect: one row per (date, tenor), columns sd_mean_full / sd_mean_intro
treatment_effect <- readRDS(file.path(parsed_dir, "intro_treatment_effect.rds")) %>%
  mutate(
    date  = as.Date(date),
    tenor = factor(tenor, levels = tenor_levels)
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
  "ensemble:         {nrow(ensemble)} rows | {n_distinct(ensemble$date)} dates\n",
  "treatment_effect: {nrow(treatment_effect)} rows\n",
  "market_vol:       {nrow(market_vol)} rows | {n_distinct(market_vol$date)} dates\n\n"
))

# ==============================================================================
# 3. DERIVE INTRO SHARE + VOL REGIME CLASSIFICATION
# ==============================================================================
# intro_share = intro_SD / full_SD (ensemble mean for both, R=10).
# Bounded to [0, 2] to remove extreme artefacts where full_SD ≈ 0.
#
# Vol regime: tenor-specific top tercile of full_SD → "High-vol"; rest → "Normal".
# Classification uses only synthetic disagreement (no actual market data).

cat("Deriving intro_share and vol regime classification...\n")

share_data <- treatment_effect %>%
  mutate(
    intro_share = if_else(
      sd_mean_full > 0,
      sd_mean_intro / sd_mean_full,
      NA_real_
    )
  ) %>%
  filter(!is.na(intro_share), is.finite(intro_share)) %>%
  filter(intro_share >= 0, intro_share <= 2)

# Tenor-specific top-tercile threshold from full-transcript ensemble SD
share_data <- share_data %>%
  group_by(tenor) %>%
  mutate(
    vol_threshold = quantile(sd_mean_full, 2/3, na.rm = TRUE),
    regime        = factor(
      if_else(sd_mean_full >= vol_threshold, "High-vol", "Normal"),
      levels = c("High-vol", "Normal")
    )
  ) %>%
  ungroup()

cat(glue(
  "Regime breakdown:\n"
))
print(share_data %>% count(tenor, regime))
cat("\n")

# Summary helper — used for both pooled and by-regime rows
summarise_share <- function(df) {
  df %>%
    summarise(
      n_conferences      = n(),
      mean_intro_share   = round(mean(intro_share,   na.rm = TRUE), 3),
      median_intro_share = round(median(intro_share, na.rm = TRUE), 3),
      pct_above_75       = round(mean(intro_share > 0.75, na.rm = TRUE) * 100, 1),
      pct_below_50       = round(mean(intro_share < 0.50, na.rm = TRUE) * 100, 1),
      mean_sd_full       = round(mean(sd_mean_full,  na.rm = TRUE), 4),
      mean_sd_intro      = round(mean(sd_mean_intro, na.rm = TRUE), 4),
      .groups = "drop"
    )
}

share_summary_pooled <- share_data %>%
  group_by(tenor) %>%
  summarise_share() %>%
  mutate(regime = "Pooled", .after = tenor)

share_summary_regime <- share_data %>%
  group_by(tenor, regime) %>%
  summarise_share()

share_summary <- bind_rows(share_summary_pooled, share_summary_regime) %>%
  arrange(tenor, regime)

cat("\nIntro share summary (pooled + by regime):\n")
print(share_summary)
cat("\n")

save_table(
  share_summary %>% mutate(across(where(is.factor), as.character)),
  "intro_share_summary"
)

# ==============================================================================
# 4. FIGURE 1: INTRO SHARE DISTRIBUTION — main result
# ==============================================================================
# Each point = one ECB conference × tenor.
# Distribution shows: how much of full-transcript disagreement was already in
# the intro statement? Values near 1 = intro explains most; near 0 = Q&A drives.

cat("Figure 1: intro share distribution (main result)...\n")

n_conf_share <- n_distinct(share_data$date)

# Per-tenor annotation: median intro share
share_labels <- share_summary %>%
  mutate(label = glue("median = {round(median_intro_share, 2)}"))

fig1 <- ggplot(share_data, aes(x = tenor, y = intro_share, fill = tenor)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey60", linewidth = 0.4) +
  geom_violin(alpha = 0.3, colour = NA, width = 0.7) +
  geom_boxplot(width = 0.22, outlier.shape = 21, outlier.size = 1.5,
               outlier.alpha = 0.5, colour = "grey20", fill = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", colour = "grey20") +
  geom_text(
    data    = share_labels,
    mapping = aes(x = tenor, y = -0.08, label = label),
    inherit.aes = FALSE,
    size = 3.5, colour = "grey30"
  ) +
  scale_fill_manual(values = tenor_colours, guide = "none") +
  scale_y_continuous(
    limits = c(-0.15, 2),
    breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    x       = "OIS Tenor",
    y       = "Intro SD / Full SD",
    caption = glue(
      "Each observation = one ECB conference ({n_conf_share} total, high-vol sample). ",
      "Ratio uses ensemble mean SD across R = 10 runs for both versions. ",
      "Dashed line = 1 (intro explains all disagreement). ",
      "Dotted line = 0.5 (intro explains half). ",
      "Diamond = mean; box = IQR."
    )
  ) +
  p1_theme +
  theme(panel.grid.major.x = element_blank())

save_plot(fig1, "intro_fig1_intro_share", w = 8, h = 7)

# ==============================================================================
# 5. FIGURE 2: LEVEL COMPARISON — intro vs full SD distributions
# ==============================================================================
# Shows the absolute magnitude of disagreement in each version, by tenor.
# Paired structure (same conferences) reveals whether the gap is large or small
# relative to the overall level of disagreement.

cat("Figure 2: level comparison intro vs full...\n")

level_data <- ensemble %>%
  mutate(version = factor(version, levels = c("intro", "full"),
                          labels = c("Intro only", "Full transcript")))

n_conf_level <- n_distinct(level_data$date)

fig2 <- ggplot(level_data, aes(x = version, y = sd_mean, fill = version)) +
  geom_violin(alpha = 0.3, colour = NA, width = 0.7) +
  geom_boxplot(width = 0.22, outlier.shape = 21, outlier.size = 1.5,
               outlier.alpha = 0.5, colour = "grey20", fill = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", colour = "grey20") +
  facet_wrap(~ tenor, ncol = 3, scales = "free_y") +
  scale_fill_manual(
    values = c("Intro only" = "#d73027", "Full transcript" = "#4575b4"),
    name   = NULL
  ) +
  labs(
    x       = NULL,
    y       = "Ensemble SD of predicted rate (avg across R = 10 runs)",
    caption = glue(
      "Each observation = one ECB conference ({n_conf_level} total). ",
      "SD is the ensemble mean across R = 10 runs. Diamond = mean; box = IQR."
    )
  ) +
  p1_theme +
  theme(
    axis.text.x  = element_text(size = 9),
    panel.border = element_rect(colour = "grey80", fill = NA)
  )

save_plot(fig2, "intro_fig2_level_comparison", w = 12, h = 5)

# ==============================================================================
# 6. FIGURE 3: INTRO SHARE BY VOL REGIME
# ==============================================================================
# Key result: does the Q&A matter more in normal times than in high-vol?
# In high-vol conferences the intro already carries the big signal; in normal
# times the Q&A may be the only place where subtle signals emerge.

cat("Figure 3: intro share by vol regime...\n")

n_conf_regime <- share_data %>%
  count(tenor, regime) %>%
  mutate(label = glue("n = {n}"))

# Per-panel medians for annotation
regime_medians <- share_data %>%
  group_by(tenor, regime) %>%
  summarise(
    med   = median(intro_share, na.rm = TRUE),
    label = glue("median = {round(med, 2)}"),
    .groups = "drop"
  )

fig3 <- ggplot(share_data, aes(x = regime, y = intro_share, fill = regime)) +
  geom_hline(yintercept = 1,   linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey60", linewidth = 0.4) +
  geom_violin(alpha = 0.3, colour = NA, width = 0.7) +
  geom_boxplot(width = 0.22, outlier.shape = 21, outlier.size = 1.5,
               outlier.alpha = 0.5, colour = "grey20", fill = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", colour = "grey20") +
  geom_text(
    data    = regime_medians,
    mapping = aes(x = regime, y = -0.1, label = label),
    inherit.aes = FALSE,
    size = 3.2, colour = "grey30"
  ) +
  facet_wrap(~ tenor, ncol = 3) +
  scale_fill_manual(
    values = c("High-vol" = "#d73027", "Normal" = "#91bfdb"),
    name   = NULL
  ) +
  scale_y_continuous(
    limits = c(-0.18, 2),
    breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5),
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    x       = NULL,
    y       = "Intro SD / Full SD",
    caption = glue(
      "Regime = tenor-specific top tercile of full-transcript ensemble SD (High-vol) vs rest (Normal). ",
      "Dashed = 1 (intro explains all). Dotted = 0.5 (intro explains half). Diamond = mean."
    )
  ) +
  p1_theme +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border       = element_rect(colour = "grey80", fill = NA)
  )

save_plot(fig3, "intro_fig3_intro_share_regime", w = 12, h = 7)

# ==============================================================================
# 7. FIGURE 4: LEVEL COMPARISON BY VOL REGIME
# ==============================================================================
# Shows absolute SD magnitudes for intro vs full within each regime.
# Helps distinguish whether the regime difference in intro_share reflects
# higher full_SD, lower intro_SD, or both.

cat("Figure 4: level comparison by vol regime...\n")

level_regime <- ensemble %>%
  inner_join(
    share_data %>% select(date, tenor, regime) %>% distinct(),
    by = c("date", "tenor")
  ) %>%
  mutate(version = factor(version, levels = c("intro", "full"),
                          labels = c("Intro only", "Full transcript")))

n_conf_level_regime <- n_distinct(level_regime$date)

fig4 <- ggplot(level_regime, aes(x = version, y = sd_mean, fill = version)) +
  geom_violin(alpha = 0.3, colour = NA, width = 0.7) +
  geom_boxplot(width = 0.22, outlier.shape = 21, outlier.size = 1.5,
               outlier.alpha = 0.5, colour = "grey20", fill = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white", colour = "grey20") +
  facet_grid(regime ~ tenor, scales = "free_y") +
  scale_fill_manual(
    values = c("Intro only" = "#d73027", "Full transcript" = "#4575b4"),
    name   = NULL
  ) +
  labs(
    x       = NULL,
    y       = "Ensemble SD of predicted rate (avg across R = 10 runs)",
    caption = glue(
      "Each observation = one ECB conference ({n_conf_level_regime} total). ",
      "Rows = vol regime (tenor-specific top tercile of full_SD). Diamond = mean."
    )
  ) +
  p1_theme +
  theme(
    axis.text.x  = element_text(size = 8),
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.background = element_rect(fill = "grey95", colour = NA)
  )

save_plot(fig4, "intro_fig4_level_regime", w = 12, h = 7)

# ==============================================================================
# 8. SAVE CORRELATION TABLE (market vol vs synthetic — kept for completeness)
# ==============================================================================

corr_full <- ensemble %>%
  filter(version == "full") %>%
  inner_join(market_vol, by = c("date", "tenor")) %>%
  filter(!is.na(sd_mean), !is.na(vol_1d))

corr_intro <- ensemble %>%
  filter(version == "intro") %>%
  inner_join(market_vol, by = c("date", "tenor")) %>%
  filter(!is.na(sd_mean), !is.na(vol_1d))

common_keys <- inner_join(
  corr_full  %>% select(date, tenor) %>% distinct(),
  corr_intro %>% select(date, tenor) %>% distinct(),
  by = c("date", "tenor")
)
corr_full  <- semi_join(corr_full,  common_keys, by = c("date", "tenor"))
corr_intro <- semi_join(corr_intro, common_keys, by = c("date", "tenor"))

set.seed(42)

compute_version_corr <- function(df, label) {
  df %>%
    rename(sd_version = sd_mean) %>%
    mutate(tenor = as.character(tenor)) %>%
    group_by(tenor) %>%
    nest() %>%
    mutate(ci = map(data, ~ compute_bootstrap_ci(.x, "sd_version", "vol_1d", "spearman"))) %>%
    select(-data) %>%
    unnest(ci) %>%
    mutate(version = label) %>%
    ungroup()
}

corr_results <- bind_rows(
  compute_version_corr(corr_full,  "Full transcript"),
  compute_version_corr(corr_intro, "Intro only")
) %>%
  mutate(
    tenor   = factor(tenor,   levels = tenor_levels),
    version = factor(version, levels = c("Full transcript", "Intro only"))
  )

save_table(
  corr_results %>% mutate(across(where(is.factor), as.character)),
  "intro_correlations"
)

# ==============================================================================
# 9. SANITY OUTPUT (printed only)
# ==============================================================================

cat("\n", strrep("-", 60), "\n")
cat("SANITY OUTPUT\n")
cat(strrep("-", 60), "\n\n")

cat("Mean ensemble SD (R=10 avg) by version and tenor:\n")
ensemble %>%
  group_by(version, tenor) %>%
  summarise(
    mean_sd   = round(mean(sd_mean,   na.rm = TRUE), 4),
    median_sd = round(median(sd_mean, na.rm = TRUE), 4),
    n_conf    = n(),
    .groups   = "drop"
  ) %>%
  print()

cat("\nIntro share summary — pooled + by regime:\n")
print(share_summary)

cat("\nCorrelation Δρ (Full − Intro):\n")
corr_results %>%
  select(tenor, version, rho) %>%
  pivot_wider(names_from = version, values_from = rho) %>%
  mutate(delta_rho = round(`Full transcript` - `Intro only`, 4)) %>%
  print()

cat("\n", strrep("=", 80), "\n")
cat("POST INTRO ENSEMBLE P1 COMPLETE\n")
cat(strrep("=", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
