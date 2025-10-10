# ============================================================================
# EXPERIMENT A ANALYSIS: ChatGPT edits → Gemini evaluates
# ============================================================================
# Analysis of disagreement reduction from counterfactual editing experiment

# Load libraries ----
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, readxl, writexl, RColorBrewer, showtext, patchwork, glue
)

# Configuration ----
setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# Enable Segoe UI font
if (!("Segoe UI" %in% font_families())) {
  font_add("Segoe UI", regular = "segoeui.ttf")
}
showtext_auto()

# Directories
OUTPUT_DIR <- "../intermediate_data/counterfactual_cross_llm_high_vol"
PARSED_DIR <- file.path(OUTPUT_DIR, "parsed_data")
FIGURES_DIR <- "../output/figures/experiment_a"

# Create figure directory
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD DATA - EXPERIMENT A ONLY
# =============================================================================

cat(crayon::blue$bold("\n=== LOADING EXPERIMENT A DATA ===\n\n"))

# Load Experiment A results
results_exp_a <- read_csv(file.path(PARSED_DIR, "exp_a_results.csv")) %>%
  mutate(
    date = as.Date(date),
    tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))
  )

# Check data
cat(crayon::green(glue("Loaded {nrow(results_exp_a)} observations\n")))
cat(crayon::green(glue("Date range: {min(results_exp_a$date)} to {max(results_exp_a$date)}\n\n")))

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat(crayon::cyan$bold("=== SUMMARY STATISTICS ===\n"))

# Overall summary by tenor
summary_stats <- results_exp_a %>%
  drop_na(reduction_pct) %>%
  group_by(tenor) %>%
  summarise(
    n_conferences = n(),
    mean_reduction_pct = mean(reduction_pct),
    median_reduction_pct = median(reduction_pct),
    sd_reduction = sd(reduction_pct),
    se = sd_reduction / sqrt(n_conferences),
    mean_baseline = mean(baseline_std),
    mean_edited = mean(edited_std),
    mean_reduction_abs = mean(reduction_abs),
    .groups = "drop"
  )

print(summary_stats)

# Count conferences with meaningful changes
# Define threshold: >5% reduction
threshold_pct <- 5

change_counts <- results_exp_a %>%
  drop_na(reduction_pct) %>%
  mutate(has_reduction = reduction_pct > threshold_pct) %>%
  group_by(tenor) %>%
  summarise(
    n_total = n(),
    n_with_reduction = sum(has_reduction),
    pct_with_reduction = (n_with_reduction / n_total) * 100,
    n_no_change = sum(abs(reduction_pct) <= threshold_pct),
    pct_no_change = (n_no_change / n_total) * 100,
    n_with_increase = sum(reduction_pct < -threshold_pct),
    pct_with_increase = (n_with_increase / n_total) * 100,
    .groups = "drop"
  )

cat(crayon::cyan$bold("\n=== CHANGE DISTRIBUTION ===\n"))
cat(glue("Threshold: >{threshold_pct}% reduction\n\n"))
print(change_counts)

# Statistical tests
cat(crayon::cyan$bold("\n=== STATISTICAL TESTS ===\n"))

t_test_results <- results_exp_a %>%
  drop_na(reduction_pct) %>%
  group_by(tenor) %>%
  summarise(
    mean_reduction = mean(reduction_pct),
    t_stat = t.test(reduction_pct, mu = 0, alternative = "greater")$statistic,
    p_value = t.test(reduction_pct, mu = 0, alternative = "greater")$p.value,
    ci_lower = t.test(reduction_pct, mu = 0)$conf.int[1],
    ci_upper = t.test(reduction_pct, mu = 0)$conf.int[2],
    .groups = "drop"
  )

print(t_test_results)

# T-tests for absolute reduction (in basis points)
t_test_results_abs <- results_exp_a %>%
  drop_na(reduction_abs) %>%
  group_by(tenor) %>%
  summarise(
    mean_reduction_bps = mean(reduction_abs) * 100,  # Convert to basis points
    t_stat = t.test(reduction_abs, mu = 0, alternative = "greater")$statistic,
    p_value = t.test(reduction_abs, mu = 0, alternative = "greater")$p.value,
    ci_lower = t.test(reduction_abs, mu = 0)$conf.int[1] * 100,  # Convert to bps
    ci_upper = t.test(reduction_abs, mu = 0)$conf.int[2] * 100,  # Convert to bps
    .groups = "drop"
  )

# =============================================================================
# PLOT 1: MEAN DISAGREEMENT REDUCTION BY TENOR
# =============================================================================

cat(crayon::blue$bold("\n=== CREATING PLOTS ===\n"))

color_palette <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

p_mean_reduction <- summary_stats %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) %>%
  ggplot(aes(x = tenor, y = mean_reduction_pct, fill = tenor)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_reduction_pct - 1.96 * se, 
        ymax = mean_reduction_pct + 1.96 * se),
    width = 0.25,
    linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = color_palette) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "%",
    caption = ""
  ) +
  theme_minimal(base_family = "Segoe UI", base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey50", margin = margin(t = 10)),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p_mean_reduction)

ggsave(
  file.path(FIGURES_DIR, "exp_a_mean_reduction.pdf"),
  plot = p_mean_reduction,
  dpi = 320,
  width = 10,
  height = 7,
  bg = "white"
)

p_mean_reduction_bps <- t_test_results_abs %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) %>%
  ggplot(aes(x = tenor, y = mean_reduction_bps, fill = tenor)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ci_lower, 
        ymax = ci_upper),
    width = 0.25,
    linewidth = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = color_palette) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "Bps",
    caption = ""
  ) +
  theme_minimal(base_family = "Segoe UI", base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey50", margin = margin(t = 10)),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p_mean_reduction_bps)

ggsave(
  file.path(FIGURES_DIR, "exp_a_mean_reduction_bps.pdf"),
  plot = p_mean_reduction_bps,
  dpi = 320,
  width = 10,
  height = 7,
  bg = "white"
)


# =============================================================================
# PLOT 2: NUMBER OF CONFERENCES WITH MEANINGFUL CHANGES
# =============================================================================

# Reshape data for stacked bar chart
change_data_long <- change_counts %>%
  select(tenor, 
         Reduction = n_with_reduction,
         `No Change` = n_no_change,
         Increase = n_with_increase) %>%
  pivot_longer(cols = c(Reduction, `No Change`, Increase),
               names_to = "change_type",
               values_to = "count") %>%
  mutate(
    change_type = factor(change_type, levels = c("Increase", "No Change", "Reduction"))
  )

# Create stacked bar chart
p_change_counts <- change_data_long %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) %>%
  ggplot(aes(x = tenor, y = count, fill = change_type)) +
  geom_col(width = 0.6) +
  geom_text(
    data = change_counts,
    aes(x = tenor, y = n_total, label = n_total, fill = NULL),
    vjust = -0.5,
    size = 5,
    family = "Segoe UI",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Reduction" = "#2ca25f",    # Green for reduction
      "No Change" = "#fee5d9",    # Light for no change
      "Increase" = "#de2d26"      # Red for increase
    )
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "OIS Tenor",
    y = "Number of Conferences",
    fill = "Change Type",
    caption = ""
  ) +
  theme_minimal(base_family = "Segoe UI", base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey50", margin = margin(t = 10)),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

print(p_change_counts)

ggsave(
  file.path(FIGURES_DIR, "exp_a_change_distribution.pdf"),
  plot = p_change_counts,
  dpi = 320,
  width = 10,
  height = 7,
  bg = "white"
)

# =============================================================================
# PLOT 5: TIME SERIES OF REDUCTION
# =============================================================================

p_time_series <- results_exp_a %>%
  drop_na(reduction_pct) %>%
  ggplot(aes(x = date, y = reduction_pct, color = tenor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = threshold_pct, linetype = "dotted", color = "grey60", alpha = 0.7) +
  geom_hline(yintercept = -threshold_pct, linetype = "dotted", color = "grey60", alpha = 0.7) +
  geom_line(linewidth = 0.6, alpha = 0.6) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~tenor, ncol = 1, scales = "free_y") +
  scale_color_manual(values = color_palette, guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Disagreement Reduction Over Time",
    subtitle = "Experiment A: ChatGPT edits → Gemini evaluates",
    x = NULL,
    y = "Disagreement Reduction (%)",
    caption = "Note: Dotted lines show ±5% threshold for meaningful change.\nPositive values = reduction in disagreement."
  ) +
  theme_minimal(base_family = "Segoe UI", base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey50", margin = margin(t = 10)),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA)
  )

print(p_time_series)

ggsave(
  file.path(FIGURES_DIR, "exp_a_time_series.pdf"),
  plot = p_time_series,
  dpi = 320,
  width = 12,
  height = 10,
  bg = "white"
)
# =============================================================================
# PLOT 5: HEATMAP OF REDUCTION EFFECTIVENESS BY CONFERENCE
# =============================================================================
p_heatmap <- results_with_labels %>%
  ggplot(aes(x = reorder(date_label, conf_num), y = tenor, fill = reduction_category)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c(
      "Strong Increase (≥20%)" = "#b2182b",
      "Moderate Increase (10-20%)" = "#ef8a62",
      "Mild Increase (5-10%)" = "#f7bf87ff",
      "No Change (±5%)" = "#f7f7f7",
      "Mild Reduction (5-10%)" = "#d1e5f0",
      "Moderate Reduction (10-20%)" = "#67a9cf",
      "Strong Reduction (≥20%)" = "#2166ac"
    ),
    drop = FALSE
  ) +
  scale_x_discrete(
    breaks = date_breaks$date_label[date_breaks$show_label],
    labels = date_breaks$date_label[date_breaks$show_label]
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "Press Conference Date",
    y = "",
    fill = "",
    caption = ""
  ) +
  theme_minimal(base_family = "Segoe UI", base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey50", margin = margin(t = 10)),
    axis.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size=18),
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    legend.position = "bottom",
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(1.5, "cm"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA),
    axis.ticks.x = element_line(color = "grey80", linewidth = 0.3)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

print(p_heatmap)

# Reasonable proportions: wide enough to read dates, tall enough for 3 tenors
ggsave(
  file.path(FIGURES_DIR, "exp_a_heatmap.pdf"),
  plot = p_heatmap,
  dpi = 320,
  width = 16,   # Wide but reasonable
  height = 6,   # Tall enough for 3 rows
  bg = "white"
)

# =============================================================================
# PLOT 8: CHARACTERISTICS OF SUCCESSFUL VS FAILED EDITS
# =============================================================================

# Prepare data for radar/spider chart comparison
radar_data <- comparison_stats %>%
  select(outcome_binary, mean_flesch, mean_hedging_rate, 
         mean_temporal_rate, mean_baseline_std) %>%
  pivot_longer(cols = -outcome_binary, 
               names_to = "characteristic", 
               values_to = "value") %>%
  mutate(
    characteristic = case_when(
      characteristic == "mean_flesch" ~ "Readability\n(Flesch Score)",
      characteristic == "mean_hedging_rate" ~ "Hedging Language\n(per 1000 words)",
      characteristic == "mean_temporal_rate" ~ "Temporal Vagueness\n(per 1000 words)",
      characteristic == "mean_baseline_std" ~ "Baseline\nDisagreement"
    )
  ) %>%
  group_by(characteristic) %>%
  mutate(
    # Normalize to 0-100 scale for comparison
    value_normalized = (value - min(value)) / (max(value) - min(value)) * 100
  ) %>%
  ungroup()

p_comparison <- radar_data %>%
  ggplot(aes(x = characteristic, y = value, fill = outcome_binary)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  scale_fill_manual(
    values = c("Success" = "#2166ac", "Failure" = "#b2182b"),
    labels = c("Success" = "Successful Edits (≥10% reduction)",
               "Failure" = "Failed Edits (≥5% increase)")
  ) +
  facet_wrap(~characteristic, scales = "free_y", ncol = 4) +
  labs(
    title = "Characteristics of Successful vs Failed Linguistic Edits",
    subtitle = "Comparing edited texts that achieved disagreement reduction vs those that didn't",
    x = NULL,
    y = "Mean Value",
    fill = NULL,
    caption = "Note: Based on 2Y OIS tenor. Higher Flesch = more readable. Higher hedging/temporal = more ambiguous."
  ) +
  theme_minimal(base_family = "Segoe UI", base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey50", margin = margin(t = 10)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

print(p_comparison)

ggsave(
  file.path(FIGURES_DIR, "exp_a_success_vs_failure_characteristics.pdf"),
  plot = p_comparison,
  dpi = 320,
  width = 14,
  height = 7,
  bg = "white"
)

ggsave(
  file.path(FIGURES_DIR, "exp_a_baseline_vs_reduction.pdf"),
  plot = p_baseline_vs_reduction,
  dpi = 320,
  width = 11,
  height = 8,
  bg = "white"
)

# =============================================================================
# EXPORT SUMMARY TABLES
# =============================================================================

cat(crayon::cyan$bold("\n=== EXPORTING RESULTS ===\n"))

# Export all tables
write_xlsx(
  list(
    "Summary_Statistics" = summary_stats,
    "Change_Distribution" = change_counts,
    "Statistical_Tests" = t_test_results,
    "Detailed_Results" = results_exp_a
  ),
  file.path(FIGURES_DIR, "exp_a_summary_tables.xlsx")
)

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat(crayon::blue$bold("\n" , strrep("=", 70), "\n"))
cat(crayon::blue$bold("EXPERIMENT A SUMMARY REPORT\n"))
cat(crayon::blue$bold(strrep("=", 70), "\n\n"))

cat(crayon::cyan("Overall Results:\n"))
cat(glue("• Total conferences analyzed: {change_counts$n_total[1]}\n"))
cat(glue("• Average reduction across all tenors: {round(mean(summary_stats$mean_reduction_pct), 1)}%\n\n"))

cat(crayon::cyan("By Tenor:\n"))
for (i in 1:nrow(summary_stats)) {
  cat(glue("• {summary_stats$tenor[i]}: {round(summary_stats$mean_reduction_pct[i], 1)}% "))
  cat(glue("(SE: {round(summary_stats$se[i], 2)}, "))
  cat(glue("{change_counts$n_with_reduction[i]}/{change_counts$n_total[i]} conferences with >{threshold_pct}% reduction)\n"))
}

cat(crayon::cyan("\nStatistical Significance:\n"))
for (i in 1:nrow(t_test_results)) {
  sig_marker <- ifelse(t_test_results$p_value[i] < 0.001, "***",
                      ifelse(t_test_results$p_value[i] < 0.01, "**",
                            ifelse(t_test_results$p_value[i] < 0.05, "*", "ns")))
  cat(glue("• {t_test_results$tenor[i]}: t = {round(t_test_results$t_stat[i], 2)}, "))
  cat(glue("p = {format.pval(t_test_results$p_value[i], digits = 3)} {sig_marker}\n"))
}

cat(crayon::blue$bold("\n", strrep("=", 70), "\n"))
cat(crayon::green$bold("\n✓ ANALYSIS COMPLETE!\n\n"))
cat(crayon::silver("Outputs saved to:\n"))
cat(glue("• Figures: {FIGURES_DIR}\n"))
cat(glue("• Tables: {file.path(FIGURES_DIR, 'exp_a_summary_tables.xlsx')}\n\n")

