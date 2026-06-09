#===============================================================================
# SCRIPT: Analyze Qwen3 (OpenRouter) LLM Results
#===============================================================================
# This script processes and visualizes LLM results from the Qwen3 naive prompt
# simulation via OpenRouter API.
#
# Outputs:
#   1. Correlation with market-based OIS volatility (bootstrapped CIs)
#   2. Rolling correlation over time
#   3. Cross-model comparison: Qwen3 vs Gemini
#===============================================================================

# --- Configuration ---
qwen3_results_date <- "2025-12-12"  # Date suffix for Qwen3 results file
gemini_results_date <- "2025-07-21"  # Date suffix for Gemini results file

# Load necessary libraries
library(tidyverse)
library(readxl)
library(showtext)
library(zoo)
library(boot)

# Enable Segoe UI font
if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) {
    font_add("Segoe UI", regular = font_path)
  } else {
    warning("Segoe UI font file not found. Using default font.")
  }
}
showtext_auto()

# Create output directory
output_dir <- "../output/figures/qwen3_naive"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#------------------------------------------------------------------------------
## 1. LOAD DATA
#------------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("LOADING DATA\n")
cat(strrep("=", 60), "\n\n")

# Load Qwen3 cleaned results
qwen3_path <- paste0("../intermediate_data/aggregate_openrouter_result/qwen3_naive/qwen3_",
                     qwen3_results_date, ".xlsx")
cat("Loading Qwen3 results from:", qwen3_path, "\n")
qwen3_df <- read_xlsx(qwen3_path)

# Load Gemini cleaned results for comparison
gemini_path <- paste0("../intermediate_data/aggregate_gemini_result/prompt_naive/2.5flash_",
                      gemini_results_date, ".xlsx")
cat("Loading Gemini results from:", gemini_path, "\n")
gemini_df <- read_xlsx(gemini_path)

# Load market-based OIS volatility data
cat("Loading market-based OIS data...\n")
range_df <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date, correct_post_mean_1) %>%
  rename(correct_post_mean = correct_post_mean_1)

cat("\nData loaded successfully!\n")
cat("  Qwen3 rows:", nrow(qwen3_df), "\n")
cat("  Gemini rows:", nrow(gemini_df), "\n")
cat("  OIS dates:", n_distinct(range_df$date), "\n\n")

#------------------------------------------------------------------------------
## 2. CALCULATE STANDARD DEVIATION BY DATE/TENOR
#------------------------------------------------------------------------------

# Qwen3 standard deviation
std_qwen3_df <- qwen3_df %>%
  group_by(date, tenor) %>%
  summarise(
    std_rate = sd(rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(date))

# Gemini standard deviation
std_gemini_df <- gemini_df %>%
  group_by(date, tenor) %>%
  summarise(
    std_rate = sd(rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(date))

cat("Standard deviation calculated:\n")
cat("  Qwen3 date-tenor combinations:", nrow(std_qwen3_df), "\n")
cat("  Gemini date-tenor combinations:", nrow(std_gemini_df), "\n\n")

#------------------------------------------------------------------------------
## 3. JOIN WITH MARKET-BASED MEASURE
#------------------------------------------------------------------------------

# Qwen3 + OIS
combined_qwen3_df <- range_df %>%
  inner_join(std_qwen3_df, by = c("date", "tenor"))

# Gemini + OIS
combined_gemini_df <- range_df %>%
  inner_join(std_gemini_df, by = c("date", "tenor"))

cat("Joined with OIS data:\n")
cat("  Qwen3 matched rows:", nrow(combined_qwen3_df), "\n")
cat("  Gemini matched rows:", nrow(combined_gemini_df), "\n\n")

#------------------------------------------------------------------------------
## 4. BOOTSTRAP CORRELATION FUNCTIONS
#------------------------------------------------------------------------------

# Define color palette
color_palette <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

# Bootstrap helper function for Spearman correlation
bootstrap_spearman <- function(data, indices) {
  sample_data <- data[indices, ]
  return(cor(sample_data[["std_rate"]], sample_data[["correct_post_mean"]],
             method = "spearman", use = "complete.obs"))
}

# Function to compute bootstrap CI for a single tenor
compute_bootstrap_ci_tenor <- function(tenor_data, n_bootstrap = 5000) {
  if (nrow(tenor_data) < 10) {
    return(data.frame(
      correlation = NA,
      ci_lower = NA,
      ci_upper = NA,
      n_obs = nrow(tenor_data)
    ))
  }

  # Calculate point estimate
  original_corr <- cor(tenor_data[["std_rate"]], tenor_data[["correct_post_mean"]],
                       method = "spearman", use = "complete.obs")

  # Bootstrap
  boot_result <- boot(
    data = tenor_data,
    statistic = bootstrap_spearman,
    R = n_bootstrap,
    sim = "ordinary",
    stype = "i"
  )

  # Extract 88% CI using percentile method
  ci <- boot.ci(boot_result, type = "perc", conf = 0.88)

  return(data.frame(
    correlation = original_corr,
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5],
    n_obs = nrow(tenor_data)
  ))
}

#------------------------------------------------------------------------------
## 5. QWEN3 vs MARKET CORRELATION (BOOTSTRAPPED)
#------------------------------------------------------------------------------

cat("Computing Qwen3 vs Market correlation with bootstrap CIs...\n")
set.seed(42)

# Calculate correlation by tenor
cor_qwen3_by_tenor <- combined_qwen3_df %>%
  group_by(tenor) %>%
  summarise(
    mean_corr = cor(std_rate, correct_post_mean, method = "spearman", use = "pairwise.complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

# Calculate bootstrap CIs
ci_qwen3_by_tenor <- combined_qwen3_df %>%
  group_by(tenor) %>%
  nest() %>%
  mutate(ci_results = map(data, ~compute_bootstrap_ci_tenor(.x, n_bootstrap = 1500))) %>%
  unnest(ci_results) %>%
  select(tenor, ci_lower, ci_upper, n_obs) %>%
  ungroup()

# Merge
cor_qwen3_by_tenor <- cor_qwen3_by_tenor %>%
  left_join(ci_qwen3_by_tenor, by = "tenor") %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

cat("Qwen3 vs Market Correlation:\n")
print(cor_qwen3_by_tenor)
cat("\n")

# Plot: Bar plot of mean correlation by tenor
ggplot(cor_qwen3_by_tenor, aes(x = tenor, y = mean_corr, fill = tenor)) +
  geom_col(width = 0.4, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.15,
    color = "grey10",
    linewidth = 1,
    show.legend = FALSE
  ) +
  geom_text(aes(label = sprintf("%.2f", mean_corr)), vjust = -6, size = 5) +
  scale_fill_manual(values = color_palette) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "OIS Tenor",
    y = "Spearman Correlation (Qwen3 vs Market)",
    caption = ""
  ) +
  expand_limits(y = c(-0.05, max(cor_qwen3_by_tenor$ci_upper, na.rm = TRUE) + 0.15)) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10), size = 16),
    axis.title.y = element_text(margin = margin(r = 10), size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(file.path(output_dir, "mean_spearman_correlation_bar.pdf"),
       dpi = 320, width = 8, height = 6, bg = "white")


#------------------------------------------------------------------------------
## 7. CROSS-MODEL COMPARISON: QWEN3 vs GEMINI
#------------------------------------------------------------------------------

cat("\nComputing Qwen3 vs Gemini correlation...\n")

# Join Qwen3 and Gemini std_rate by date and tenor
cross_model_df <- std_qwen3_df %>%
  inner_join(std_gemini_df, by = c("date", "tenor"), suffix = c("_qwen3", "_gemini"))

cat("Cross-model matched rows:", nrow(cross_model_df), "\n")

# Bootstrap function for cross-model correlation
bootstrap_cross_model <- function(data, indices) {
  sample_data <- data[indices, ]
  return(cor(sample_data[["std_rate_qwen3"]], sample_data[["std_rate_gemini"]],
             method = "spearman", use = "complete.obs"))
}

# Compute cross-model CI for a single tenor
compute_cross_model_ci <- function(tenor_data, n_bootstrap = 1500) {
  if (nrow(tenor_data) < 10) {
    return(data.frame(
      correlation = NA,
      ci_lower = NA,
      ci_upper = NA,
      n_obs = nrow(tenor_data)
    ))
  }

  original_corr <- cor(tenor_data[["std_rate_qwen3"]], tenor_data[["std_rate_gemini"]],
                       method = "spearman", use = "complete.obs")

  boot_result <- boot(
    data = tenor_data,
    statistic = bootstrap_cross_model,
    R = n_bootstrap,
    sim = "ordinary",
    stype = "i"
  )

  ci <- boot.ci(boot_result, type = "perc", conf = 0.88)

  return(data.frame(
    correlation = original_corr,
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5],
    n_obs = nrow(tenor_data)
  ))
}

set.seed(42)

# Calculate cross-model correlation by tenor
cor_cross_model <- cross_model_df %>%
  group_by(tenor) %>%
  summarise(
    mean_corr = cor(std_rate_qwen3, std_rate_gemini, method = "spearman", use = "pairwise.complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

# Calculate bootstrap CIs
ci_cross_model <- cross_model_df %>%
  group_by(tenor) %>%
  nest() %>%
  mutate(ci_results = map(data, ~compute_cross_model_ci(.x, n_bootstrap = 1500))) %>%
  unnest(ci_results) %>%
  select(tenor, ci_lower, ci_upper, n_obs) %>%
  ungroup()

# Merge
cor_cross_model <- cor_cross_model %>%
  left_join(ci_cross_model, by = "tenor") %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

cat("\nQwen3 vs Gemini Correlation:\n")
print(cor_cross_model)

# Plot: Cross-model correlation bar chart
ggplot(cor_cross_model, aes(x = tenor, y = mean_corr, fill = tenor)) +
  geom_col(width = 0.4, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.15,
    color = "grey10",
    linewidth = 1,
    show.legend = FALSE
  ) +
  geom_text(aes(label = sprintf("%.2f", mean_corr)), vjust = -6, size = 5) +
  scale_fill_manual(values = color_palette) +
  labs(
    title = NULL,
    x = "OIS Tenor",
    y = "Spearman Correlation (Qwen3 vs Gemini)",
    caption = ""
  ) +
  expand_limits(y = c(-0.05, max(cor_cross_model$ci_upper, na.rm = TRUE) + 0.15)) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10), size = 16),
    axis.title.y = element_text(margin = margin(r = 10), size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave(file.path(output_dir, "qwen3_vs_gemini_correlation.pdf"),
       dpi = 320, width = 8, height = 6, bg = "white")

#------------------------------------------------------------------------------
## 8. SUMMARY TABLE
#------------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("CORRELATION SUMMARY\n")
cat(strrep("=", 60), "\n\n")

# Also compute Gemini vs Market for comparison
cor_gemini_by_tenor <- combined_gemini_df %>%
  group_by(tenor) %>%
  summarise(
    mean_corr = cor(std_rate, correct_post_mean, method = "spearman", use = "pairwise.complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

# Create summary table
summary_table <- tibble(
  Tenor = c("3M", "2Y", "10Y"),
  `Qwen3 vs Market` = cor_qwen3_by_tenor$mean_corr,
  `Gemini vs Market` = cor_gemini_by_tenor$mean_corr,
  `Qwen3 vs Gemini` = cor_cross_model$mean_corr
)

print(summary_table)

cat("\n", strrep("=", 60), "\n")
cat("Plots saved to:", output_dir, "\n")
cat(strrep("=", 60), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
