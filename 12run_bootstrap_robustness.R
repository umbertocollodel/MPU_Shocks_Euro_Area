# Bootstrap and Multiple Correlation Analysis for LLM Results
# This script extends your existing analysis with robustness tests

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  readxl,
  writexl,
  RColorBrewer,
  showtext,
  boot
)

# Load your existing data (modify path as needed)
setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# Read your LLM results - update with your actual file name
name_prompt_request <- "naive_prompt"  # or "naive" or "anchor"

llm_data <- read_xlsx(paste0("../intermediate_data/aggregate_gemini_result/prompt_naive/",
                             "2.5flash_",
                             "2025-07-21", # This will look for a file with today's date
                             ".xlsx"))

# Read actual OIS data
actual_ois_df <- read_xlsx("../raw_data/ois_daily_data.xlsx", skip=1) %>%
  select(1,2,4,6) %>% 
  setNames(c("date","3M","2Y","10Y")) %>% 
  mutate(date = as.Date(date)) %>% 
  pivot_longer(`3M`:`10Y`, names_to = "tenor", values_to = "actual_rate")

# Compute LLM disagreement (standard deviation across agents)
llm_disagreement <- llm_data %>% 
  group_by(date, tenor) %>% 
  summarise(
    llm_std = sd(rate, na.rm = T),
    llm_mean = mean(rate, na.rm = T),
    n_agents = n(),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(date))

# Join with actual market data
combined_data <- llm_disagreement %>%
  inner_join(actual_ois_df, by = c("date", "tenor")) %>%
  filter(!is.na(llm_std), !is.na(actual_rate))

# Load your market volatility measure (from your existing work)
# You'll need to adapt this to your specific volatility measure
market_vol_data <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date, market_volatility = correct_post_mean)

# Final dataset for analysis
analysis_data <- combined_data %>%
  inner_join(market_vol_data, by = c("date", "tenor")) %>%
  filter(!is.na(market_volatility))

print(paste("Analysis dataset has", nrow(analysis_data), "observations"))
print(paste("Unique dates:", length(unique(analysis_data$date))))

set.seed(1346)  # For reproducibility

# ============================================================================
# BOOTSTRAP CONFIDENCE INTERVALS
# ============================================================================

bootstrap_correlation <- function(data, indices, x_var, y_var, method = "spearman") {
  # Function for bootstrap resampling
  sample_data <- data[indices, ]
  
  if (method == "spearman") {
    return(cor(sample_data[[x_var]], sample_data[[y_var]], 
               method = "spearman", use = "complete.obs"))
  } else if (method == "pearson") {
    return(cor(sample_data[[x_var]], sample_data[[y_var]], 
               method = "pearson", use = "complete.obs"))
  } else if (method == "kendall") {
    return(cor(sample_data[[x_var]], sample_data[[y_var]], 
               method = "kendall", use = "complete.obs"))
  }
}

# Function to compute bootstrap CI for each tenor
compute_bootstrap_ci <- function(data, tenor_name, n_bootstrap = 1000) {
  tenor_data <- data %>% filter(tenor == tenor_name)
  
  if (nrow(tenor_data) < 10) {
    return(list(
      tenor = tenor_name,
      n_obs = nrow(tenor_data),
      correlation = NA,
      ci_lower = NA,
      ci_upper = NA,
      p_value = NA
    ))
  }
  
  # Original correlation
  original_corr <- cor(tenor_data$llm_std, tenor_data$market_volatility, 
                      method = "spearman", use = "complete.obs")
  
  # Bootstrap
  boot_result <- boot(
    data = tenor_data,
    statistic = bootstrap_correlation,
    R = n_bootstrap,
    x_var = "llm_std",
    y_var = "market_volatility",
    method = "spearman"
  )
  
  # Confidence interval
  ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
  
  # Approximate p-value (proportion of bootstrap samples with correlation <= 0)
  p_value <- mean(boot_result$t <= 0)
  
  return(list(
    tenor = tenor_name,
    n_obs = nrow(tenor_data),
    correlation = original_corr,
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5],
    p_value = p_value,
    boot_std = sd(boot_result$t)
  ))
}

# Compute bootstrap CIs for all tenors
tenors <- c("3M", "2Y", "10Y")
bootstrap_results <- map(tenors, ~compute_bootstrap_ci(analysis_data, .x))
names(bootstrap_results) <- tenors

# Create results table
bootstrap_table <- map_dfr(bootstrap_results, ~data.frame(.x))

print("BOOTSTRAP CONFIDENCE INTERVALS")
print("==============================")
print(bootstrap_table)

# ============================================================================
# MULTIPLE CORRELATION MEASURES
# ============================================================================

compute_multiple_correlations <- function(data, tenor_name) {
  tenor_data <- data %>% filter(tenor == tenor_name)
  
  if (nrow(tenor_data) < 10) {
    return(data.frame(
      tenor = tenor_name,
      spearman = NA,
      pearson = NA,
      kendall = NA,
      n_obs = nrow(tenor_data)
    ))
  }
  
  spearman_corr <- cor(tenor_data$llm_std, tenor_data$market_volatility, 
                      method = "spearman", use = "complete.obs")
  pearson_corr <- cor(tenor_data$llm_std, tenor_data$market_volatility, 
                     method = "pearson", use = "complete.obs")
  kendall_corr <- cor(tenor_data$llm_std, tenor_data$market_volatility, 
                     method = "kendall", use = "complete.obs")
  
  return(data.frame(
    tenor = tenor_name,
    spearman = spearman_corr,
    pearson = pearson_corr,
    kendall = kendall_corr,
    n_obs = nrow(tenor_data)
  ))
}

# Compute multiple correlations
correlation_comparison <- map_dfr(tenors, ~compute_multiple_correlations(analysis_data, .x))

print("MULTIPLE CORRELATION MEASURES")
print("=============================")
print(correlation_comparison)

# Test correlation stability
correlation_stability <- correlation_comparison %>%
  select(spearman, pearson, kendall) %>%
  summarise(
    mean_correlation = rowMeans(., na.rm = TRUE),
    std_correlation = apply(., 1, sd, na.rm = TRUE),
    range_correlation = apply(., 1, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  ) %>%
  bind_cols(tenor = correlation_comparison$tenor, .)

print("CORRELATION STABILITY")
print("====================")
print(correlation_stability)

# ============================================================================
# VISUALIZATION
# ============================================================================

# Bootstrap CI plot
bootstrap_plot_data <- bootstrap_table %>%
  filter(!is.na(correlation)) %>%
  mutate(
    significant = ci_lower > 0,
    tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))
  )

p1 <- ggplot(bootstrap_plot_data, aes(x = tenor, y = correlation, color = significant)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "blue"),
                     labels = c("Not Significant", "Significant")) +
  labs(
    title = "Bootstrap Confidence Intervals",
    subtitle = "95% Confidence Intervals for Spearman Correlations",
    x = "Tenor",
    y = "Correlation",
    color = "Statistical Significance"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Multiple correlations plot
correlation_long <- correlation_comparison %>%
  pivot_longer(c(spearman, pearson, kendall), 
               names_to = "method", values_to = "correlation") %>%
  filter(!is.na(correlation)) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

p2 <- ggplot(correlation_long, aes(x = tenor, y = correlation, fill = method)) +
  geom_col(position = "dodge", alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Multiple Correlation Measures",
    subtitle = "Robustness Across Different Correlation Methods",
    x = "Tenor",
    y = "Correlation",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plots
ggsave("../output/figures/bootstrap_confidence_intervals.png", 
       plot = p1, dpi = 320, width = 10, height = 6, bg = "white")
ggsave("../output/figures/multiple_correlations.png", 
       plot = p2, dpi = 320, width = 10, height = 6, bg = "white")

# ============================================================================
# SUMMARY RESULTS
# ============================================================================

# Create summary table
summary_results <- bootstrap_table %>%
  left_join(correlation_comparison, by = "tenor") %>%
  left_join(correlation_stability, by = "tenor") %>%
  select(
    tenor, n_obs, 
    correlation, ci_lower, ci_upper, p_value,
    spearman, pearson, kendall,
    std_correlation
  )

print("COMPLETE ROBUSTNESS SUMMARY")
print("===========================")
print(summary_results)

# Export results
write_xlsx(
  list(
    "bootstrap_results" = bootstrap_table,
    "correlation_comparison" = correlation_comparison,
    "correlation_stability" = correlation_stability,
    "summary" = summary_results
  ),
  paste0("../output/robustness_analysis_", name_prompt_request, "_", Sys.Date(), ".xlsx")
)

# ============================================================================
# MULTIPLE CORRELATION MEASURES
# ============================================================================

compute_multiple_correlations <- function(data, tenor_name) {
  tenor_data <- data %>% filter(tenor == tenor_name)
  
  if (nrow(tenor_data) < 10) {
    return(data.frame(
      tenor = tenor_name,
      spearman = NA,
      pearson = NA,
      kendall = NA,
      n_obs = nrow(tenor_data)
    ))
  }
  
  spearman_corr <- cor(tenor_data$llm_std, tenor_data$market_volatility, 
                      method = "spearman", use = "complete.obs")
  pearson_corr <- cor(tenor_data$llm_std, tenor_data$market_volatility, 
                     method = "pearson", use = "complete.obs")
  kendall_corr <- cor(tenor_data$llm_std, tenor_data$market_volatility, 
                     method = "kendall", use = "complete.obs")
  
  return(data.frame(
    tenor = tenor_name,
    spearman = spearman_corr,
    pearson = pearson_corr,
    kendall = kendall_corr,
    n_obs = nrow(tenor_data)
  ))
}

# Compute multiple correlations
correlation_comparison <- map_dfr(tenors, ~compute_multiple_correlations(analysis_data, .x))

print("MULTIPLE CORRELATION MEASURES")
print("=============================")
print(correlation_comparison)

# Test correlation stability
correlation_stability <- correlation_comparison %>%
  select(spearman, pearson, kendall) %>%
  summarise(
    mean_correlation = rowMeans(., na.rm = TRUE),
    std_correlation = apply(., 1, sd, na.rm = TRUE),
    range_correlation = apply(., 1, function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  ) %>%
  bind_cols(tenor = correlation_comparison$tenor, .)

print("CORRELATION STABILITY")
print("====================")
print(correlation_stability)

# ============================================================================
# VISUALIZATION
# ============================================================================

# Bootstrap CI plot
bootstrap_plot_data <- bootstrap_table %>%
  filter(!is.na(correlation)) %>%
  mutate(
    significant = ci_lower > 0,
    tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))
  )

p1 <- ggplot(bootstrap_plot_data, aes(x = tenor, y = correlation, color = significant)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "blue"),
                     labels = c("Not Significant", "Significant")) +
  labs(
    title = "Bootstrap Confidence Intervals",
    subtitle = "95% Confidence Intervals for Spearman Correlations",
    x = "Tenor",
    y = "Correlation",
    color = "Statistical Significance"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Multiple correlations plot
correlation_long <- correlation_comparison %>%
  pivot_longer(c(spearman, pearson, kendall), 
               names_to = "method", values_to = "correlation") %>%
  filter(!is.na(correlation)) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

p2 <- ggplot(correlation_long, aes(x = tenor, y = correlation, fill = method)) +
  geom_col(position = "dodge", alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Multiple Correlation Measures",
    subtitle = "Robustness Across Different Correlation Methods",
    x = "Tenor",
    y = "Correlation",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plots
ggsave("../output/figures/bootstrap_confidence_intervals.png", 
       plot = p1, dpi = 320, width = 10, height = 6, bg = "white")
ggsave("../output/figures/multiple_correlations.png", 
       plot = p2, dpi = 320, width = 10, height = 6, bg = "white")

# ============================================================================
# SUMMARY RESULTS
# ============================================================================

# Create summary table
summary_results <- bootstrap_table %>%
  left_join(correlation_comparison, by = "tenor") %>%
  left_join(correlation_stability, by = "tenor") %>%
  select(
    tenor, n_obs, 
    correlation, ci_lower, ci_upper, p_value,
    spearman, pearson, kendall,
    std_correlation
  )

print("COMPLETE ROBUSTNESS SUMMARY")
print("===========================")
print(summary_results)

# Export results
write_xlsx(
  list(
    "bootstrap_results" = bootstrap_table,
    "correlation_comparison" = correlation_comparison,
    "correlation_stability" = correlation_stability,
    "summary" = summary_results
  ),
  paste0("../output/robustness_analysis_", name_prompt_request, "_", Sys.Date(), ".xlsx")
)

# ============================================================================
# ROBUSTNESS ASSESSMENT
# ============================================================================

# Assess robustness criteria
robustness_check <- summary_results %>%
  mutate(
    stat_significant = ci_lower > 0,
    correlation_robust = std_correlation < 0.15,
    economically_meaningful = correlation > 0.3
  )

print("ROBUSTNESS ASSESSMENT")
print("=====================")
print(robustness_check %>% 
      select(tenor, stat_significant, correlation_robust, economically_meaningful))

# Overall assessment
overall_assessment <- robustness_check %>%
  summarise(
    tenors_tested = n(),
    statistically_significant = sum(stat_significant, na.rm = TRUE),
    correlation_robust = sum(correlation_robust, na.rm = TRUE),
    economically_meaningful = sum(economically_meaningful, na.rm = TRUE)
  )

print("OVERALL ROBUSTNESS")
print("==================")
print(overall_assessment)

cat("\n=== ROBUSTNESS VERDICT ===\n")
if (overall_assessment$statistically_significant >= 2) {
  cat("✅ STATISTICAL SIGNIFICANCE: PASS\n")
} else {
  cat("❌ STATISTICAL SIGNIFICANCE: FAIL\n")
}

if (overall_assessment$correlation_robust >= 2) {
  cat("✅ CORRELATION ROBUSTNESS: PASS\n")
} else {
  cat("❌ CORRELATION ROBUSTNESS: FAIL\n")
}

if (overall_assessment$economically_meaningful >= 2) {
  cat("✅ ECONOMIC SIGNIFICANCE: PASS\n")
} else {
  cat("❌ ECONOMIC SIGNIFICANCE: FAIL\n")
}

cat("\nNext steps based on results:\n")
if (overall_assessment$statistically_significant >= 2 & 
    overall_assessment$economically_meaningful >= 2) {
  cat("🚀 Results look strong! Proceed to cross-LLM validation.\n")
} else {
  cat("⚠️  Results need improvement before expensive cross-LLM testing.\n")
}