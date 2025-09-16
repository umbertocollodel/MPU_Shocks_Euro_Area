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
# BOOTSTRAP CONFIDENCE INTERVALS FOR ALL CORRELATION METHODS
# ============================================================================

bootstrap_correlation <- function(data, indices, x_var, y_var, method) {
  sample_data <- data[indices, ]
  return(cor(sample_data[[x_var]], sample_data[[y_var]], 
             method = method, use = "complete.obs"))
}

compute_bootstrap_ci <- function(data, tenor_name, method, n_bootstrap = 5000) {
  tenor_data <- data %>% filter(tenor == tenor_name)
  method <- as.character(method)   # 🔑 ensure method is character
  
  if (nrow(tenor_data) < 10) {
    return(data.frame(
      tenor = tenor_name,
      method = method,
      n_obs = nrow(tenor_data),
      correlation = NA,
      ci_lower = NA,
      ci_upper = NA
    ))
  }
  
  # Original correlation
  original_corr <- cor(tenor_data$llm_std, tenor_data$market_volatility, 
                       method = method, use = "complete.obs")
  
  # Bootstrap
  boot_result <- boot(
    data = tenor_data,
    statistic = bootstrap_correlation,
    R = n_bootstrap,
    x_var = "llm_std",
    y_var = "market_volatility",
    method = method
  )
  
  # Confidence interval
  ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
  
  return(data.frame(
    tenor = tenor_name,
    method = method,
    n_obs = nrow(tenor_data),
    correlation = original_corr,
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5]
  ))
}

# Make sure method stays character in the loop
methods <- c("spearman", "pearson", "kendall")
tenors <- c("3M", "2Y", "10Y")

bootstrap_results_all <- expand.grid(tenor = tenors, method = methods, stringsAsFactors = FALSE) %>%
  pmap_dfr(~compute_bootstrap_ci(analysis_data, ..1, ..2))


print("BOOTSTRAP CONFIDENCE INTERVALS (ALL METHODS)")
print("==========================================")
print(bootstrap_results_all)

# ============================================================================
# CUSTOM COLOR PALETTE (by correlation method)
# ============================================================================
color_palette_methods <- c(
  "spearman" = "#d73027",
  "pearson"  = "#4575b4",
  "kendall"  = "#91bfdb"
)

# Plot with method colors
p_all <- ggplot(bootstrap_results_all, aes(x = tenor, y = correlation, fill = method)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, group = method),
                position = position_dodge(width = 0.7), width = 0.2, size = 0.4) +
  scale_fill_manual(
    values = color_palette_methods,
    labels = c(expression("Spearman's "~rho), 
               expression("Pearson's "~r), 
               expression("Kendall's "~tau))
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "Tenor",
    y = "Correlation",
    fill = "Method"
  ) +
  theme_minimal(base_size = 14) +  # base text size
  theme(
    axis.title = element_text(size = 18),       # axis titles
    axis.text = element_text(size = 16),        # axis tick labels
    legend.title = element_text(size = 18),     # legend title
    legend.text = element_text(size = 16),      # legend text
    legend.position = "bottom"
  )


ggsave("../output/figures/multiple_correlation_bootstrap.pdf", 
       plot = p_all, dpi = 320, width = 10, height = 6, bg = "white")

# ============================================================================
# SUMMARY TABLE FOR LATEX EXPORT
# ============================================================================
library(xtable)

summary_table <- bootstrap_results_all %>%
  mutate(
    ci = paste0("(", round(ci_lower, 2), " -- ", round(ci_upper, 2), ")"),
    value = paste0(round(correlation, 2), " ", ci)
  ) %>%
  select(tenor, method, value) %>%
  pivot_wider(names_from = method, values_from = value)

print(summary_table)

# Export LaTeX table
latex_tab <- xtable(summary_table, 
                    caption = "Robustness of Correlations: Multiple Measures and Bootstrap Confidence Intervals",
                    label = "tab:robust_corr",
                    align = c("l", "c", "c", "c", "c"))

print(latex_tab,
      include.rownames = FALSE,
      file = "../output/robustness_table.tex",
      sanitize.text.function = identity)
