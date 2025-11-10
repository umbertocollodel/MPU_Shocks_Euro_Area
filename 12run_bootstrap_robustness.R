# ============================================================================
# BOOTSTRAP AND MULTIPLE CORRELATION ANALYSIS FOR LLM RESULTS
# ============================================================================
#
# IMPORTANT: Run this script from the 'code/' directory
# The script uses relative paths (../intermediate_data/, ../output/)
#
# ============================================================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  readxl,
  writexl,
  RColorBrewer,
  boot
)

# Verify we're in the correct directory
if (!file.exists("../raw_data") || !file.exists("../intermediate_data")) {
  stop("Please run this script from the 'code/' directory.\n",
       "Current directory: ", getwd(), "\n",
       "Expected to find ../raw_data/ and ../intermediate_data/ directories.")
}

# --------------------------------------------------------------------------
# 1) LOAD LLM DATA
# --------------------------------------------------------------------------
name_prompt_request <- "naive_prompt"  # Update if needed

llm_data <- read_xlsx(paste0("../intermediate_data/aggregate_gemini_result/prompt_naive/",
                             "2.5flash_2025-07-21.xlsx"))

# Compute LLM disagreement (SD across agents)
llm_disagreement <- llm_data %>% 
  group_by(date, tenor) %>% 
  summarise(
    llm_std = sd(rate, na.rm = TRUE),
    llm_mean = mean(rate, na.rm = TRUE),
    n_agents = n(),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(date))

# --------------------------------------------------------------------------
# 2) LOAD POST-ANNOUNCEMENT VOLATILITY DATA (1-,2-,3-day windows)
# --------------------------------------------------------------------------
market_vol_data <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date, 
         vol_3d = correct_post_mean, 
         vol_2d = correct_post_mean_2, 
         vol_1d = correct_post_mean_1)

# Merge LLM disagreement with volatility windows
analysis_data <- llm_disagreement %>%
  inner_join(market_vol_data, by = c("date", "tenor")) %>%
  filter(!is.na(llm_std))

print(paste("Analysis dataset has", nrow(analysis_data), "observations"))
print(paste("Unique dates:", length(unique(analysis_data$date))))

# --------------------------------------------------------------------------
# 3) BOOTSTRAP FUNCTIONS
# --------------------------------------------------------------------------
bootstrap_correlation <- function(data, indices, x_var, y_var, method) {
  sample_data <- data[indices, ]
  return(cor(sample_data[[x_var]], sample_data[[y_var]], 
             method = method, use = "complete.obs"))
}

compute_bootstrap_ci <- function(data, tenor_name, method, vol_var, n_bootstrap = 5000) {
  tenor_data <- data %>% filter(tenor == tenor_name)
  method <- as.character(method)
  
  if (nrow(tenor_data) < 10) {
    return(data.frame(
      tenor = tenor_name,
      method = method,
      vol_var = vol_var,
      n_obs = nrow(tenor_data),
      correlation = NA,
      ci_lower = NA,
      ci_upper = NA
    ))
  }
  
  original_corr <- cor(tenor_data$llm_std, tenor_data[[vol_var]], 
                       method = method, use = "complete.obs")
  
  boot_result <- boot(
    data = tenor_data,
    statistic = bootstrap_correlation,
    R = n_bootstrap,
    x_var = "llm_std",
    y_var = vol_var,
    method = method
  )
  
  ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
  
  return(data.frame(
    tenor = tenor_name,
    method = method,
    vol_var = vol_var,
    n_obs = nrow(tenor_data),
    correlation = original_corr,
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5]
  ))
}

# --------------------------------------------------------------------------
# 4) DEFINE PARAMETERS
# --------------------------------------------------------------------------
methods <- c("spearman", "pearson", "kendall")
tenors  <- c("3M", "2Y", "10Y")
vol_vars <- c("vol_3d", "vol_2d", "vol_1d")

color_palette_methods <- c(
  "spearman" = "#d73027",
  "pearson"  = "#4575b4",
  "kendall"  = "#91bfdb"
)

# ============================================================================ 
# 1) BASELINE PLOT: 3-DAY VOLATILITY, TENOR ORDERED
# ============================================================================

baseline_results <- expand.grid(tenor = tenors, method = methods, stringsAsFactors = FALSE) %>%
  pmap_dfr(~compute_bootstrap_ci(analysis_data, ..1, ..2, vol_var = "vol_3d"))

# Reorder tenor factor
baseline_results$tenor <- factor(baseline_results$tenor, levels = c("3M", "2Y", "10Y"))

p_baseline <- ggplot(baseline_results, aes(x = tenor, y = correlation, fill = method)) +
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
    x = "Tenor",
    y = "Correlation",
    fill = "Method"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "bottom"
  )

ggsave("../output/figures/llm_3day_vol_corr_ordered.pdf",
       plot = p_baseline, dpi = 320, width = 10, height = 6, bg = "white")


# ============================================================================ 
# 2) WINDOW COMPARISON PLOT: SPEARMAN ONLY, FACET BY TENOR, WINDOWS ON X-AXIS
# ============================================================================

# Filter only Spearman correlations
window_results_spearman <- expand.grid(tenor = tenors, method = "spearman", vol_var = vol_vars, stringsAsFactors = FALSE) %>%
  pmap_dfr(~compute_bootstrap_ci(analysis_data, ..1, ..2, ..3))

# Rename windows for plotting
window_results_spearman <- window_results_spearman %>%
  mutate(window_label = case_when(
    vol_var == "vol_1d" ~ "1-Day",
    vol_var == "vol_2d" ~ "2-Day",
    vol_var == "vol_3d" ~ "3-Day"
  ))

# Reorder factor for x-axis
window_results_spearman$window_label <- factor(window_results_spearman$window_label,
                                               levels = c("1-Day","2-Day","3-Day"))

# Reorder tenor factor for faceting
window_results_spearman$tenor <- factor(window_results_spearman$tenor,
                                        levels = c("3M", "2Y", "10Y"))

# Assign a coherent color per tenor
tenor_colors <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

p_windows <- ggplot(window_results_spearman, 
                    aes(x = window_label, y = correlation, fill = tenor)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2, size = 0.4, color = "black") +
  facet_wrap(~tenor, nrow = 1) +
  scale_fill_manual(values = tenor_colors) +
  labs(
    title = "",
    x = "Volatility Window",
    y = "Spearman Correlation",
    fill = "Tenor"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),
    axis.text.x = element_text(size = 16, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    strip.text = element_text(size = 16)
  )

# Save figure
ggsave("../output/figures/llm_vol_windows_spearman_facet_ordered.pdf",
       plot = p_windows, dpi = 320, width = 12, height = 6, bg = "white")

