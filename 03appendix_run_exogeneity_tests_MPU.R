# ==============================================================================
# Autoregression Analysis: Test for serial correlation in MPU surprises
# ==============================================================================
# Purpose: Check if current MPU surprises predict future ones (exogeneity test)
# If coefficients are insignificant, surprises are truly unpredictable/exogenous

# Run AR(3) regression for each tenor
regression_results <- differences_df %>% 
  split(.$tenor) %>% 
  map(~ {
    # Create lagged variables
    .x %>% 
      mutate(
        `Lag(1)` = dplyr::lag(diff_3, 1),
        `Lag(2)` = dplyr::lag(diff_3, 2),
        `Lag(3)` = dplyr::lag(diff_3, 3)
      )
  }) %>% 
  # Estimate AR(3) model: diff_3(t) = β0 + β1*diff_3(t-1) + β2*diff_3(t-2) + β3*diff_3(t-3) + ε
  map(~ lm(diff_3 ~ `Lag(1)` + `Lag(2)` + `Lag(3)`, data = .x)) %>% 
  # Extract coefficients with 95% confidence intervals
  map(~ tidy(.x, conf.int = TRUE))

# Combine results across all tenors
results_df <- bind_rows(regression_results, .id = "tenor") %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

# ==============================================================================
# Visualization: Coefficient plot with confidence intervals
# ==============================================================================

results_df %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate, color = tenor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = 0.5), 
    width = 0.3, 
    linewidth = 1
  ) +
  scale_color_brewer(palette = "Set2") +  # Same as your baseline correlation plot
  labs(x = "", y = "", color = "Tenor") +
  theme_bw() +
  theme(
    text = element_text(family = "Segoe UI Light"),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )

# ==============================================================================
# Export high-quality figure
# ==============================================================================
ggsave(
  "../output/figures/autocorrelation_surprises.pdf",
  dpi = 320,
  width = 12,
  height = 9,
  bg = "white"
)

# Optional: Print summary statistics to verify exogeneity
cat("\n=== Summary: Statistical Significance of Lags ===\n")
results_df %>%
  filter(term != "(Intercept)") %>%
  group_by(tenor) %>%
  summarise(
    any_significant = any(p.value < 0.05),
    max_coef = max(abs(estimate))
  ) %>%
  print()
cat("\nNote: If 'any_significant' is FALSE for all tenors, MPU surprises are exogenous.\n")
