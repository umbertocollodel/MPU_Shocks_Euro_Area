#==============================================================================
# SCRIPT: Analyze 'prompt_anchor_values' LLM Results
#==============================================================================
# This script processes LLM results from the 'prompt_anchor_values' analysis

# --- Define Parameters ---
# Set the name for this analysis run for consistent file naming
name_prompt_request <- "prompt_history_surprises"
# Set the batch size if it's a variable you use
# batch_size <- "your_batch_size"

# Enable Segoe UI font (ensure it's installed on your system)
font_add("Segoe UI", regular = "C:/Windows/Fonts/segoeui.ttf")
showtext_auto()


#------------------------------------------------------------------------------
## 2. LOAD AND PROCESS LLM ASSESSMENT DATA
#------------------------------------------------------------------------------

# --- Read LLM cleaned results ---
# Note: Using a fixed name for now. Update the paste0 call if batch_size and Sys.Date() are needed.
clean_df <- read_xlsx(paste0("../intermediate_data/llm_assessment_prompt_history_surprises2025-07-18.xlsx"))


#------------------------------------------------------------------------------
## 3. PLOT 1: LLM RESPONSE UNCERTAINTY (STANDARD DEVIATION)
#------------------------------------------------------------------------------

# --- Calculate standard deviation and highlight extremes ---
std_df <- clean_df %>%
  group_by(date, tenor) %>%
  summarise(std_rate = sd(rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(date))

# Calculate 95th percentile threshold to highlight high uncertainty
thresholds <- std_df %>%
  group_by(tenor) %>%
  summarise(p95 = quantile(std_rate, 0.95, na.rm = TRUE), .groups = "drop")

# Join thresholds and create highlight flag
std_df <- std_df %>%
  left_join(thresholds, by = "tenor") %>%
  mutate(highlight = std_rate > p95)

# --- Create the plot ---
plot_std_dev <- ggplot(std_df, aes(x = date, y = std_rate)) +
  geom_line(aes(color = tenor), linewidth = 0.8) +
  geom_point(data = . %>% filter(highlight), aes(fill = tenor), shape = 21, size = 3, stroke = 1) +
  facet_wrap(~ tenor, nrow = 3, scales = "free_y") +
  scale_color_manual(values = c("10Y" = "#d73027", "2Y" = "#4575b4", "3M" = "#91bfdb"), guide = "none") +
  scale_fill_manual(values = c("10Y" = "#d73027", "2Y" = "#4575b4", "3M" = "#91bfdb"), guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "LLM Response Uncertainty Over Time",
    subtitle = "Standard deviation of LLM rate predictions. Points highlight values above the 95th percentile.",
    x = NULL,
    y = "Standard Deviation of Predicted Rate"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.text = element_text(face = "bold", size = 12)
  )

print(plot_std_dev)

# --- Save the plot ---
ggsave(
  filename = paste0("../output/figures/", name_prompt_request, "/sd.png"),
  plot = plot_std_dev,
  dpi = 320, width = 10, height = 8, bg = "white"
)


#------------------------------------------------------------------------------
## 4. PLOT 2: SPREAD IN LLM UNCERTAINTY (10Y vs 3M)
#------------------------------------------------------------------------------

# --- Compute spread ---
spread_df <- std_df %>%
  filter(tenor %in% c("10Y", "3M")) %>%
  select(date, tenor, std_rate) %>%
  pivot_wider(names_from = tenor, values_from = std_rate) %>%
  mutate(diff_10y_3m = `10Y` - `3M`) %>%
  drop_na(diff_10y_3m)

# --- Compute quantiles for highlighting ---
quantiles <- quantile(spread_df$diff_10y_3m, probs = c(0.05, 0.95), na.rm = TRUE)

# --- Add highlight category ---
spread_df <- spread_df %>%
  mutate(
    highlight = case_when(
      diff_10y_3m > quantiles[2] ~ "High Spread",
      diff_10y_3m < quantiles[1] ~ "Low Spread",
      TRUE ~ "Normal"
    )
  )

# --- Define custom colors ---
highlight_colors <- c("High Spread" = "#D7263D", "Low Spread" = "#1B9AAA", "Normal" = "grey80")

# --- Create the plot ---
plot_spread <- ggplot(spread_df, aes(x = date, y = diff_10y_3m)) +
  geom_area(fill = "grey95", alpha = 0.8) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_hline(yintercept = quantiles, linetype = "dotted", color = "grey50") +
  geom_line(color = "grey30", linewidth = 0.8) +
  geom_point(aes(color = highlight), size = 3) +
  scale_color_manual(values = highlight_colors) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Spread in LLM Uncertainty Between 10Y and 3M Tenors",
    subtitle = "Difference in standard deviation (10Y - 3M). Points highlight values outside the 5th-95th percentile range.",
    x = NULL, y = "Uncertainty Spread (10Y - 3M)", color = ""
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank()
  )

print(plot_spread)

# --- Save the plot ---
ggsave(
  filename = paste0("../output/figures/", name_prompt_request, "_tenor_spread_sd.png"),
  plot = plot_spread,
  dpi = 320, width = 12, height = 7, bg = "white"
)


#------------------------------------------------------------------------------
## 5. PLOT 3: DIRECTIONAL AGREEMENT OF LLM RESPONSES
#------------------------------------------------------------------------------

# --- Calculate percentage of each direction ---
direction_pct_df <- clean_df %>%
  filter(direction %in% c("Up", "Down", "Unchanged")) %>%
  count(date, tenor, direction) %>%
  group_by(date, tenor) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

# --- Create the plot ---
plot_direction <- ggplot(direction_pct_df, aes(x = as.factor(date), y = percentage, fill = direction)) +
  geom_col(position = "stack", width = 1) +
  facet_wrap(~ tenor, ncol = 1) +
  scale_fill_manual(values = c("Up" = "#d73027", "Down" = "#4575b4", "Unchanged" = "grey80")) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 12)]) + # Show break every 12 meetings
  labs(
    title = "LLM Directional Agreement Over Time",
    subtitle = "Percentage of LLM responses predicting rates to go Up, Down, or stay Unchanged",
    x = NULL, y = "Percentage (%)", fill = "Predicted Direction"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )

print(plot_direction)

# --- Save the plot ---
ggsave(
  filename = paste0("../output/figures/", name_prompt_request, "/direction_percentage.png"),
  plot = plot_direction,
  dpi = 320, width = 12, height = 9, bg = "white"
)


#------------------------------------------------------------------------------
## 6. PLOT 4: CORRELATION (LLM UNCERTAINTY vs MARKET SURPRISE)
#------------------------------------------------------------------------------

# --- Load market-based surprise data ---
range_df <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = if_else(tenor == "3mnt", "3M", tenor)) %>%
  select(tenor, date, correct_post_mean)

# --- Combine with LLM uncertainty data ---
combined_df <- range_df %>%
  inner_join(std_df %>% select(date, tenor, std_rate), by = c("date", "tenor"))

# --- Compute rolling Spearman correlation ---

rolling_corr_df <- combined_df %>%
  arrange(tenor, date) %>%
  group_by(tenor) %>%
  mutate(
    rolling_corr = rollapply(
      data = cbind(std_rate, correct_post_mean),
      width = 12,
      FUN = function(w) cor(w[, 1], w[, 2], method = "spearman", use = "pairwise.complete.obs"),
      by.column = FALSE,
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup()


# --- Create the plot ---
color_palette_corr <- c("10Y" = "#d73027", "2Y" = "#4575b4", "3M" = "#91bfdb")

plot_rolling_corr <- ggplot(rolling_corr_df, aes(x = date, y = rolling_corr +0.1, color = tenor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_line(linewidth = 1, alpha = 0.9) +
  scale_color_manual(values = color_palette_corr) +
  scale_y_continuous(breaks = seq(-1.0, 1.0, 0.25), limits = c(-1.0, 1.0)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Correlation Between LLM Uncertainty and Market Surprise",
    subtitle = "12-month rolling Spearman correlation between LLM StDev and market-based surprise (change in mean rate)",
    x = NULL, y = "Rolling Spearman Correlation", color = "OIS Tenor"
  ) +
  facet_wrap(~ tenor, ncol = 1) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank()
  )

print(plot_rolling_corr)

# --- Save the plot ---
ggsave(
  filename = paste0("../output/figures/", name_prompt_request, "/rolling_correlation.png"),
  plot = plot_rolling_corr,
  dpi = 320, width = 12, height = 7, bg = "white"
)



# Calculate Spearman correlation by tenor
cor_by_tenor <- combined_df %>%
  group_by(tenor) %>%
  summarise(
    spearman_corr = cor(std_rate, correct_post_mean, method = "spearman", use = "pairwise.complete.obs"),
    .groups = "drop"
  )

print(cor_by_tenor)


#------------------------------------------------------------------------------
## 7. PLOT 5: LLM FORECAST ACCURACY
#------------------------------------------------------------------------------

# --- Prepare data for plotting accuracy ---
# Get LLM mean prediction
mean_llm_df <- clean_df %>%
  group_by(date, tenor) %>%
  summarise(llm_mean_rate = mean(rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(date))

# Load actual OIS data
actual_ois_df <- read_xlsx("../raw_data/ois_daily_data.xlsx", skip = 1) %>%
  select(1, 2, 4, 6) %>%
  setNames(c("date", "3M", "2Y", "10Y")) %>%
  mutate(date = as.Date(date)) %>%
  pivot_longer(`3M`:`10Y`, names_to = "tenor", values_to = "actual_rate")

# --- Join LLM predictions with actuals and compute error ---
joined_df <- mean_llm_df %>%
  inner_join(actual_ois_df, by = c("date", "tenor")) %>%
  mutate(error = actual_rate - llm_mean_rate)

# --- PLOT 5a: ACTUAL vs. PREDICTED ---
plot_df_compare <- joined_df %>%
  pivot_longer(
    cols = c(actual_rate, llm_mean_rate),
    names_to = "rate_type",
    values_to = "rate_value"
  ) %>%
  mutate(rate_type = recode(rate_type,
                           "actual_rate" = "Actual Market Rate",
                           "llm_mean_rate" = "LLM Predicted Rate"))

color_palette_compare <- c("Actual Market Rate" = "black", "LLM Predicted Rate" = "#377EB8")

plot_actual_vs_pred <- ggplot(plot_df_compare, aes(x = date, y = rate_value, color = rate_type)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ tenor, nrow = 3, scales = "free_y") +
  scale_color_manual(values = color_palette_compare) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title = "LLM-Predicted Rates vs. Actual Market Outcomes",
    subtitle = "Comparison of mean predicted OIS rate against actual rate post-ECB press conference",
    x = NULL, y = "OIS Rate (%)", color = NULL
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.text = element_text(face = "bold", size = 12)
  )

print(plot_actual_vs_pred)

ggsave(
  filename = paste0("../output/figures/", name_prompt_request, "/actual_vs_predicted.png"),
  plot = plot_actual_vs_pred,
  dpi = 320, width = 10, height = 8, bg = "white"
)


# --- PLOT 5b: FORECAST ERROR OVER TIME ---
plot_error <- ggplot(joined_df, aes(x = date, y = error)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_line(aes(color = tenor), linewidth = 0.9) +
  facet_wrap(~ tenor, nrow = 3, scales = "free_y") +
  scale_color_manual(values = c("10Y" = "#d73027", "2Y" = "#4575b4", "3M" = "#91bfdb"), guide = "none") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title = "LLM Forecast Error Over Time",
    subtitle = "Error calculated as: Actual Rate - LLM Mean Predicted Rate",
    x = NULL, y = "Forecast Error (bps)"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.text = element_text(face = "bold", size = 12)
  )

print(plot_error)

ggsave(
  filename = paste0("../output/figures/", name_prompt_request, "/expected_value_error.png"),
  plot = plot_error,
  dpi = 320, width = 10, height = 8, bg = "white"
)






















