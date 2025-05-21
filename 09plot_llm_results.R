# Add Segoe UI font ----

# Automatically use showtext for new plots
showtext_auto()

# Add Segoe UI (you must know the path or have it installed)
font_add("Segoe UI", regular = "C:/Windows/Fonts/segoeui.ttf")


#Figure: time series confusion scores for long and short run -----

plot_and_save <- function(horizon_type, filename) {
  
  df_filtered <- diff_df %>%
    filter(horizon == horizon_type) %>%
    arrange(date) %>%
    mutate(moving_avg = zoo::rollmean(conf_score, k = 12, fill = NA, align = "right"))
  
  high_threshold <- quantile(df_filtered$conf_score, 0.98, na.rm = TRUE)
  
  highlight_high <- df_filtered %>%
    filter(conf_score >= high_threshold)
  
  p <- ggplot(df_filtered, aes(x = date, y = conf_score)) +
    geom_line(color = "#2C3E50", size = 1) +
    geom_line(aes(y = moving_avg), color = "#2980B9", size = 1.2, linetype = "dashed") +
    geom_point(data = highlight_high, aes(x = date, y = conf_score), color = "#E74C3C", size = 3, alpha = 0.8) +
    geom_vline(data = highlight_high, aes(xintercept = date), color = "#E74C3C", alpha = 0.5, size = 2) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    scale_x_date(
      date_labels = "%b '%y",
      date_breaks = "9 months",
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      axis.text.x = element_text(angle = 270, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      plot.caption = element_text(size = 10, color = "gray40"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = NULL,
      y = "Confusion Score",
      title = paste("Confusion Scores for", horizon_type, "Horizon"),
      subtitle = "Dashed line shows 12-point moving average\nRed = top 2%",
      caption = "Source: Internal model outputs"
    )
  
  ggsave(filename = filename, plot = p, width = 14, height = 7, dpi = 320)
  return(p)
}



# Step 3: Generate and export both plots
plot_and_save("Short-term", "../output/figures/Short-term_conf_scores_actual.png")
plot_and_save("Long-term", "../output/figures/Long-term_conf_scores_actual.png")





# Table: highest confusion scores: ----

# Step: Extract 98th percentile occurrences for each horizon
percentile_98_tables <- clean_df %>%
  group_by(horizon) %>%
  mutate(high_threshold = quantile(conf_score, 0.98, na.rm = TRUE)) %>%
  filter(conf_score >= high_threshold) %>%
  arrange(horizon, desc(conf_score)) %>%
  split(.$horizon)

# View the table
print(percentile_98_table)

# Plot difference over time: -----


# Step 1: Prepare the data
diff_df <- clean_df %>%
  mutate(date = as.Date(date)) %>%
  select(date, horizon, conf_score) %>%
  pivot_wider(names_from = horizon, values_from = conf_score) %>%
  rename(short = `Short-term`, long = `Long-term`) %>%
  mutate(diff = short - long)

# Step 2: Identify high-difference dates (95th percentile and above)
threshold <- quantile(diff_df$diff, 0.98, na.rm = TRUE)
highlight_dates <- diff_df %>%
  filter(diff >= threshold) %>%
  pull(date)



# Step 3: Plot
ggplot(diff_df, aes(x = date, y = diff)) +
  geom_vline(data = tibble(date = highlight_dates),
             aes(xintercept = date),
             color = "grey60", alpha = 0.6, size = 1) +
  geom_point(size = 3, alpha = 0.6) +
  geom_line(aes(group = 1), size = 1) +
  geom_hline(yintercept = 0) +
  ylim(-4,4) +
  theme_minimal(base_family = "Segoe UI Light") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 16),
        legend.position = "bottom") +
  labs(x = "", y = "Confusion Score", color = "", title = "Confusion Scores with High Difference Highlights")


# Save the plot
ggsave(filename = "../output/figures/llm_confusion_score_actual_diff.png", 
       width = 12, 
       height = 6,
       dpi = "retina")