# Plot short and long term confusion with lowest/highest scores: ----


# Step 1: Prepare the data
diff_df <- clean_df %>%
  mutate(date = as.Date(date)) %>%
  select(date, horizon, conf_score)

# Step 2: Function to create and save plot
plot_and_save <- function(horizon_type, filename) {
  df_filtered <- diff_df %>%
    filter(horizon == horizon_type)
  
  high_threshold <- quantile(df_filtered$conf_score, 0.98, na.rm = TRUE)
  low_threshold <- quantile(df_filtered$conf_score, 0.02, na.rm = TRUE)
  
  highlight_high_dates <- df_filtered %>%
    filter(conf_score >= high_threshold) %>%
    pull(date)
  
  highlight_low_dates <- df_filtered %>%
    filter(conf_score <= low_threshold) %>%
    pull(date)
  
  p <- ggplot(df_filtered, aes(x = date, y = conf_score,col=horizon)) +
    geom_vline(data = tibble(date = highlight_high_dates),
               aes(xintercept = date),
               color = "grey40", alpha = 0.6, size = 1) +
    geom_vline(data = tibble(date = highlight_low_dates),
               aes(xintercept = date),
               color = "blue", alpha = 0.6, size = 1) +
    geom_point(size = 3, alpha = 0.6) +
    geom_line(size = 1) +
    theme_minimal(base_family = "Segoe UI Light") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 16),
          legend.position = "none") +
    labs(x = "", y = "Confusion Score",
         title = paste("Confusion Scores for", horizon_type, "Horizon"))
  
  # Save the plot
  ggsave(filename = filename, 
         plot = p, 
         width = 12, 
         height = 6,
         dpi = "retina")
  
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