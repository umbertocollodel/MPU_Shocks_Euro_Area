# Read LLM cleaned results: -----



clean_df=read_xlsx("../intermediate_data/llm_assessment_3batch_2025-06-17.xlsx")





# Figure: standard deviation over time for ech tenor ------ 


# Enable Segoe UI font
font_add("Segoe UI", regular = "C:/Windows/Fonts/segoeui.ttf")
showtext_auto()

# Assuming your cleaned data is in `clean_df`
# Calculate standard deviation of rate by date and tenor
std_df <- clean_df %>%
  group_by(date, tenor) %>%
  summarise(std_rate = sd(rate),
            range_rate = max(rate) - min(rate),.groups = "drop")

# Calculate 95th percentile threshold for each tenor
thresholds <- std_df %>%
  group_by(tenor) %>%
  summarise(p95 = quantile(std_rate, 0.95,na.rm=T), .groups = "drop")

# Join thresholds back to std_df and flag high values
std_df <- std_df %>%
  left_join(thresholds, by = "tenor") %>%
  mutate(highlight = std_rate > p95) %>% 
  mutate(date =as.Date(date))

std_df %>% 
  group_by(tenor) %>% 
  mutate(mean = mean(std_rate,na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = std_rate, color = tenor)) +
  geom_line() +
  geom_line(aes(y=mean),size=1.5,linetype="dashed") +
  geom_point(aes(size = highlight), shape = 21, fill = "white", stroke = 1.2) +
  facet_wrap(~ tenor, nrow = 3, scales = "free_y") +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  scale_x_date(date_breaks = "9 months", date_labels = "%b %Y") +  # Adjust as needed
  labs(
    title = "Standard Deviation of Rates by Tenor Over Time",
    x = "Date",
    y = "Standard Deviation of Rate",
    color = "Tenor"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))


ggsave("../output/figures/naive_prompt_sd.png",
       dpi = "retina")

# Table: compute correlation matrix across sd tenors ----

cor_matrix <- std_df %>%
  select(date, tenor, std_rate) %>%
  pivot_wider(names_from = tenor, values_from = std_rate) %>%
  select(-date) %>%
  cor(use = "pairwise.complete.obs")

print(cor_matrix)


# Figure: difference between 10 years and 3months standard deviation: -----

# Compute spread
spread_df <- std_df %>%
  filter(tenor %in% c("10Y", "3M")) %>%
  select(date, tenor, std_rate) %>%
  pivot_wider(names_from = tenor, values_from = std_rate) %>%
  mutate(diff_10y_3m = `10Y` - `3M`)

# Compute quantiles
quantiles <- quantile(spread_df$diff_10y_3m, probs = c(0.05, 0.95), na.rm = TRUE)

# Add highlight category
spread_df <- spread_df %>%
  mutate(
    highlight = case_when(
      diff_10y_3m > quantiles[2] ~ "Above 95th Percentile",
      diff_10y_3m < quantiles[1] ~ "Below 5th Percentile",
      TRUE ~ "Normal"
    )
  )

# Define custom colors
highlight_colors <- c(
  "Above 95th Percentile" = "#D7263D",  # bold red
  "Below 5th Percentile" = "#1B9AAA",   # deep blue
  "Normal" = "#CCCCCC"                 # soft gray
)

# Plot
ggplot(spread_df, aes(x = date, y = diff_10y_3m)) +
  geom_area(fill = "#F4F4F4", alpha = 0.5) +
  geom_line(color = "#333333", size = 0.8) +
  geom_point(aes(color = highlight), size = 4, alpha = 0.8) +
  geom_hline(yintercept = quantiles, linetype = "dotted", color = "#999999") +
  scale_color_manual(values = highlight_colors) +
  labs(
    title = "Volatility Spread Between 10Y and 3M Tenors",
    subtitle = "Standard Deviation Difference with Highlighted Extremes",
    x = "Date",
    y = "Std Dev Spread (10Y - 3M)",
    color = ""
  ) +
  theme_minimal(base_family = "Segoe UI") +
  scale_x_date(date_breaks = "2 years", date_labels = "%b %Y") +  # Adjust as needed
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 270, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

# Save
ggsave("../output/figures/tenor_spread_sd_highlighted_beautiful.png", dpi = 320, width = 10, height = 6)



# Figure: percentage of direction for each tenor and date -----

# Calculate percentage of each direction per tenor and date
direction_pct_df <- clean_df %>%
  group_by(date, tenor, direction) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(date, tenor) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup() %>% 
  filter(direction %in% c("Up","Down","Unchanged"))


# Get a subset of dates to show as breaks (e.g., every 10th unique date)
date_breaks <- unique(direction_pct_df$date)[seq(1, length(unique(direction_pct_df$date)), by = 6)]

ggplot(direction_pct_df, aes(x = date, y = percentage, fill = direction)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ tenor, ncol = 1) +
  scale_fill_manual(values = c("Up" = "#D7263D", "Down" = "#1B9AAA", "Unchanged" = "#CCCCCC")) +
  scale_x_discrete(breaks = date_breaks) +
  labs(
    title = "Distribution of Rate Change Directions Over Time by Tenor",
    x = "", y = "%", fill = "Direction"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))



# Save the plot
ggsave("../output/figures/direction_percentage_heatmap.png", dpi = 320, width = 10, height = 8)





# Figure: correlation between standard deviation and range rates


std_df %>% 
  pivot_longer(std_rate:range_rate,names_to = "type",values_to = "value") %>% 
  mutate(type = ifelse(type == "std_rate","SD","Range")) %>% 
  ggplot(aes(x = date, y = value, color = type)) +
  geom_line(size=1) +
  facet_wrap(~ tenor, nrow = 3, scales = "free_y") +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  scale_x_date(date_breaks = "9 months", date_labels = "%b %Y") +  # Adjust as needed
  labs(
    title = "Correlation between SD and Range of Rates by Tenor Over Time",
    x = "Date",
    y = "Standard Deviation of Rate",
    color = "Measure"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))


ggsave("../output/figures/naive_prompt_correlation_sd_range.png",
       dpi = "retina")
  

# Correlation between market-based measure and in-vitro llm measure: ----

# Step 1: Load and prepare the data
range_df <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date, correct_post_mean)

# Assuming std_df is already loaded and contains: date, tenor, std_rate

combined_df <- range_df %>%
  inner_join(std_df, by = c("date", "tenor"))


# Step 2: Compute rolling Spearman correlation


# Ensure the data is sorted
combined_df <- combined_df %>% arrange(date)

# Select only the two columns needed
rolling_corr_df <- combined_df %>%
  select(date, tenor, std_rate, correct_post_mean) %>%
  group_by(tenor) %>%
  group_split() %>%
  purrr::map_dfr(~ {
    df <- .x
    if (nrow(df) >= 12) {
      df$rolling_corr <- rollapply(
        data = df[, c("std_rate", "correct_post_mean")],
        width = 12,
        FUN = function(w) cor(w[, 1], w[, 2], method = "spearman", use = "complete.obs"),
        by.column = FALSE,
        align = "right",
        fill = NA
      )
    } else {
      df$rolling_corr <- NA
    }
    df
  })








































