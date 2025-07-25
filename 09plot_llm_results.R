# Read LLM cleaned results: -----



clean_df=read_xlsx(paste0("../intermediate_data/llm_assessment_",
                                 name_prompt_request,
                                 "_",
                                 batch_size,
                                 "batch_"
                                 ,Sys.Date(),
                                 ".xlsx"))





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


ggsave(paste0("../output/figures/",
       name_prompt_request,
       "sd.png"),
       dpi = "retina",
       bg = "white")


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
ggsave(paste0(
       "../output/figures/",
       name_prompt_request,
       "tenor_spread_sd.png"), 
       dpi = 320, 
       width = 10, 
       height = 6,
       bg = "white")




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
date_breaks <- unique(direction_pct_df$date)[seq(1, length(unique(direction_pct_df$date)), by = 12)]

ggplot(direction_pct_df, aes(x = date, y = percentage, fill = direction)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ tenor, ncol = 1) +
  scale_fill_manual(values = c("Up" = "#D7263D", "Down" = "#1B9AAA", "Unchanged" = "#CCCCCC")) +
  scale_x_discrete(breaks = date_breaks) +
  labs(
    title = "",
    x = "", y = "%", fill = "Direction"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 24),
         axis.title = element_text( size = 20, face = "bold" ),
         legend.text = element_text(size=18),
         # The new stuff
         strip.text = element_text(size = 24)) +
  theme(legend.position = "bottom") +
  theme(plot.caption = element_text(hjust = 0,size=26)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) 



# Save the plot
ggsave(paste0("../output/figures/",
       name_prompt_request,
       "direction_percentage_heatmap.png"), 
       dpi = 320, 
       width = 10,
       height = 8,
       bg="white")



  

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



# Mistakes for the E(x) over time: ------

mean_llm_df <- clean_df %>% 
  group_by(date,tenor) %>% 
  mutate(date = as.Date(date))


actual_ois_df <- read_xlsx("../raw_data/ois_daily_data.xlsx",skip=1) %>%
  select(1,2,4,6) %>% 
  setNames(c("date","3M","2Y","10Y")) %>% 
  mutate(date = as.Date(date)) %>% 
  pivot_longer(`3M`:`10Y`,names_to = "tenor",values_to = "actual_rate")
  



# Assuming mean_llm_df and actual_ois_df are already loaded in your environment
# Join and compute error
joined_df <- merge(mean_llm_df, actual_ois_df,by=c("date","tenor")) %>%
  as_tibble() %>% 
  mutate(error = actual_rate - rate)

# Plot error by tenor over time
ggplot(joined_df, aes(x = date, y = error, color = id)) +
  geom_line() +
  geom_hline(yintercept = 0) + 
  facet_wrap(~ tenor, nrow=3) +
  labs(title = "Error by Tenor Over Time",
       x = "Date",
       y = "Error (Actual - LLM Mean Rate)",
       color = "Tenor") +
  theme_minimal()



ggsave(paste0("../output/figures/",
              name_prompt_request,
              "expected_value_error.png"), 
       dpi = 320, 
       width = 10,
       height = 8,
       bg="white")




# Add rolling correlation plot and best forecaster error!!





























