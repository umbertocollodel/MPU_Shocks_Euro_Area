#==============================================================================
# SCRIPT: Analyze 'prompt_anchor_values' LLM Results
#==============================================================================
# This script processes LLM results from the 'prompt_anchor_values' analysis

# --- Define Parameters ---
# Set the name for this analysis run for consistent file naming
name_prompt_request <- "prompt_naive" # Changed to "prompt_naive" as requested
# Set the batch size if it's a variable you use
# batch_size <- "your_batch_size"

# Load necessary libraries
library(tidyverse) # For data manipulation and plotting
library(readxl)    # For reading Excel files
library(showtext)  # For custom fonts
library(zoo)       # For rolling functions
# library(Hmisc)     # For wtd.var (weighted variance) - Hmisc is no longer needed for sd()

# Enable Segoe UI font (ensure it's installed on your system)
# Check if "Segoe UI" font is available, if not, add it
if (!("Segoe UI" %in% font_families())) {
  font_add("Segoe UI", regular = "C:/Users/collodelu/OneDrive - centralbankmalta.org/Desktop/Projects/Uncertainty_surprises/code/segoeui.ttf") # Path to font might need adjustment based on user's setup
}
showtext_auto()

# Create output directories if they don't exist
output_dir <- paste0("../output/figures/", name_prompt_request)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#------------------------------------------------------------------------------
## 2. LOAD AND PROCESS LLM ASSESSMENT DATA
#------------------------------------------------------------------------------

# --- Read LLM cleaned results ---
# Note: Using Sys.Date() for the input filename as in your original code
clean_df <- read_xlsx(paste0("../intermediate_data/aggregate_gemini_result/prompt_naive/",
                           "2.5flash_",
                           Sys.Date(),
                           ".xlsx"))


#------------------------------------------------------------------------------
## 3. Figure: standard deviation over time for each tenor
#------------------------------------------------------------------------------

# Assuming your cleaned data is in `clean_df`
# Calculate standard deviation of rate by date and tenor

std_df <- clean_df %>%
  group_by(date, tenor) %>%
  summarise(
    # Changed standard deviation calculation to normal sd()
    std_rate = sd(rate, na.rm = TRUE),
    range_rate = max(rate) - min(rate),
    .groups = "drop"
  )

# Calculate 95th percentile threshold for each tenor
thresholds <- std_df %>%
  group_by(tenor) %>%
  summarise(p95 = quantile(std_rate, 0.95,na.rm=T), .groups = "drop")

# Join thresholds back to std_df and flag high values
std_df <- std_df %>%
  left_join(thresholds, by = "tenor") %>%
  mutate(highlight = std_rate > p95) %>%
  mutate(date =as.Date(date))


# Define custom color palette
color_palette <- c("10Y" = "#E41A1C", "2Y" = "#377EB8", "3M" = "#4DAF4A")

# Prepare the data with group-wise mean
std_df <- std_df %>%
  group_by(tenor) %>%
  mutate(mean = mean(std_rate, na.rm = TRUE)) %>%
  ungroup()

# Create the plot
ggplot(std_df, aes(x = date, y = std_rate, color = tenor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 1, alpha = 0.85) +
  geom_line(aes(y = mean), size = 1.2, linetype = "dashed", color = "black") +
  geom_point(aes(size = highlight), shape = 21, fill = "white", stroke = 1.2) +
  facet_wrap(~ tenor, nrow = 3, scales = "free_y") +
  scale_color_manual(values = color_palette) +
  scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = NULL,         # Removed title
    subtitle = NULL,      # Removed subtitle
    x = NULL,
    y = "Standard Deviation of Rate",
    color = "OIS Tenor",
    caption = "Source: Author's calculations using OIS data."
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50"),
    axis.text.x = element_text(angle = 270, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(output_dir, "sd.pdf"), # Changed to PDF and using file.path
       dpi = "retina",
       bg = "white")


#------------------------------------------------------------------------------
## 4. Table: compute correlation matrix across sd tenors
#------------------------------------------------------------------------------

cor_matrix <- std_df %>%
  select(date, tenor, std_rate) %>%
  pivot_wider(names_from = tenor, values_from = std_rate) %>%
  select(-date) %>%
  cor(use = "pairwise.complete.obs")

print(cor_matrix)


#------------------------------------------------------------------------------
## 5. Figure: difference between 10 years and 3months standard deviation
#------------------------------------------------------------------------------

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
  "Above 95th Percentile" = "#D7263D", # bold red
  "Below 5th Percentile" = "#1B9AAA", # deep blue
  "Normal" = "#CCCCCC" # soft gray
)

# Plot
ggplot(spread_df, aes(x = date, y = diff_10y_3m)) +
  geom_area(fill = "#F4F4F4", alpha = 0.5) +
  geom_line(color = "#333333", size = 0.8) +
  geom_point(aes(color = highlight), size = 4, alpha = 0.8) +
  geom_hline(yintercept = quantiles, linetype = "dotted", color = "#999999") +
  scale_color_manual(values = highlight_colors) +
  labs(
    title = NULL,         # Removed title
    subtitle = NULL,      # Removed subtitle
    x = "Date",
    y = "Std Dev Spread (10Y - 3M)",
    color = ""
  ) +
  theme_minimal(base_family = "Segoe UI") +
  scale_x_date(date_breaks = "2 years", date_labels = "%b %Y") + # Corrected line
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 270, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

# Save
ggsave(file.path(output_dir, "tenor_spread_sd.pdf"), # Changed to PDF and using file.path
       dpi = 320,
       width = 10,
       height = 6,
       bg = "white")


#------------------------------------------------------------------------------
## 6. Figure: percentage of direction for each tenor and date
#------------------------------------------------------------------------------

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
    title = NULL,         # Removed title
    subtitle = NULL,      # Removed subtitle
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
ggsave(file.path(output_dir, "direction_percentage_heatmap.pdf"), # Changed to PDF and using file.path
       dpi = 320,
       width = 10,
       height = 8,
       bg="white")


#------------------------------------------------------------------------------
## 7. Correlation between market-based measure and in-vitro llm measure
#------------------------------------------------------------------------------

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
    if (nrow(df) >= 14) { # Ensure enough data for a 14-period window
      df$rolling_corr <- rollapply(
        data = df[, c("std_rate", "correct_post_mean")],
        width = 14,
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

# Step 3: Plot the rolling correlation in storytelling format

# Define a professional and colorblind-friendly palette
color_palette <- c("10Y" = "#E41A1C", "2Y" = "#377EB8", "3M" = "#4DAF4A")

# Create the plot
ggplot(rolling_corr_df, aes(x = date, y = rolling_corr, color = tenor)) + # Keeping +0.1 as in your original code
  # Add a clear zero-reference line first, so it's in the background
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +

  # Use a slightly thicker line for better visibility
  geom_line(linewidth = 1, alpha = 0.8) +

  # Apply the custom color scale
  scale_color_manual(values = color_palette) +

  # Improve axis formatting for clarity
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.5, 1.0, by = 0.25)) +
  facet_wrap(~ tenor, nrow = 3) +
  # Add clear, informative labels and a caption
  labs(
    title = NULL,         # Removed title
    subtitle = NULL,      # Removed subtitle
    x = NULL, # The date axis is self-explanatory
    y = "Rolling Spearman Correlation",
    color = "OIS Tenor", # Set a clear legend title
    caption = "Source: Author's calculations using ECB transcripts and OIS data."
  ) +

  # Use a clean, modern theme as a base
  theme_minimal(base_family = "Segoe UI") +

  # Further refine theme elements for a publication-quality finish
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50"),
    panel.grid.minor = element_blank() # Declutter by removing minor grid lines
  )

ggsave(file.path(output_dir, "rolling_spearman_correlation.pdf"), # Changed to PDF and using file.path
       dpi = 320,
       width = 10,
       height = 8,
       bg="white")

#------------------------------------------------------------------------------
## 8. Table: Compute the mean and standard deviation of the rolling correlation by tenor
#------------------------------------------------------------------------------

rolling_corr_summary <- rolling_corr_df %>%
  group_by(tenor) %>%
  summarise(
    mean_corr = mean(rolling_corr, na.rm = TRUE),
    sd_corr = sd(rolling_corr, na.rm = TRUE),
    .groups = "drop"
  )

# Print the summary table
print(rolling_corr_summary)

#------------------------------------------------------------------------------
## 9. Mistakes for the E(x) over time
#------------------------------------------------------------------------------

mean_llm_df <- clean_df %>%
  group_by(date,tenor) %>%
  filter(grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) %>% # Keep only valid YYYY-MM-DD format
  mutate(date = as.Date(date)) |>
  summarise(rate = mean(rate, na.rm = TRUE), .groups = "drop") # Changed to normal mean()


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
ggplot(joined_df, aes(x = date, y = error)) +
  # Add a subtle zero-reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +

  # Plot the lines
  geom_line(linewidth = 0.8, col = "#377EB8") +
  facet_wrap(~ tenor, nrow=3) +

  # Format axes for clarity
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +

  # Add informative labels and a clear title
  labs(
    title = NULL,         # Removed title
    subtitle = NULL,      # Removed subtitle
    x = NULL, # Date axis is self-explanatory
    y = "OIS Rate (%)",
    color = NULL, # Legend title is not needed with descriptive labels
    caption = "Source: Author's calculations using ECB transcripts and OIS data."
  ) +

  # Apply a clean theme as a base
  theme_minimal(base_family = "Segoe UI") +

  # Refine theme elements for a publication-quality finish
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50"),
    panel.grid.minor = element_blank(),
    # Add a border around the facets to clearly separate them
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    strip.text = element_text(face = "bold", size = 12) # Make facet titles (10Y, 2Y, 3M) stand out
  )

ggsave(file.path(output_dir, "expected_value_error.pdf"), # Changed to PDF and using file.path
       dpi = 320,
       width = 10,
       height = 8,
       bg="white")