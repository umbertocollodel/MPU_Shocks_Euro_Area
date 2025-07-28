#==============================================================================
# SCRIPT: Analyze 'prompt_anchor_values' LLM Results
#==============================================================================
# This script processes LLM results from the 'prompt_anchor_values' analysis

# --- Define Parameters ---
# Set the name for this analysis run for consistent file naming
name_prompt_request <- "prompt_history_surprises"
# Set the batch size if it's a variable you use
# batch_size <- "your_batch_size"

# Load necessary libraries
library(tidyverse) # For data manipulation and plotting
library(readxl)    # For reading Excel files
library(showtext)  # For custom fonts
library(zoo)       # For rolling functions

# Enable Segoe UI font (ensure it's installed on your system)
# Check if "Segoe UI" font is available, if not, add it
if (!("Segoe UI" %in% font_families())) {
  font_add("Segoe UI", regular = "C:/Windows/Fonts/segoeui.ttf")
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
# Note: Using a fixed name for now. Update the paste0 call if batch_size and Sys.Date() are needed.
# Ensure the file path is correct based on your project structure
clean_df <- read_xlsx(paste0("../intermediate_data/llm_assessment_", name_prompt_request, "2025-07-18.xlsx"))


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
    title = NULL,         # Removed title
    subtitle = NULL,      # Removed subtitle
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
    strip.text = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10) # Ensure axis labels are readable
  )

print(plot_std_dev)

# --- Save the plot ---
ggsave(
  filename = file.path(output_dir, "sd.pdf"), # Changed to PDF
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
    title = NULL,         # Removed title
    subtitle = NULL,      # Removed subtitle
    x = NULL, y = "Uncertainty Spread (10Y - 3M)", color = ""
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10) # Ensure axis labels are readable
  )

print(plot_spread)

# --- Save the plot ---
ggsave(
  filename = file.path(output_dir, paste0(name_prompt_request, "_tenor_spread_sd.pdf")), # Changed to PDF
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
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 12)], # Show break every 12 meetings
                   labels = function(x) format(as.Date(x), "%Y-%m")) + # Format date for better readability
  labs(
    title = NULL,         # Removed title
    subtitle = NULL,      # Removed subtitle
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
    strip.text = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10) # Ensure axis labels are readable
  )

print(plot_direction)

# --- Save the plot ---
ggsave(
  filename = file.path(output_dir, "direction_percentage.pdf"), # Changed to PDF
  plot = plot_direction,
  dpi = 320, width = 12, height = 9, bg = "white"
)


------------------------------------------------------------------------------
## 7. Correlation between market-based measure and in-vitro llm measure (Updated Aesthetics)
#------------------------------------------------------------------------------

#  Load and prepare the data
range_df <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date, correct_post_mean)

# Assuming std_df is already loaded and contains: date, tenor, std_rate

combined_df <- range_df %>%
  inner_join(std_df, by = c("date", "tenor"))

# Define a professional and colorblind-friendly palette (matching the second script's style)
color_palette_corr <- c("10Y" = "#d73027", "2Y" = "#4575b4", "3M" = "#91bfdb")



#------------------------------------------------------------------------------
## 7a. Overall by tenor
#------------------------------------------------------------------------------

cor_by_tenor <- combined_df %>%
  group_by(tenor) %>%
  summarise(
    mean_corr = cor(std_rate, correct_post_mean, method = "spearman", use = "pairwise.complete.obs"),
    .groups = "drop"
  ) |> 
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) # Ensure tenor order is consistent


# New plot: Bar plot of mean correlation by tenor
ggplot(cor_by_tenor, aes(x = tenor, y = mean_corr, fill = tenor)) +
  geom_col(width=0.4,show.legend = FALSE) + # Bar plot, no legend needed for fill
  geom_text(aes(label = sprintf("%.2f", mean_corr)), vjust = -0.5, size = 3) + # Add value labels
  scale_fill_manual(values = color_palette_corr) + # Use the same color palette
  labs(
    title = NULL,
    subtitle = NULL,
    x = "OIS Tenor",
    y = "Overall Correlation",
    caption = "Source: Author's calculations."
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50"),
    panel.grid.major.x = element_blank(), # Remove major vertical grid lines
    panel.grid.minor.x = element_blank()  # Remove minor vertical grid lines
  )

ggsave(file.path(output_dir, "mean_spearman_correlation_bar.pdf"),
       dpi = 320,
       width = 8,
       height = 5,
       bg="white")


#------------------------------------------------------------------------------
## 7b. Rolling correlation by tenor
#------------------------------------------------------------------------------

# Step 2: Compute rolling Spearman correlation
# Ensure the data is sorted
combined_df <- combined_df %>% arrange(date)

# Select only the two columns needed
# Adjusted rollapply for robustness and consistency with second script
rolling_corr_df <- combined_df %>%
  select(date, tenor, std_rate, correct_post_mean) %>%
  group_by(tenor) %>%
  arrange(date) %>% # Ensure data is sorted within groups for rollapply
  mutate(
    rolling_corr = rollapply(
      data = cbind(std_rate, correct_post_mean),
      width = 12, # Used 12 as in the second script, instead of 14
      FUN = function(w) {
        if (sum(!is.na(w[, 1]) & !is.na(w[, 2])) >= 2) { # Check for at least 2 complete pairs
          cor(w[, 1], w[, 2], method = "spearman", use = "pairwise.complete.obs")
        } else {
          NA
        }
      },
      by.column = FALSE,
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup() 


# Step 3: Plot the rolling correlation in storytelling format


# Create the plot
ggplot(rolling_corr_df, aes(x = date, y = rolling_corr, color = tenor)) + # Removed +0.1 from y
  # Add a clear zero-reference line first, so it's in the background
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  # Use a slightly thicker line for better visibility
  geom_line(linewidth = 1, alpha = 0.9) + # Matched alpha

  # Apply the custom color scale
  scale_color_manual(values = color_palette_corr) +

  # Improve axis formatting for clarity
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-1.0, 1.0, by = 0.25), limits = c(-1.0, 1.0)) + # Matched limits and breaks
  facet_wrap(~ tenor, nrow = 3) +
  # Add clear, informative labels and a caption
  labs(
    title = NULL,
    subtitle = NULL,
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
    axis.text = element_text(size = 10), # Matched size
    panel.grid.minor = element_blank(), # Declutter by removing minor grid lines
    panel.border = element_rect(colour = "grey80", fill = NA), # Added for consistency
    strip.text = element_text(face = "bold", size = 12) # Matched size
  )

ggsave(file.path(output_dir, "rolling_spearman_correlation.pdf"),
       dpi = 320,
       width = 12, # Wider to match second script's correlation plot
       height = 7, # Adjusted height
       bg="white")

#------------------------------------------------------------------------------
## 8. Mistakes for the E(x) over time (Updated Aesthetics)
#------------------------------------------------------------------------------

mean_llm_df <- clean_df %>%
  group_by(date,tenor) %>%
  filter(grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) %>% # Keep only valid YYYY-MM-DD format
  mutate(date = as.Date(date)) |>
  summarise(llm_mean_rate = mean(rate, na.rm = TRUE), .groups = "drop") # Renamed 'rate' to 'llm_mean_rate' for clarity


actual_ois_df <- read_xlsx("../raw_data/ois_daily_data.xlsx",skip=1) %>%
  select(1,2,4,6) %>%
  setNames(c("date","3M","2Y","10Y")) %>%
  mutate(date = as.Date(date)) %>%
  pivot_longer(`3M`:`10Y`,names_to = "tenor",values_to = "actual_rate")


# Assuming mean_llm_df and actual_ois_df are already loaded in your environment
# Join and compute error
joined_df <- mean_llm_df %>% # Started with mean_llm_df for consistency
  inner_join(actual_ois_df,by=c("date","tenor")) %>% # Used inner_join to match second script
  mutate(error = actual_rate - llm_mean_rate) # Used llm_mean_rate


# Plot error by tenor over time
ggplot(joined_df, aes(x = date, y = error)) +
  # Add a subtle zero-reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") + # Matched color

  # Plot the lines
  geom_line(aes(color = tenor), linewidth = 0.9) + # Used tenor color, matched width
  facet_wrap(~ tenor, nrow=3, scales = "free_y") + # Added free_y scale
  scale_color_manual(values = color_palette_corr, guide = "none") + # Added manual color, no guide

  # Format axes for clarity
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") + # Changed to 3 years breaks

  # Add informative labels and a clear title
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL, # Date axis is self-explanatory
    y = "Forecast Error (bps)", # Consistent y-axis label
    # color = NULL, # Legend title is not needed with descriptive labels (guide="none")
    caption = "Source: Author's calculations using ECB transcripts and OIS data."
  ) +

  # Apply a clean theme as a base
  theme_minimal(base_family = "Segoe UI") +

  # Refine theme elements for a publication-quality finish
  theme(
    # legend.position = "top", # Removed as guide="none"
    # legend.text = element_text(size = 11), # Removed as guide="none"
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50"),
    axis.text = element_text(size = 10), # Matched size
    panel.grid.minor = element_blank(),
    # Add a border around the facets to clearly separate them
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.5), # Matched color and linewidth
    strip.text = element_text(face = "bold", size = 12) # Make facet titles (10Y, 2Y, 3M) stand out
  )

ggsave(file.path(output_dir, "expected_value_error.pdf"),
       dpi = 320,
       width = 10,
       height = 8,
       bg="white")
















