#==============================================================================
# SCRIPT: Analyze "LLM-as-Judge" Results
#==============================================================================
# This script processes LLM results from the Juge-as-LLM analysis


# --- Install and Load Packages ---
# install.packages("jsonlite") # Only if not already installed
# install.packages("dplyr")    # Only if not already installed
# install.packages("ggplot2")  # Only if not already installed
# install.packages("tidyr")    # Only if not already installed
# install.packages("lubridate")# Only if not already installed
# install.packages("RColorBrewer") # Only if not already installed

library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(RColorBrewer)

# --- Load and Prepare Data ---
# Load the JSON file containing the optimization history.
# This file is expected to contain results from an LLM 'Judge'
# on its predicted rates and their standard deviations,
# alongside actual historical volatility.
judge_df <- fromJSON("optimization_history_in_sample_training_20250731_124608.json")

# Convert to a data frame and clean up the data structure.
# This involves flattening nested JSON structures and converting data types.
judge_df <- as_tibble(judge_df) %>%
  split(.$iteration) %>% # Split by iteration to handle potential nesting per iteration
  map(~ .x |> unnest(detailed_results_training)) |> # Unnest the detailed results for each iteration
  bind_rows() %>% # Combine all unnested data back into a single data frame
  mutate(
    transcript_date = as.Date(transcript_date), # Convert date column to Date type
    iteration = as.factor(iteration), # Convert iteration to factor for plotting
    tenor = as.factor(tenor) # Ensure tenor is treated consistently as a factor
  )

# --- Data Transformation for Unified Plotting ---
# Reshape the data to have 'predicted_sd' and 'actual_volatility' in a single column
# This allows 'Actual Volatility' to be treated as a separate category for faceting or grouping.
judge_df_long <- judge_df %>%
  select(transcript_date, tenor, iteration, predicted_sd, actual_volatility) %>%
  pivot_longer(
    cols = c(predicted_sd, actual_volatility),
    names_to = "series_type_raw", # Temporary column for the original name
    values_to = "value" # New column for the numerical value (SD or volatility)
  ) %>%
  mutate(
    # Create a unified 'category' for faceting/x-axis
    # For predicted_sd, use the prompt iteration. For actual_volatility, use a distinct label.
    plot_category = case_when(
      series_type_raw == "predicted_sd" ~ paste("Prompt", iteration),
      series_type_raw == "actual_volatility" ~ "Actual Volatility",
      TRUE ~ NA_character_ # Should not happen
    ),
    # Factorize 'plot_category' for desired ordering in plots (Prompts 1-5, then Actual)
    plot_category = factor(plot_category, levels = c(paste("Prompt", 1:5), "Actual Volatility")),
    # Create a cleaner 'series_type' label for legends if needed
    series_type = factor(series_type_raw, levels = c("predicted_sd", "actual_volatility"),
                         labels = c("LLM Predicted SD", "Actual Volatility"))
  )


# --- Define Custom Color Palettes ---
# These palettes are chosen for clarity and distinction across different
# prompts (LLM iterations) and interest rate tenors.

# Colors for prompts (iterations 1-5)
prompt_colors <- brewer.pal(n = 5, name = "Dark2")
names(prompt_colors) <- paste("Prompt", 1:5) # Name the colors for 'Prompt X' categories

# Color for the actual volatility series
actual_vol_color <- "black"

# Combine prompt colors with actual volatility color for scales that use 'plot_category'
all_plot_category_colors <- c(prompt_colors, "Actual Volatility" = actual_vol_color)

# Colors for tenors (10Y, 2Y, 3MNT) - adapting from a scientific visualization reference
tenor_colors <- c("10Y" = "#d73027", "2Y" = "#4575b4", "3MNT" = "#91bfdb") # Red, Dark Blue, Light Blue


# ==============================================================================
## PLOT 1: LLM Response Uncertainty (Standard Deviation) - Grouped by Tenor with Separate Actual Panel
# This plot visualizes the standard deviation of predicted rates from each
# LLM prompt ('iteration') over time, grouped by interest rate tenor.
# The 'actual_volatility' is now shown in its own separate column of facets
# for direct comparison with predicted values.
# ==============================================================================

plot1 <- ggplot(judge_df_long, aes(x = transcript_date, y = value)) +
  # Use geom_line for all series
  geom_line(aes(color = plot_category, group = plot_category),
            # Filter to only show actual_volatility in its specific facet column
            data = . %>% filter(series_type_raw == "actual_volatility"),
            color = actual_vol_color, linewidth = 0.9, alpha = 0.9) +
  # Filter to show predicted_sd in its specific facet column, colored by prompt
  geom_line(aes(color = plot_category, group = plot_category),
            data = . %>% filter(series_type_raw == "predicted_sd"),
            linewidth = 0.8, alpha = 0.8) +
  # Facet by tenor (rows) and series_type_raw (columns for Predicted vs Actual)
  facet_grid(tenor ~ series_type, scales = "free_y") +
  labs(
    title = "LLM Predicted vs. Actual Volatility of Rates by Tenor",
    y = "Standard Deviation of Rate", # Simplified label
    x = NULL, # No x-axis label for cleaner look
    color = "Series Type" # Legend for line colors (Prompts)
  ) +
  # Manual color scale for prompts (only applies to predicted_sd in its column)
  scale_color_manual(values = all_plot_category_colors,
                     breaks = names(prompt_colors)) + # Only show prompt colors in legend
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + # Format x-axis dates
  theme_minimal(base_family = "Segoe UI") + # Set base font
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_rect(colour = "grey80", fill = NA), # Add border around each facet
    strip.text = element_text(face = "bold", size = 12), # Make facet titles bold
    axis.text = element_text(size = 10), # Ensure axis labels are readable
    legend.position = "right", # Place legend on the right
    legend.box = "vertical", # Stack legend items vertically
    legend.title = element_text(face = "bold")
  )

print(plot1)


# ==============================================================================
## PLOT 2: Distribution of LLM Response Uncertainty - Box Plots including Actual Volatility
# This plot provides a statistical summary (median, quartiles, outliers)
# of both predicted standard deviations (per LLM prompt) and actual volatility,
# separated by tenor. This allows for direct comparison of distributions.
# ==============================================================================

plot2 <- ggplot(judge_df_long, aes(x = plot_category, y = value, fill = plot_category)) +
  geom_boxplot(alpha = 0.7) +
  # Facet by tenor, with independent y-axes
  facet_wrap(~ tenor, scales = "free_y", ncol = length(unique(judge_df_long$tenor))) +
  labs(
    title = "Distribution of LLM Predicted vs. Actual Volatility by Tenor",
    y = "Standard Deviation of Rate", # Simplified label
    x = "Series / Prompt" # Unified label for x-axis categories
  ) +
  # Manual fill scale for all plot categories (prompts + actual)
  scale_fill_manual(values = all_plot_category_colors) +
  # Apply a clean, minimal theme with custom font
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_rect(colour = "grey80", fill = NA), # Add border around each facet
    strip.text = element_text(face = "bold", size = 12), # Make facet titles bold
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1), # Angle x-axis labels for readability
    axis.text.y = element_text(size = 10),
    legend.position = "none" # No legend needed as fill is mapped to x-axis categories
  )

print(plot2)


# ==============================================================================
## PLOT 3: LLM Response Uncertainty - Facetted by Prompt including Separate Actual Panel
# This plot displays the standard deviation of predicted rates for each
# tenor over time, grouped by each LLM prompt. A dedicated facet for
# 'Actual Volatility' is added to allow comparison of tenor trends
# against the real-world data under one panel.
# ==============================================================================

plot3 <- ggplot(judge_df_long, aes(x = transcript_date, y = value, color = tenor, group = tenor)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  # Facet by the combined plot_category (Prompts 1-5 and Actual Volatility)
  facet_wrap(~ plot_category, scales = "free_y", ncol = 2) + # Adjust ncol as needed
  labs(
    title = "LLM Predicted vs. Actual Volatility of Rates by Prompt",
    y = "Standard Deviation of Rate", # Simplified label
    x = NULL, # No x-axis label for cleaner look
    color = "Tenor" # Legend title for tenors
  ) +
  # Manual color scale for tenors
  scale_color_manual(values = tenor_colors) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + # Format x-axis dates
  theme_minimal(base_family = "Segoe UI") + # Set base font
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.border = element_rect(colour = "grey80", fill = NA), # Add border around each facet
    strip.text = element_text(face = "bold", size = 12), # Make facet titles bold
    axis.text = element_text(size = 10), # Ensure axis labels are readable
    legend.position = "right", # Place legend on the right
    legend.box = "vertical", # Stack legend items vertically
    legend.title = element_text(face = "bold")
  )

print(plot3)