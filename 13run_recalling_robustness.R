# Plot LLM disagreement with ECB narrative annotations

library(tidyverse)
library(readxl)

# Load LLM data
clean_df <- read_xlsx("../intermediate_data/aggregate_gemini_result/prompt_naive/2.5flash_2025-07-21.xlsx")

# Calculate LLM standard deviation by date and tenor
llm_std_df <- clean_df %>%
  mutate(date = str_extract(date, "\\d{4}-\\d{2}-\\d{2}")) %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, tenor) %>%
  summarise(std_rate = sd(rate, na.rm = TRUE), .groups = "drop") %>%
  filter(date > as.Date("2024-09-01")) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

# Define ECB meeting dates and expected disagreement direction
ecb_events <- tribble(
  ~date, ~event, ~narrative, ~direction_3m, ~direction_2y, ~direction_10y,
  as.Date("2024-10-17"), "Oct 2024\nLjubljana", "Continued easing cycle", "STABLE", "UP", "UP",
  as.Date("2024-12-12"), "Dec 2024\nFrankfurt", "4th consecutive cut\n100bp total for 2024", "STABLE", "STABLE", "UP", 
  as.Date("2025-01-30"), "Jan 2025\nFrankfurt", "5th cut to 2.75%\nRuled out 50bp cuts", "UP", "UP", "UP",
  as.Date("2025-03-06"), "Mar 2025\nFrankfurt", "Growth marked down\nTrade uncertainty cited", "UP", "UP", "UP",
  as.Date("2025-04-17"), "Apr 2025\nFrankfurt", "Continued easing\nTrade tensions", "STABLE", "UP", "STABLE",
  as.Date("2025-06-05"), "Jun 2025\nFrankfurt", "Summer pause questions\nGeopolitical risks", "UP", "STABLE", "UP"
)

# Create direction mapping for colors and shapes
direction_colors <- c(
  "UP" = "#d73027",        # Red - increasing disagreement
  "DOWN" = "#2E8B57",      # Green - decreasing disagreement  
  "STABLE" = "#4575b4"     # Blue - stable disagreement
)

direction_shapes <- c(
  "UP" = 24,      # Triangle pointing up
  "DOWN" = 25,    # Triangle pointing down
  "STABLE" = 22   # Square
)

# Reshape ECB events for plotting annotations
ecb_annotations <- ecb_events %>%
  pivot_longer(
    cols = starts_with("direction_"),
    names_to = "tenor_direction",
    values_to = "direction"
  ) %>%
  mutate(
    tenor = case_when(
      tenor_direction == "direction_3m" ~ "3M",
      tenor_direction == "direction_2y" ~ "2Y", 
      tenor_direction == "direction_10y" ~ "10Y"
    )
  ) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

# Main plot
ggplot(llm_std_df, aes(x = date, y = std_rate)) +
  geom_line(color = "#d73027", linewidth = 0.8) +
  
  # Add vertical lines for ECB meetings
  geom_vline(
    data = ecb_events,
    aes(xintercept = date),
    linetype = "dotted", 
    color = "grey60",
    alpha = 0.7
  ) +
  
  # Add ECB event points showing expected direction
  geom_point(
    data = ecb_annotations,
    aes(x = date, y = Inf, fill = direction, shape = direction),
    size = 4,
    color = "white",
    stroke = 0.5
  ) +
  
  # Add event labels
  geom_text(
    data = ecb_events,
    aes(x = date, y = Inf, label = event),
    hjust = 0.5,
    vjust = 1.8,
    size = 2.5,
    angle = 90,
    color = "grey30"
  ) +
  
  facet_wrap(~ tenor, nrow = 3, scales = "free_y") +
  
  scale_fill_manual(
    values = direction_colors,
    name = "Expected\nDirection"
  ) +
  
  scale_shape_manual(
    values = direction_shapes,
    name = "Expected\nDirection"
  ) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  
  labs(
    title = "LLM Disagreement with Expected Direction from ECB Communication",
    subtitle = "Shapes show expected disagreement direction: ▲ UP, ▼ DOWN, ■ STABLE",
    x = NULL,
    y = "Standard Deviation of LLM Rate Predictions",
    caption = "ECB meetings marked with vertical lines. Symbols indicate expected disagreement direction vs previous meeting."
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey30"),
    plot.caption = element_text(size = 10, color = "grey50")
  )