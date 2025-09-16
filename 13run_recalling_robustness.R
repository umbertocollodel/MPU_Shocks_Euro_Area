# Plot LLM disagreement for data after September 2024 - Naive prompt

library(tidyverse)
library(readxl)

# Load data
clean_df <- read_xlsx("../intermediate_data/aggregate_gemini_result/prompt_naive/2.5flash_2025-07-21.xlsx")

# Calculate standard deviation by date and tenor
std_df <- clean_df %>%
  # Clean date format first
  mutate(date = str_extract(date, "\\d{4}-\\d{2}-\\d{2}")) %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, tenor) %>%
  summarise(std_rate = sd(rate, na.rm = TRUE), .groups = "drop") %>%
  filter(date > as.Date("2024-09-01")) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

# Color palette
color_palette_tenors <- c("10Y" = "#d73027", "2Y" = "#4575b4", "3M" = "#91bfdb")

# Plot
ggplot(std_df, aes(x = date, y = std_rate)) +
  geom_line(aes(color = tenor), linewidth = 0.8) +
  facet_wrap(~ tenor, nrow = 3, scales = "free_y") +
  scale_color_manual(values = color_palette_tenors, guide = "none") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    x = NULL,
    y = "Standard Deviation of Predicted Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.text = element_text(face = "bold")
  )
