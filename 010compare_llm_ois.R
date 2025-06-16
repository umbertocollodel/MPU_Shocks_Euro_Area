ois_df <- read_rds("../intermediate_data/range_difference_df.rds") %>% 
  filter(tenor == "10Y") %>% 
  mutate(horizon = "Long-term")


library(dplyr)
library(ggplot2)

ois_df %>%
  inner_join(clean_df, by = c("horizon", "date")) %>%
  write_xlsx("~/../Desktop/problem_cor.xlsx")
  ggplot(aes(x = diff, y = conf_score, label = date)) +
  geom_text(size = 3, color = "red") +  # Use text labels instead of points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
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
    x = "Δσ (Change in OIS Std Dev)",
    y = "Confusion Score",
    title = "Confusion Scores vs OIS Dispersion",
    subtitle = "Each label is a press conference date\nBlue line = linear regression",
    caption = "Source: Internal model outputs"
  )

 summarise(correlation = cor(diff,conf_score))

cor