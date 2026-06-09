# Description: Process US economic release dates and analyze overlaps with ECB meetings


# Clean and combine all different US economic release date files: -------

# Set base path - use relative path from project root
# Place US release data files in ../raw_data/us_releases/
base_path <- "../raw_data/us_releases/"

# Check if directory exists, if not provide helpful error message
if (!dir.exists(base_path)) {
  stop(paste0("Directory not found: ", base_path,
              "\nPlease create the directory and place US release data files there:\n",
              "  - release_dates_50.xlsx (Employment Situation)\n",
              "  - release_dates_10.xlsx (CPI)\n",
              "  - release_dates_53.xlsx (GDP)\n",
              "  - release_dates_9.xlsx (Retail Sales)"))
}

# Read each file (sheet 2)
es <- read_excel(paste0(base_path, "release_dates_50.xlsx"), sheet = 2)
cpi <- read_excel(paste0(base_path, "release_dates_10.xlsx"), sheet = 2)
gdp <- read_excel(paste0(base_path, "release_dates_53.xlsx"), sheet = 2)
rs <- read_excel(paste0(base_path, "release_dates_9.xlsx"), sheet = 2)

# Add variable names
es$variable <- "ES"
cpi$variable <- "CPI"
gdp$variable <- "GDP"
rs$variable <- "RS"


source=c("US Bureau of Labor Statistics", "US Bureau of Labor Statistics","US Bureau of Economic Analysis", "US Census Bureau")

# Combine all
calendar <- bind_rows(es, cpi, gdp, rs) %>%
  mutate(release_date = as.Date(`Release Dates`)) %>%
  filter(release_date >= as.Date("1998-01-01") & release_date <= as.Date("2025-10-31")) %>%
  arrange(release_date) |> 
  select(release_date, variable) %>%
  split(.$variable) %>%
  map2(source, ~ mutate(.x, source = .y)) %>%
  bind_rows()

# Save
writexl::write_xlsx(calendar, paste0("../intermediate_data/us_economic_releases.xlsx"))


# Analyze overlaps with ECB meetings: -------

# Read ECB dates
ecb <- read_excel("../raw_data/dates_govc.xlsx")
ecb_dates <- as.Date(paste(ecb$year, ecb$month, ecb$day, sep = "-"))

# Check overlaps with 3-day window
calendar$days_to_ecb <- sapply(calendar$release_date, function(d) {
  min(abs(as.numeric(d - ecb_dates)))
})

calendar$ecb_overlap <- calendar$days_to_ecb == 0  # Same day
calendar$ecb_window_3 <- calendar$days_to_ecb <= 3  # Within 3 days

# Match your paper's style
theme_paper <- function() {
  theme_bw() +
  theme(
    text = element_text(family = "Segoe UI Light"),
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 18)
  )
}

# Figure 1: Comparison same day vs 3-day window
calendar %>%
  group_by(variable) %>%
  summarise(
    `Same Day` = 100 * mean(ecb_overlap),
    `±3 Days` = 100 * mean(ecb_window_3)
  ) %>%
  pivot_longer(`Same Day`:`±3 Days`, names_to = "Window", values_to = "pct") %>%
  ggplot(aes(x = reorder(variable, pct), y = pct, fill = Window)) +
  geom_col(width = 0.7, position = position_dodge(0.8), alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f", pct)), 
            position = position_dodge(0.8), hjust = -0.1, size = 5) +
  coord_flip() +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  labs(title = "", x = "", y = "%", caption = "") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 0))

ggsave("../output/figures/overlap_us_releases_ecb_comparison.pdf", dpi = 320, width = 12, height = 9, bg = "white")

# Figure 2: Time series with 3-day window only
calendar %>%
  mutate(year = year(release_date)) %>%
  group_by(year) %>%
  summarise(overlap_rate = 100 * mean(ecb_window_3)) %>%
  ggplot(aes(x = year, y = overlap_rate)) +
  geom_line(size = 0.8, color = "#F8766D") +
  labs(title = "", x = "", y = "%", caption = "") +
  theme_paper()

ggsave("../output/figures/overlap_us_releases_ecb_time.pdf", dpi = 320, width = 12, height = 9, bg = "white")

# Figure 3: By indicator over time with 3-day window only
calendar %>%
  mutate(year = year(release_date)) %>%
  group_by(year, variable) %>%
  summarise(overlap_rate = 100 * mean(ecb_window_3)) %>%
  ggplot(aes(x = year, y = overlap_rate)) +
  geom_line(size = 0.8, color = "#F8766D") +
  facet_wrap(~variable) +
  labs(title = "", x = "", y = "%", caption = "") +
  theme_paper() +
  theme(legend.position = "none")

ggsave("../output/figures/overlap_us_releases_ecb_time_indicator.pdf", dpi = 320, width = 12, height = 9, bg = "white")

# Check ECB meeting frequency over time
ecb %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  arrange(date) %>%
  mutate(days_between = as.numeric(date - lag(date))) %>%
  mutate(period = case_when(
    year < 2010 ~ "Pre-2010",
    year >= 2010 & year < 2015 ~ "2010-2014",
    year >= 2015 ~ "2015+"
  )) %>%
  group_by(period) %>%
  summarise(
    n_meetings = n(),
    avg_days_between = mean(days_between, na.rm = TRUE),
    meetings_per_year = n() / n_distinct(year)
  )

# Check ES release pattern
calendar %>%
  filter(variable == "ES") %>%
  mutate(weekday = wday(release_date, label = TRUE)) %>%
  count(weekday)  # Should confirm it's always Friday