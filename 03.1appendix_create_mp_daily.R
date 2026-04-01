# ==============================================================================
# 03.1appendix_create_mp_daily.R
# Purpose: Construct MP surprises as avg(close 3 days post) - avg(close 3 days
#          pre) GovC, then compare with ECB MPD standard surprises
# ==============================================================================

# Prepare environment: -----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, lubridate, showtext, sysfonts)

if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) font_add("Segoe UI", regular = font_path)
}
showtext_auto()

# Load objects if not in environment: -----

if (!exists("differences_df")) {
  differences_df <- readRDS("../intermediate_data/range_difference_df.rds")
}

dates <- read_xlsx("../raw_data/dates_govc.xlsx") %>%
  unite("date", day:year, sep = "-", remove = TRUE) %>%
  mutate(date = as.Date(date, "%d-%m-%Y")) %>%
  .$date

# Read OIS daily data (last = closing bid rate): -----

names_sheets <- excel_sheets("../raw_data/daily_OIS_updated15Sept_2025..xls")
tenors       <- names_sheets %>% str_remove("_.*")

ois_daily <- names_sheets %>%
  set_names(tenors) %>%
  map(~ read_excel("../raw_data/daily_OIS_updated15Sept_2025..xls", sheet = .x) %>%
        select(1:5) %>%
        setNames(c("daily", "high", "low", "first", "last")) %>%
        mutate(daily = as.Date(daily), last = as.numeric(last)) %>%
        arrange(daily))

# ==============================================================================
# Construct MP surprises: avg(last, 3 days post) - avg(last, 3 days pre)
# ==============================================================================

mp_daily_df <- ois_daily %>%
  imap_dfr(function(df, tenor_name) {
    map_dfr(dates, function(d) {
      pre  <- df %>% arrange(desc(daily)) %>% filter(daily < d) %>% slice(1:3) %>% pull(last)
      post <- df %>% arrange(daily)      %>% filter(daily > d) %>% slice(1:3) %>% pull(last)

      if (all(is.na(pre)) || all(is.na(post))) return(NULL)

      tibble(
        date     = d,
        mp_daily = (mean(post, na.rm = TRUE) - mean(pre, na.rm = TRUE)) * 100
      )
    }) %>%
      mutate(tenor = tenor_name)
  }) %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

# Load ECB MPD standard surprises (1-day window): -----

parse_mpd_date <- function(date_char) {
  map_chr(date_char, function(x) {
    if (is.na(x)) return(NA_character_)
    if (str_detect(x, "/"))           return(as.character(dmy(x)))
    if (str_detect(x, "^[0-9]{5}$")) return(as.character(as.Date(as.numeric(x), origin = "1899-12-30")))
    if (str_detect(x, "^[0-9]{4}-")) return(x)
    NA_character_
  }) %>% as.Date()
}

mpd_surprises <- read_excel("../raw_data/00EA_MPD_update_june2025.xlsx", sheet = 4) %>%
  mutate(date = parse_mpd_date(as.character(date))) %>%
  select(date, OIS_3M, OIS_6M, OIS_1Y, OIS_2Y, OIS_5Y, OIS_10Y) %>%
  setNames(c("date", "3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")) %>%
  pivot_longer(-date, names_to = "tenor", values_to = "mp_ecb") %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

# Merge: -----

comparison_df <- mp_daily_df %>%
  inner_join(mpd_surprises, by = c("date", "tenor")) %>%
  filter(!is.na(mp_daily), !is.na(mp_ecb))

# Correlations by tenor: -----

cor_df <- comparison_df %>%
  split(.$tenor) %>%
  map_dfr(~ {
    test <- cor.test(.x$mp_daily, .x$mp_ecb, method = "pearson")
    tibble(
      tenor = unique(.x$tenor),
      r     = round(test$estimate, 2),
      sig   = case_when(test$p.value < 0.01 ~ "***",
                        test$p.value < 0.05 ~ "**",
                        test$p.value < 0.10 ~ "*",
                        TRUE ~ "")
    )
  }) %>%
  mutate(label = paste0("r = ", r, sig),
         tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

dir.create("../output/figures", recursive = TRUE, showWarnings = FALSE)

# Figure: scatterplot daily MP surprise vs ECB MPD surprise: -----

comparison_df %>%
  left_join(cor_df %>% select(tenor, label), by = "tenor") %>%
  ggplot(aes(mp_ecb, mp_daily)) +
  geom_point(size = 2, alpha = 0.5, color = "#4575b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#d73027", fill = "#d73027",
              alpha = 0.15, linewidth = 0.8) +
  geom_text(aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.5, size = 5,
            color = "grey30", family = "Segoe UI Light",
            inherit.aes = FALSE) +
  facet_wrap(~ tenor, scales = "free", nrow = 2) +
  labs(x = "ECB MPD Surprise (Bps)", y = "Daily-Window MP Surprise (Bps)", caption = "") +
  theme_bw() +
  theme(text        = element_text(family = "Segoe UI Light"),
        axis.text   = element_text(size = 16),
        axis.title  = element_text(size = 18),
        strip.text  = element_text(size = 18, face = "bold"))

ggsave("../output/figures/mp_daily_vs_ecb_scatter.pdf",
       dpi = 320, width = 14, height = 10, bg = "white")

# Save intermediate data: -----

mp_daily_df %>%
  saveRDS("../intermediate_data/mp_daily_df.rds")

# ==============================================================================
# R2 request: correlation between home-built first-moment surprise (mp_daily)
# and MPU (diff_3) — same data, same 3-day window, mean vs. dispersion.
# If uncorrelated, MPU genuinely isolates the second moment within the design.
# ==============================================================================

mpu_vs_mp <- mp_daily_df %>%
  inner_join(differences_df %>% select(date, tenor, diff_3), by = c("date", "tenor")) %>%
  filter(!is.na(mp_daily), !is.na(diff_3))

cor_mpu_mp <- mpu_vs_mp %>%
  split(.$tenor) %>%
  map_dfr(~ {
    test <- cor.test(.x$mp_daily, .x$diff_3, method = "pearson")
    tibble(
      tenor = unique(.x$tenor),
      r     = round(test$estimate, 3),
      p     = round(test$p.value,  3),
      sig   = case_when(test$p.value < 0.01 ~ "***",
                        test$p.value < 0.05 ~ "**",
                        test$p.value < 0.10 ~ "*",
                        TRUE ~ "")
    )
  }) %>%
  mutate(
    label = paste0("r = ", r, sig),
    tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))
  )

cat("\n=== Correlation: home-built MP surprise vs MPU (same window, same data) ===\n\n")
print(cor_mpu_mp %>% select(tenor, r, p, sig), n = Inf)

# Figure: scatter of MPU vs home-built MP surprise, by tenor -----

mpu_vs_mp %>%
  left_join(cor_mpu_mp %>% select(tenor, label), by = "tenor") %>%
  ggplot(aes(mp_daily, diff_3)) +
  geom_point(size = 2, alpha = 0.5, color = "#4575b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#d73027", fill = "#d73027",
              alpha = 0.15, linewidth = 0.8) +
  geom_text(aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.5, size = 5,
            color = "grey30", family = "Segoe UI Light",
            inherit.aes = FALSE) +
  facet_wrap(~ tenor, scales = "free", nrow = 2) +
  labs(
    x = "Home-Built MP Surprise (Bps, 3-day window)",
    y = "MPU Surprise (Bps, 3-day window)",
    caption = ""
  ) +
  theme_bw() +
  theme(text       = element_text(family = "Segoe UI Light"),
        axis.text  = element_text(size = 18),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 18, face = "bold"))

ggsave("../output/figures/mpu_vs_mp_daily_scatter.pdf",
       dpi = 320, width = 14, height = 10, bg = "white")
