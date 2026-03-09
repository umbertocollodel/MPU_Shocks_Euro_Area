# ==============================================================================
# 03.2relationship_liquidity_mpu_testing.R
# Purpose: Compute ask-bid spread around GovC meetings and correlate with MPU.
#          Bid close = last col from daily OIS; Ask close = ASK col from
#          ask_quotes_daily.xlsx. Liquidity measure = avg(spread, 3 days post)
#          - avg(spread, 3 days pre).
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

tenors <- c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")

# Read bid close (last column from daily OIS data): -----

names_sheets <- excel_sheets("../raw_data/daily_OIS_updated15Sept_2025..xls")

bid_daily <- names_sheets %>%
  set_names(tenors) %>%
  map(~ read_excel("../raw_data/daily_OIS_updated15Sept_2025..xls", sheet = .x) %>%
        select(1:5) %>%
        setNames(c("daily", "high", "low", "first", "last")) %>%
        mutate(daily = as.Date(daily), bid_close = as.numeric(last)) %>%
        select(daily, bid_close) %>%
        arrange(daily))

# Read ask quotes: -----

ask_raw <- read_excel("../raw_data/ask_quotes_daily.xlsx", skip = 1)

# Split into list of 2-column pairs, rename, convert, bind
col_pairs <- list(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12))

ask_long <- map2_dfr(col_pairs, tenors, function(cols, tenor_name) {
  ask_raw[, cols] %>%
    setNames(c("daily", "ask_close")) %>%
    mutate(daily     = as.Date(daily),
           ask_close = as.numeric(ask_close)) %>%
    filter(!is.na(daily), !is.na(ask_close)) %>%
    mutate(tenor = tenor_name)
})

# Compute ask-bid spread: -----

spread_df <- ask_long %>%
  left_join(bid_daily %>% bind_rows(.id = "tenor"), by = c("daily", "tenor")) %>%
  filter(!is.na(bid_close)) %>%
  mutate(spread = ask_close - bid_close)

cat("spread_df rows:", nrow(spread_df), "\n")

# ==============================================================================
# Liquidity measure: avg(spread, 3 days post) - avg(spread, 3 days pre) GovC
# ==============================================================================

liquidity_df <- map_dfr(tenors, function(tenor_name) {
  df <- spread_df %>% filter(tenor == tenor_name)
  map_dfr(dates, function(d) {
    pre  <- df %>% filter(daily %in% seq(d - 3, d - 1, by = "day")) %>% pull(spread)
    post <- df %>% filter(daily %in% seq(d + 1, d + 3, by = "day")) %>% pull(spread)

    if (length(pre) == 0 || length(post) == 0) return(NULL)
    if (all(is.na(pre)) || all(is.na(post))) return(NULL)

    tibble(
      date            = d,
      tenor           = tenor_name,
      pre_avg_spread  = mean(pre,  na.rm = TRUE),
      post_avg_spread = mean(post, na.rm = TRUE),
      liq_measure     = mean(post, na.rm = TRUE) - mean(pre, na.rm = TRUE)
    )
  })
})

cat("liquidity_df rows:", nrow(liquidity_df), "| columns:", paste(names(liquidity_df), collapse=", "), "\n")

if (nrow(liquidity_df) == 0 || !"tenor" %in% names(liquidity_df)) {
  stop("liquidity_df is empty. Check the diagnostic output above to identify where data is lost.")
}

liquidity_df <- liquidity_df %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

# Merge with MPU: -----

mpu_liq_df <- liquidity_df %>%
  inner_join(differences_df %>% select(date, tenor, diff_3), by = c("date", "tenor")) %>%
  filter(!is.na(liq_measure), !is.na(diff_3)) %>%
  mutate(liq_measure = liq_measure*100) #convert to bps the bid-ask spread change

# Correlations by tenor: -----

cor_df <- mpu_liq_df %>%
  split(.$tenor) %>%
  map_dfr(~ {
    test <- cor.test(.x$liq_measure, .x$diff_3, method = "pearson")
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

# Figure: scatterplot liquidity measure vs MPU: -----

mpu_liq_df %>%
  left_join(cor_df %>% select(tenor, label), by = "tenor") %>%
  ggplot(aes(diff_3, liq_measure)) +
  geom_point(size = 2, alpha = 0.5, color = "#4575b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#d73027", fill = "#d73027",
              alpha = 0.15, linewidth = 0.8) +
  geom_text(aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.5, size = 5,
            color = "grey30", family = "Segoe UI Light",
            inherit.aes = FALSE) +
  facet_wrap(~ tenor, scales = "free", nrow = 2) +
  labs(x = "MPU Surprise (Bps)", y = "Ask-Bid Spread Change Around GovC (Bps)", caption = "") +
  theme_bw() +
  theme(text       = element_text(family = "Segoe UI Light"),
        axis.text  = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18, face = "bold"))

ggsave("../output/figures/liquidity_vs_mpu_scatter.pdf",
       dpi = 320, width = 14, height = 10, bg = "white")
