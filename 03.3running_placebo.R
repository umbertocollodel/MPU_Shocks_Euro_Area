# ==============================================================================
# 03.3running_placebo.R
# Purpose: Placebo test — 1000 draws of non-announcement days.
#   - Exclusion window: ±10 trading days around each GovC
#   - Figure 1: faceted histogram by maturity (mean MPU per tenor)
#   - Figure 2: pooled MPU — mean and % positive (two-panel)
# ==============================================================================

# Prepare environment: -----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, lubridate, showtext, sysfonts, patchwork)

if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) font_add("Segoe UI", regular = font_path)
}
showtext_auto()

if (!exists("differences_df")) {
  differences_df <- readRDS("../intermediate_data/range_difference_df.rds")
}

dates <- read_xlsx("../raw_data/dates_govc.xlsx") %>%
  unite("date", day:year, sep = "-", remove = TRUE) %>%
  mutate(date = as.Date(date, "%d-%m-%Y")) %>%
  .$date

# Load OIS daily data: -----

names_sheets <- excel_sheets("../raw_data/daily_OIS_updated15Sept_2025..xls")
tenors_ois   <- names_sheets %>% str_remove("_.*")

gap_df <- names_sheets %>%
  set_names(tenors_ois) %>%
  map(~ read_excel("../raw_data/daily_OIS_updated15Sept_2025..xls", sheet = .x) %>%
        select(1:5) %>%
        setNames(c("daily", "high", "low", "first", "last")) %>%
        mutate(daily = as.Date(daily),
               gap   = as.numeric(high) - as.numeric(low)) %>%
        filter(!is.na(gap)) %>%
        select(daily, gap)) %>%
  bind_rows(.id = "tenor") %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

# ==============================================================================
# Part 1: Eligible trading days (±10 trading days exclusion window)
# ==============================================================================

sample_start     <- as.Date("2005-01-01")
sample_end       <- as.Date("2025-09-30")
exclusion_window <- 10L

trading_days   <- seq(sample_start, sample_end, by = "day") %>%
  .[!weekdays(.) %in% c("Saturday", "Sunday")]

dates_in_range <- dates[dates >= sample_start & dates <= sample_end]
n_real         <- length(dates_in_range)

exclude_idx <- map(dates_in_range, function(d) {
  idx <- which(trading_days == d)
  if (length(idx) == 0) return(integer(0))
  seq(max(1L, idx - exclusion_window), min(length(trading_days), idx + exclusion_window))
}) %>% unlist() %>% unique()

eligible_days <- trading_days[-exclude_idx]
ois_range     <- range(gap_df$daily)
eligible_days <- eligible_days[eligible_days >= ois_range[1] &
                               eligible_days <= ois_range[2]]

cat("GovC meetings in sample:    ", n_real, "\n")
cat("Eligible placebo days:      ", length(eligible_days), "\n")

# ==============================================================================
# Part 2: Precompute placebo MPU for every eligible day (vectorised)
# ==============================================================================

all_dates <- tibble(daily = eligible_days)

placebo_lookup <- gap_df %>%
  group_by(tenor) %>%
  group_modify(function(df_t, key) {
    all_dates %>%
      mutate(
        pre_mean  = rowMeans(cbind(df_t$gap[match(daily - 1, df_t$daily)],
                                  df_t$gap[match(daily - 2, df_t$daily)],
                                  df_t$gap[match(daily - 3, df_t$daily)]),
                             na.rm = TRUE),
        post_mean = rowMeans(cbind(df_t$gap[match(daily + 1, df_t$daily)],
                                  df_t$gap[match(daily + 2, df_t$daily)],
                                  df_t$gap[match(daily + 3, df_t$daily)]),
                             na.rm = TRUE),
        mpu_val   = (post_mean - pre_mean) * 100
      ) %>%
      filter(is.finite(mpu_val)) %>%
      select(daily, mpu_val)
  }) %>%
  ungroup()

# Pooled version: average across tenors per date
placebo_lookup_pooled <- placebo_lookup %>%
  group_by(daily) %>%
  summarise(mpu_pooled = mean(mpu_val, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(mpu_pooled))

# Real MPU statistics: per tenor and pooled
real_means <- differences_df %>%
  group_by(tenor) %>%
  summarise(real_mean = mean(diff_3, na.rm = TRUE), .groups = "drop") %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

real_mpu_pooled <- differences_df %>%
  group_by(date) %>%
  summarise(mpu_pooled = mean(diff_3, na.rm = TRUE), .groups = "drop")

real_mean_pooled   <- mean(real_mpu_pooled$mpu_pooled)
real_pctpos_pooled <- mean(real_mpu_pooled$mpu_pooled > 0) * 100

cat(sprintf("\nReal pooled MPU — mean: %.3f bps | %% positive: %.1f%%\n",
            real_mean_pooled, real_pctpos_pooled))

# ==============================================================================
# Part 3: 1000 draws — compute per-tenor mean, pooled mean, and pooled % positive
# ==============================================================================

cat("Running 1000 placebo draws...\n")

set.seed(42)
n_draws <- 1000

placebo_draws <- map(seq_len(n_draws), function(i) {
  sampled <- sample(eligible_days, n_real, replace = FALSE)

  per_tenor <- placebo_lookup %>%
    filter(daily %in% sampled) %>%
    group_by(tenor) %>%
    summarise(mean_mpu = mean(mpu_val, na.rm = TRUE), .groups = "drop") %>%
    mutate(draw = i)

  pooled_vals <- placebo_lookup_pooled %>%
    filter(daily %in% sampled) %>%
    pull(mpu_pooled)

  pooled <- tibble(
    draw     = i,
    mean_mpu = mean(pooled_vals, na.rm = TRUE),
    pct_pos  = mean(pooled_vals > 0, na.rm = TRUE) * 100
  )

  list(per_tenor = per_tenor, pooled = pooled)
})

draws_per_tenor <- map_dfr(placebo_draws, "per_tenor")
draws_pooled    <- map_dfr(placebo_draws, "pooled")

# P-values
p_values_tenor <- draws_per_tenor %>%
  left_join(real_means, by = "tenor") %>%
  group_by(tenor) %>%
  summarise(p_sim = mean(mean_mpu >= real_mean), .groups = "drop")

p_mean_pooled   <- mean(draws_pooled$mean_mpu   >= real_mean_pooled)
p_pctpos_pooled <- mean(draws_pooled$pct_pos    >= real_pctpos_pooled)

cat("\n=== Simulated p-values by tenor ===\n")
print(p_values_tenor)
cat(sprintf("\nPooled — mean p-value: %.4f | %% positive p-value: %.4f\n",
            p_mean_pooled, p_pctpos_pooled))

# ==============================================================================
# Figure 1: Faceted histogram by maturity
# ==============================================================================

draws_per_tenor %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))) %>%
  left_join(real_means,       by = "tenor") %>%
  left_join(p_values_tenor,   by = "tenor") %>%
  mutate(label = paste0("p = ", round(p_sim, 3))) %>%
  ggplot(aes(mean_mpu)) +
  geom_histogram(bins = 50, fill = "#4575b4", alpha = 0.7,
                 color = "white", linewidth = 0.2) +
  geom_vline(aes(xintercept = real_mean), color = "#d73027", linewidth = 1.1) +
  geom_text(aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.5, size = 4.5,
            color = "#d73027", family = "Segoe UI Light",
            inherit.aes = FALSE) +
  facet_wrap(~ tenor, scales = "free", nrow = 2) +
  labs(x       = "Placebo Mean MPU (Bps)", y = "Count",
       caption = sprintf("1000 draws of %d non-announcement days (±%d trading days excluded). Red line = real MPU mean.",
                         n_real, exclusion_window)) +
  theme_bw() +
  theme(text         = element_text(family = "Segoe UI Light"),
        axis.text    = element_text(size = 14),
        axis.title   = element_text(size = 16),
        strip.text   = element_text(size = 16, face = "bold"),
        plot.caption = element_text(size = 11, hjust = 0))

ggsave("../output/figures/placebo_1000_draws_by_tenor.pdf",
       dpi = 320, width = 14, height = 9, bg = "white")

# ==============================================================================
# Figure 2: Pooled MPU — mean (left) and % positive (right)
# ==============================================================================

theme_placebo <- theme_bw() +
  theme(text       = element_text(family = "Segoe UI Light"),
        axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"))

p1 <- ggplot(draws_pooled, aes(mean_mpu)) +
  geom_histogram(bins = 50, fill = "#4575b4", alpha = 0.7,
                 color = "white", linewidth = 0.2) +
  geom_vline(xintercept = real_mean_pooled, color = "#d73027", linewidth = 1.1) +
  annotate("text", x = real_mean_pooled, y = Inf,
           label = sprintf("Real = %.2f bps\np = %.4f", real_mean_pooled, p_mean_pooled),
           hjust = -0.1, vjust = 1.4, size = 4.5,
           color = "#d73027", family = "Segoe UI Light") +
  labs(title = "Mean Pooled MPU", x = "Placebo Mean (Bps)", y = "Count") +
  theme_placebo

p2 <- ggplot(draws_pooled, aes(pct_pos)) +
  geom_histogram(bins = 50, fill = "#4575b4", alpha = 0.7,
                 color = "white", linewidth = 0.2) +
  geom_vline(xintercept = real_pctpos_pooled, color = "#d73027", linewidth = 1.1) +
  annotate("text", x = real_pctpos_pooled, y = Inf,
           label = sprintf("Real = %.1f%%\np = %.4f", real_pctpos_pooled, p_pctpos_pooled),
           hjust = -0.1, vjust = 1.4, size = 4.5,
           color = "#d73027", family = "Segoe UI Light") +
  labs(title = "% Positive Pooled MPU", x = "Placebo % Positive", y = "Count") +
  theme_placebo

p1 + p2 +
  plot_annotation(
    caption = sprintf("1000 draws of %d non-announcement days (±%d trading days excluded). Red line = real value.",
                      n_real, exclusion_window),
    theme = theme(plot.caption = element_text(size = 11, hjust = 0,
                                              family = "Segoe UI Light"))
  )

ggsave("../output/figures/placebo_1000_draws_pooled.pdf",
       dpi = 320, width = 14, height = 7, bg = "white")
