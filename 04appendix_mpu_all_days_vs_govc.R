# ==============================================================================
# Script 04: MPU on all days vs. GovC days — distribution comparison
# ==============================================================================
# Applies the same 3-trading-day symmetric window to every trading day.
# "Control" excludes any day within ±3 trading days of a GovC meeting
# (those windows would overlap the announcement itself).

if (!require("pacman")) install.packages("pacman")
pacman::p_load(padr, readxl, tidyverse, showtext, sysfonts, stargazer)

if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) font_add("Segoe UI", regular = font_path)
}
showtext_auto()

# ==============================================================================
# 1. Load raw OIS data (same as script 01)
# ==============================================================================

names_sheets <- excel_sheets("../raw_data/daily_OIS_updated15Sept_2025..xls")
tenors       <- names_sheets %>% str_remove("_.*")

df_list <- names_sheets %>%
  map(~ read_excel("../raw_data/daily_OIS_updated15Sept_2025..xls", sheet = .x)) %>%
  map(~ .x %>% select(1:5)) %>%
  map(~ .x %>% rename(Daily = Timestamp)) %>%
  map(~ .x %>% mutate(Daily = as.Date(Daily))) %>%
  map(~ .x %>% setNames(c("daily", "high", "low", "first", "last"))) %>%
  map(~ .x %>% mutate(across(-daily, as.numeric))) %>%
  map(~ .x %>% arrange(daily))

final_date_to_pad <- df_list %>% map(~ .x$daily[1])
df_list <- df_list %>%
  map(~ .x %>% add_row(daily = as.Date("1999-01-03")) %>% arrange(daily)) %>%
  map2(final_date_to_pad, ~ .x %>% filter(daily <= .y)) %>%
  map(~ .x %>% pad("day")) %>%
  map2(df_list, ~ rbind(.x, .y))

# ==============================================================================
# 2. Load GovC dates
# ==============================================================================

dates <- read_xlsx("../raw_data/dates_govc.xlsx") %>%
  unite("date", day:year, sep = "-", remove = TRUE) %>%
  mutate(date = as.Date(date, "%d-%m-%Y")) %>%
  .$date

# ==============================================================================
# 3. Compute MPU for every trading day
# ==============================================================================
# Filter to trading days first so lag/lead index consecutive trading days,
# not calendar days (which would introduce NAs for weekends/holidays).
#
# MPU(t) = [ mean(gap[t+1, t+2, t+3]) - mean(gap[t-1, t-2, t-3]) ] * 100
#
# Grouping:
#   GovC days  — the 199/120 announcement days
#   Other days — all trading days NOT within ±3 trading days of any GovC meeting
#   (transition days within the ±3 buffer are dropped entirely)

all_days_mpu <- df_list %>%
  map(~ {
    .x %>%
      mutate(gap = high - low) %>%
      filter(!is.na(gap)) %>%                    # trading days only
      mutate(
        govc      = as.integer(daily %in% dates),
        pre_mean  = (lag(gap, 1) + lag(gap, 2) + lag(gap, 3)) / 3,
        post_mean = (lead(gap, 1) + lead(gap, 2) + lead(gap, 3)) / 3,
        mpu       = (post_mean - pre_mean) * 100,
        # TRUE if this day is within ±3 trading days of any GovC meeting
        near_govc = Reduce(`|`, map(1:3, ~ lag(govc,  .x) == 1)) |
                    Reduce(`|`, map(1:3, ~ lead(govc, .x) == 1))
      ) %>%
      filter(!is.na(mpu)) %>%
      select(daily, govc, near_govc, mpu)
  }) %>%
  set_names(tenors) %>%
  bind_rows(.id = "tenor") %>%
  mutate(
    group = case_when(
      govc == 1          ~ "GovC",
      near_govc == FALSE ~ "Control",
      TRUE               ~ NA_character_    # ±3 buffer days — excluded
    ),
    tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))
  ) %>%
  filter(!is.na(group))

# ==============================================================================
# 4. Density plot
# ==============================================================================

# Clip display x-axis per tenor to 2nd–98th percentile (cosmetic — long tails
# otherwise collapse the bulk of the distribution into a flat line)
display_limits <- all_days_mpu %>%
  group_by(tenor) %>%
  summarise(xlo = quantile(mpu, 0.02), xhi = quantile(mpu, 0.98), .groups = "drop")

plot_data <- all_days_mpu %>%
  left_join(display_limits, by = "tenor") %>%
  filter(mpu >= xlo, mpu <= xhi)

group_means <- plot_data %>%
  group_by(tenor, group) %>%
  summarise(mean_val = mean(mpu), .groups = "drop")

ggplot(plot_data, aes(x = mpu, colour = group)) +
  # Control first so GovC line sits on top visually
  geom_density(
    data = ~ filter(.x, group == "Control"),
    fill = NA, linewidth = 0.6, bw = "SJ"
  ) +
  geom_density(
    data = ~ filter(.x, group == "GovC"),
    fill = NA, linewidth = 0.9, bw = "SJ"
  ) +
  # Dashed vertical lines at group means
  geom_vline(
    data = group_means,
    aes(xintercept = mean_val, colour = group),
    linetype = "dashed", linewidth = 0.7
  ) +
  geom_vline(xintercept = 0, linetype = "dotted", colour = "grey50", linewidth = 0.5) +
  facet_wrap(~ tenor, scales = "free", ncol = 2) +
  scale_colour_manual(values = c("GovC" = "#d73027", "Control" = "#4575b4")) +
  labs(x = "MPU (bps)", y = "Density", colour = "") +
  theme_bw() +
  theme(
    text            = element_text(family = "Segoe UI Light"),
    axis.text       = element_text(size = 18),
    axis.title      = element_text(size = 20),
    strip.text      = element_text(size = 18, face = "bold"),
    legend.text     = element_text(size = 16),
    legend.position = "bottom",
    panel.spacing   = unit(1.2, "lines")
  )

ggsave("../output/figures/mpu_all_days_density.pdf",
       dpi = 320, width = 14, height = 10, bg = "white")

# ==============================================================================
# 5. Formal tests: Wilcoxon rank-sum + Kolmogorov-Smirnov
# ==============================================================================
# H0: distributions of MPU on GovC days and other days are identical.
# Wilcoxon detects location shifts; KS detects any difference in shape.

test_results <- all_days_mpu %>%
  split(.$tenor) %>%
  map_dfr(~ {
    govc_vals  <- .x %>% filter(group == "GovC")  %>% pull(mpu)
    other_vals <- .x %>% filter(group == "Control") %>% pull(mpu)

    wt <- wilcox.test(govc_vals, other_vals, exact = FALSE)
    ks <- ks.test(govc_vals, other_vals)

    tibble(
      tenor      = unique(.x$tenor),
      n_govc     = length(govc_vals),
      n_other    = length(other_vals),
      mean_govc  = round(mean(govc_vals),  3),
      mean_other = round(mean(other_vals), 3),
      wilcox_W   = round(wt$statistic,     0),
      wilcox_p   = round(wt$p.value,       3),
      ks_D       = round(ks$statistic,     3),
      ks_p       = round(ks$p.value,       3)
    )
  }) %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))) %>%
  arrange(tenor)

cat("\n=== Distribution Tests: GovC days vs. other days (±3 trading-day buffer excluded) ===\n\n")
print(test_results, n = Inf)

# ==============================================================================
# 6. Export LaTeX table
# ==============================================================================

dir.create("../output/tables", showWarnings = FALSE)

test_results %>%
  select(
    Tenor  = tenor,
    `W`    = wilcox_W,  `p (W)`  = wilcox_p,
    `D`    = ks_D,      `p (KS)` = ks_p
  ) %>%
  mutate(Tenor = as.character(Tenor)) %>%
  stargazer(
    summary      = FALSE,
    rownames     = FALSE,
    out          = "../output/tables/mpu_distribution_tests.tex",
    title        = "Distribution tests: MPU on GovC days vs.\\ all other days",
    label        = "tab:mpu_dist_tests",
  )

cat("\nDone.\n")
cat("Figure: ../output/figures/mpu_all_days_density.pdf\n")
cat("Table:  ../output/tables/mpu_distribution_tests.tex\n")
