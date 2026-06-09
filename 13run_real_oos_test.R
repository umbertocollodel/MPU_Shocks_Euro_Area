# ============================================================================
# OUT-OF-SAMPLE EXPERIMENT (CLEAN + FIXED + NO API CALLS)
# Uses precomputed R=10 ensemble from P1 pipeline
# ============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, lubridate, writexl, scales, showtext, ggrepel)

# ============================================================================
# 0. SAFETY CHECKS
# ============================================================================

ensemble_path <- "../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds"

if (!file.exists(ensemble_path)) {
  stop("Missing file: all_runs_long.rds")
}

dir.create("../output/figures/oos_jan2025", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# 1. LOAD ENSEMBLE DATA (NO API CALLS)
# ============================================================================

all_runs <- readRDS(ensemble_path)

test_runs <- all_runs %>%
  mutate(
    date = as.Date(date),
    rate = as.numeric(rate)
  ) %>%
  filter(date >= as.Date("2025-01-01"))

cat("\n=== OUT-OF-SAMPLE DATA ===\n")
cat("Rows:", nrow(test_runs), "\n")
cat("Dates:", n_distinct(test_runs$date), "\n")
cat("Runs:", n_distinct(test_runs$run), "\n")
cat("Tenors:", n_distinct(test_runs$tenor), "\n\n")

# ============================================================================
# 2. DISAGREEMENT METRICS (P1-CONSISTENT FIXED VERSION)
# ============================================================================

# Step 1: within-run disagreement (agent level)
within_run <- test_runs %>%
  group_by(date, run, tenor) %>%
  summarise(
    within_sd = sd(rate, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: ensemble disagreement (across runs)
test_disagreement <- within_run %>%
  group_by(date, tenor) %>%
  summarise(
    llm_std = mean(within_sd, na.rm = TRUE),
    n_runs  = n(),
    .groups = "drop"
  )

write_xlsx(
  test_disagreement,
  "../intermediate_data/oos_test_disagreement.xlsx"
)

cat("\n=== DISAGGREGATION DONE ===\n")
print(test_disagreement)

# ============================================================================
# 3. MARKET VOLATILITY DATA
# ============================================================================

market_vol <- readRDS("../intermediate_data/range_difference_df.rds") %>%
  mutate(
    tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor),
    date  = as.Date(date)
  ) %>%
  filter(date >= as.Date("2025-01-01")) %>%
  select(date, tenor, market_std = correct_post_mean_3)

# ============================================================================
# 4. MERGE DATA
# ============================================================================

comparison <- test_disagreement %>%
  inner_join(market_vol, by = c("date", "tenor"))

# ============================================================================
# 5. CORRELATIONS
# ============================================================================

correlations <- comparison %>%
  group_by(tenor) %>%
  summarise(
    pearson  = cor(llm_std, market_std, use = "complete.obs"),
    spearman = cor(llm_std, market_std, method = "spearman", use = "complete.obs"),
    n        = n(),
    .groups  = "drop"
  )

cat("\n=== CORRELATIONS ===\n")
print(correlations)

write_xlsx(
  correlations,
  "../intermediate_data/oos_correlations.xlsx"
)

# ============================================================================
# 6. PLOT: DISAGREEMENT OVER TIME
# ============================================================================

if (file.exists("segoeui.ttf")) font_add("Segoe UI", regular = "segoeui.ttf")
showtext_auto()

colors <- c("10Y" = "#d73027",
            "2Y"  = "#4575b4",
            "3M"  = "#91bfdb")

p1 <- test_disagreement %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) %>%
  ggplot(aes(x = date, y = llm_std, color = tenor)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~tenor, ncol = 1, scales = "free_y") +
  scale_color_manual(values = colors, guide = "none") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "LLM Disagreement (Out-of-Sample 2025)",
    x = NULL,
    y = "SD of Forecasts"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(colour = "grey80", fill = NA),
    panel.grid.minor = element_blank()
  )

ggsave(
  "../output/figures/oos_jan2025/disagreement_by_tenor.png",
  p1,
  width = 10,
  height = 8,
  dpi = 300
)

# ============================================================================
# CLEAN SCATTER WITH REGRESSION + NON-OVERLAPPING LABELS
# ============================================================================

comparison <- comparison %>%
  mutate(
    tenor = factor(tenor, levels = c("3M", "2Y", "10Y")),
    month_label = paste(format(date, "%b"), format(date, "%Y"))
  )

# OLS regression
ols_fit <- lm(market_std ~ llm_std, data = comparison)

reg_slope <- coef(ols_fit)[2]
reg_intercept <- coef(ols_fit)[1]

cat("\nOLS slope:", round(reg_slope, 4), "\n")
cat("OLS intercept:", round(reg_intercept, 4), "\n")


reg_r2 <- summary(ols_fit)$r.squared


reg_label <- sprintf(
  "Market Vol = %.3f + %.3f × LLM Disagreement",
  reg_intercept,
  reg_slope
)



p_scatter <- ggplot(comparison, aes(x = llm_std, y = market_std)) +

  # regression line
  geom_abline(
    intercept = reg_intercept,
    slope = reg_slope,
    linewidth = 1
  ) +

  # points
  geom_point(aes(color = tenor), size = 3, alpha = 0.85) +

  # NON-OVERLAPPING LABELS (key fix)
  ggrepel::geom_text_repel(
    aes(label = month_label),
    size = 4,
    family = "Segoe UI",
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey60"
  ) +
annotate(
  "text",
  x = -Inf, y = Inf,               # TOP‑LEFT corner
  label = reg_label,
  hjust = -0.05, vjust = 1.1,      # pull text inside panel
  size = 4,
  family = "Segoe UI"
) +
  scale_color_manual(values = c(
    "10Y" = "#d73027",
    "2Y"  = "#4575b4",
    "3M"  = "#91bfdb"
  )) +

  labs(
    x = "LLM Disagreement (pp)",
    y = "Realized Market Volatility (pp)",
    col=""
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "right",   # FIX: remove overlap
    panel.border = element_rect(colour = "grey80", fill = NA),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold")
  )

ggsave(
  "../output/figures/oos_jan2025/pooled_scatter.pdf",
  p_scatter,
  width = 10,
  height = 8,
  dpi = 320,
  bg = "white"
)

# ============================================================================
# 8. SUMMARY STATS
# ============================================================================

summary_stats <- test_disagreement %>%
  group_by(tenor) %>%
  summarise(
    mean_std   = mean(llm_std, na.rm = TRUE),
    median_std = median(llm_std, na.rm = TRUE),
    min_std    = min(llm_std, na.rm = TRUE),
    max_std    = max(llm_std, na.rm = TRUE),
    n_obs      = n(),
    .groups    = "drop"
  )

cat("\n=== SUMMARY ===\n")
print(summary_stats)

write_xlsx(
  summary_stats,
  "../intermediate_data/oos_summary_stats.xlsx"
)

# ============================================================================
# 9. FINAL MESSAGE
# ============================================================================

cat("\n=== COMPLETE ===\n")
cat("Out-of-sample evaluation finished (NO API calls used)\n")