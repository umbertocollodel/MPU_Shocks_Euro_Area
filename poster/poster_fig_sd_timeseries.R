#===============================================================================
# POSTER FIGURE — Synthetic Disagreement vs. Observed Market Volatility
#===============================================================================
# Purpose:
#   Creates the hero figure for the CEBRA poster: ensemble synthetic SD
#   (R = 10) overlaid with actual post-announcement OIS volatility, across
#   3M / 2Y / 10Y tenors. Both series are z-score normalised within each
#   tenor so they share a common axis.
#
#   Derived from post_zero_shot_p1.R sec.5 (Figure 4) — zoom panel removed,
#   observed volatility line added, Spearman rho annotated per facet,
#   series identified by in-plot labels (no legend).
#
# Inputs (READ ONLY):
#   ../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds
#   ../intermediate_data/range_difference_df.rds
#
# Outputs:
#   ../output/figures/poster/poster_fig_sd_timeseries.pdf
#   ../output/figures/poster/poster_fig_sd_timeseries.png
#
# Usage: source("25poster_fig_sd_timeseries.R")  from code/ directory
#===============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales, showtext, glue)

# -- Fonts ---------------------------------------------------------------------
if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) font_add("Segoe UI", regular = font_path)
}
showtext_auto()
showtext_opts(dpi = 320)
BASE_FAMILY <- if ("Segoe UI" %in% font_families()) "Segoe UI" else ""

# -- Directories ---------------------------------------------------------------
fig_dir <- "../output/figures/poster"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# -- Palette & constants -------------------------------------------------------
tenor_levels  <- c("3M", "2Y", "10Y")
tenor_colours <- c("3M" = "#4292c6", "2Y" = "#4575b4", "10Y" = "#d73027")
OBS_COLOUR    <- "grey25"

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================

cat("Loading ensemble data...\n")
all_runs_path <- "../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds"
if (!file.exists(all_runs_path)) {
  stop(glue(
    "all_runs_long.rds not found at {all_runs_path}.\n",
    "Run post_zero_shot_p1.R (or run_full_ensemble_p1.R) first."
  ))
}

all_runs <- readRDS(all_runs_path) %>%
  mutate(
    date  = as.Date(date),
    tenor = if_else(tenor == "3mnt", "3M", tenor),
    tenor = factor(tenor, levels = tenor_levels)
  ) %>%
  filter(tenor %in% tenor_levels)

market_vol <- readRDS("../intermediate_data/range_difference_df.rds") %>%
  mutate(
    tenor = if_else(tenor == "3mnt", "3M", tenor),
    tenor = factor(tenor, levels = tenor_levels),
    date  = as.Date(date)
  ) %>%
  select(tenor, date, vol_1d = correct_post_mean_1) %>%
  filter(!is.na(vol_1d), tenor %in% tenor_levels)

cat(glue(
  "all_runs:   {nrow(all_runs)} rows | {n_distinct(all_runs$date)} dates\n",
  "market_vol: {nrow(market_vol)} rows | {n_distinct(market_vol$date)} dates\n\n"
))

# ==============================================================================
# 2. COMPUTE ENSEMBLE DISAGREEMENT
# ==============================================================================

within_run <- all_runs %>%
  group_by(date, run, tenor) %>%
  summarise(within_sd = sd(rate, na.rm = TRUE), .groups = "drop")

ensemble <- within_run %>%
  group_by(date, tenor) %>%
  summarise(
    sd_mean = mean(within_sd, na.rm = TRUE),
    sd_se   = sd(within_sd,   na.rm = TRUE) / sqrt(n()),
    n_runs  = n(),
    .groups = "drop"
  )

# ==============================================================================
# 3. SPEARMAN CORRELATIONS (computed on full sample, before any plot filters)
# ==============================================================================

corr_labels <- ensemble %>%
  filter(!(date == as.Date("2005-01-20") & tenor == "10Y")) %>%
  inner_join(select(market_vol, date, tenor, vol_1d), by = c("date", "tenor")) %>%
  group_by(tenor) %>%
  summarise(
    rho = cor(sd_mean, vol_1d, method = "spearman", use = "complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf("rho == %.2f", rho),
    tenor = factor(tenor, levels = tenor_levels)
  )

cat("Spearman correlations:\n")
print(corr_labels)

# ==============================================================================
# 4. NORMALISE BOTH SERIES (z-score within tenor)
# ==============================================================================

synth_z <- ensemble %>%
  filter(!(date == as.Date("2005-01-20") & tenor == "10Y")) %>%
  group_by(tenor) %>%
  mutate(
    z_mean    = (sd_mean - mean(sd_mean, na.rm = TRUE)) / sd(sd_mean, na.rm = TRUE),
    z_se      = sd_se / sd(sd_mean, na.rm = TRUE),
    ribbon_lo = z_mean - 1.96 * z_se,
    ribbon_hi = z_mean + 1.96 * z_se,
    p95       = quantile(sd_mean, 0.95, na.rm = TRUE),
    highlight = sd_mean > p95
  ) %>%
  ungroup() %>%
  filter(!(tenor == "3M" & ribbon_hi > 10))  # drop 3M date where one run is extreme (plot only)

obs_z <- market_vol %>%
  group_by(tenor) %>%
  mutate(z_obs = (vol_1d - mean(vol_1d, na.rm = TRUE)) / sd(vol_1d, na.rm = TRUE)) %>%
  ungroup()

# ==============================================================================
# 5. BUILD FIGURE
# ==============================================================================

cat("Building poster figure...\n")

tenor_labels <- c("3M" = "3-Month OIS", "2Y" = "2-Year OIS", "10Y" = "10-Year OIS")

poster_theme <- theme_minimal(base_family = BASE_FAMILY, base_size = 16) +
  theme(
    plot.title        = element_blank(),
    strip.text        = element_text(size = 26, face = "bold"),
    axis.title        = element_text(size = 20, face = "bold"),
    axis.text         = element_text(size = 18),
    legend.position   = "none",
    panel.grid.minor  = element_blank(),
    panel.border      = element_rect(colour = "grey80", fill = NA),
    plot.margin       = margin(8, 12, 8, 8)
  )

# One-row-per-tenor frames used only for in-plot text labels
label_df <- tibble(tenor = factor(tenor_levels, levels = tenor_levels))

fig <- ggplot() +
  # Uncertainty ribbon
  geom_ribbon(
    data = synth_z,
    aes(x = date, ymin = ribbon_lo, ymax = ribbon_hi, fill = tenor, group = tenor),
    colour = NA, alpha = 0.25
  ) +
  # Zero reference
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  # Observed market volatility
  geom_line(
    data = obs_z,
    aes(x = date, y = z_obs, group = tenor),
    colour = OBS_COLOUR, linewidth = 0.6, alpha = 0.75
  ) +
  # Synthetic disagreement
  geom_line(
    data = synth_z,
    aes(x = date, y = z_mean, colour = tenor, group = tenor),
    linewidth = 1.3
  ) +
  # Highlight top-5% events
  geom_point(
    data = filter(synth_z, highlight),
    aes(x = date, y = z_mean, colour = tenor),
    shape = 21, size = 2.5, stroke = 1, fill = "white",
    show.legend = FALSE
  ) +
  # Spearman rho — top-right corner of each facet
  geom_text(
    data = corr_labels,
    aes(label = label),
    x = Inf, y = Inf,
    hjust = 1.1, vjust = 1.6,
    size = 9, fontface = "bold", colour = "grey15",
    parse = TRUE,
    inherit.aes = FALSE
  ) +
  # In-plot label: "Synthetic disagreement" in tenor colour — top-left, line 1
  geom_text(
    data = label_df,
    aes(colour = tenor, label = "Synthetic disagreement"),
    x = -Inf, y = Inf,
    hjust = -0.07, vjust = 1.8,
    size = 6.5, fontface = "bold",
    inherit.aes = FALSE
  ) +
  # In-plot label: "Observed volatility" in grey — top-left, line 2
  geom_text(
    data = label_df,
    aes(label = "Observed volatility"),
    colour = OBS_COLOUR,
    x = -Inf, y = Inf,
    hjust = -0.07, vjust = 3.5,
    size = 6.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(
    ~ tenor, ncol = 1, scales = "free_y",
    labeller = labeller(tenor = tenor_labels)
  ) +
  scale_colour_manual(values = tenor_colours, guide = "none") +
  scale_fill_manual(values = tenor_colours, guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = 0.01)) +
  labs(x = NULL, y = "Standardised units (z-score)") +
  poster_theme

# ==============================================================================
# 6. SAVE
# ==============================================================================

stem <- file.path(fig_dir, "poster_fig_sd_timeseries")
for (ext in c("pdf", "png")) {
  ggsave(
    filename = paste0(stem, ".", ext),
    plot     = fig,
    dpi      = 320,
    width    = 14,
    height   = 12,
    bg       = "white"
  )
}
cat(glue("Saved: {stem}.pdf / .png\n"))
