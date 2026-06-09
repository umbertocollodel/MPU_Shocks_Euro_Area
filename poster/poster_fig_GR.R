#===============================================================================
# POSTER FIGURE — Spearman-Brown Reliability Curve G(R)
#===============================================================================
# Purpose:
#   Poster-quality version of p1_fig_GR_full from post_zero_shot_p1.R.
#   Shows how ensemble reliability G(R) grows with the number of runs R,
#   across 3M / 2Y / 10Y tenors.
#
# Inputs (READ ONLY):
#   ../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds
#
# Outputs:
#   ../output/figures/poster/poster_fig_GR.pdf
#   ../output/figures/poster/poster_fig_GR.png
#
# Usage: source("26poster_fig_GR.R")  from code/ directory
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

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================

cat("Loading ensemble data...\n")
all_runs_path <- "../intermediate_data/full_ensemble_p1/parsed/all_runs_long.rds"
if (!file.exists(all_runs_path)) {
  stop(glue(
    "all_runs_long.rds not found at {all_runs_path}.\n",
    "Run post_zero_shot_p1.R first."
  ))
}

all_runs <- readRDS(all_runs_path) %>%
  mutate(
    date  = as.Date(date),
    tenor = if_else(tenor == "3mnt", "3M", tenor),
    tenor = factor(tenor, levels = tenor_levels)
  ) %>%
  filter(tenor %in% tenor_levels)

# ==============================================================================
# 2. COMPUTE G(R)
# ==============================================================================

within_run <- all_runs %>%
  group_by(date, run, tenor) %>%
  summarise(within_sd = sd(rate, na.rm = TRUE), .groups = "drop")

date_tenor_stats <- within_run %>%
  group_by(date, tenor) %>%
  summarise(
    mean_wr_sd = mean(within_sd, na.rm = TRUE),
    var_wr_sd  = var(within_sd,  na.rm = TRUE),
    .groups    = "drop"
  )

reliability_params <- date_tenor_stats %>%
  group_by(tenor) %>%
  summarise(
    sigma_between = sd(mean_wr_sd,          na.rm = TRUE),
    sigma_within  = sqrt(mean(var_wr_sd,    na.rm = TRUE)),
    .groups       = "drop"
  )

gr_df <- crossing(reliability_params, R = 1:20) %>%
  mutate(
    G_R   = sigma_between^2 / (sigma_between^2 + sigma_within^2 / R),
    tenor = factor(tenor, levels = tenor_levels)
  )

# End-of-line label positions (R = 20) — nudge 2Y/3M apart so they don't overlap
label_df <- gr_df %>%
  filter(R == 20) %>%
  select(tenor, G_R) %>%
  mutate(y_label = case_when(
    tenor == "2Y" ~ G_R + 0.035,
    tenor == "3M" ~ G_R - 0.035,
    TRUE          ~ G_R
  ))

cat("G(R) at R=10 by tenor:\n")
gr_df %>% filter(R == 10) %>% select(tenor, G_R) %>% print()

# ==============================================================================
# 3. BUILD FIGURE
# ==============================================================================

cat("Building poster G(R) figure...\n")

poster_theme <- theme_minimal(base_family = BASE_FAMILY, base_size = 16) +
  theme(
    plot.title       = element_blank(),
    axis.title       = element_text(size = 32, face = "bold"),
    axis.text        = element_text(size = 28),
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "grey80", fill = NA),
    plot.margin      = margin(8, 40, 8, 8)  # extra right margin for end labels
  )

fig <- ggplot(gr_df, aes(x = R, y = G_R, colour = tenor)) +
  # Reference lines
  geom_hline(yintercept = 0.60, linetype = "dashed", colour = "grey60",
             linewidth = 0.6) +
  # R = 10 vertical marker
  geom_vline(xintercept = 10, linetype = "dotdash", colour = "grey50",
             linewidth = 0.7) +
  # G(R) curves
  geom_line(linewidth = 2.0) +
  # R = 10 annotation
  annotate(
    "text", x = 10.4, y = 0.06,
    label = "R = 10", size = 6.5, fontface = "bold",
    colour = "grey40", hjust = 0
  ) +
  # End-of-line tenor labels (y_label has manual nudge for 2Y/3M separation)
  geom_text(
    data = label_df,
    aes(x = 20.4, y = y_label, label = tenor, colour = tenor),
    hjust = 0, size = 7, fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_colour_manual(values = tenor_colours, guide = "none") +
  scale_x_continuous(
    breaks = c(1, 5, 10, 15, 20),
    limits = c(1, 23),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    x = "Number of runs averaged (R)",
    y = "Signal-to-noise ratio"
  ) +
  poster_theme

# ==============================================================================
# 4. SAVE
# ==============================================================================

stem <- file.path(fig_dir, "poster_fig_GR")
for (ext in c("pdf", "png")) {
  ggsave(
    filename = paste0(stem, ".", ext),
    plot     = fig,
    dpi      = 320,
    width    = 10,
    height   = 7,
    bg       = "white"
  )
}
cat(glue("Saved: {stem}.pdf / .png\n"))
