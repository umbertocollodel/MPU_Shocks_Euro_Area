# ==============================================================================
# 03.4appendix_mpu_monetary_policy_regimes.R
# Purpose: Compare MPU across monetary policy regimes.
#   Outputs:
#     - Figure: density of MPU by regime, faceted by tenor
#     - Table:  mean, SD, and % positive meetings per regime × tenor
# ==============================================================================
# Regimes:
#   Pre-FG  : before July 2013
#   FG      : July 2013 – December 2022 (forward guidance / APP / NIRP era)
#   Post-FG : January 2023 onwards (rate normalisation)
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, showtext, sysfonts, stargazer)

if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) font_add("Segoe UI", regular = font_path)
}
showtext_auto()

if (!exists("differences_df")) {
  differences_df <- readRDS("../intermediate_data/range_difference_df.rds")
}

dir.create("../output/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("../output/figures", showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1. Assign regimes
# ==============================================================================

regime_df <- differences_df %>%
  mutate(
    tenor  = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")),
    regime = case_when(
      date <  as.Date("2013-07-01") ~ "Pre-FG",
      date <= as.Date("2022-12-31") ~ "FG",
      TRUE                          ~ "Post-FG"
    ),
    regime = factor(regime, levels = c("Pre-FG", "FG", "Post-FG"))
  )

# ==============================================================================
# 2. Summary table: mean, SD, % positive by regime × tenor
# ==============================================================================

sumstats <- regime_df %>%
  group_by(tenor, regime) %>%
  summarise(
    N       = n(),
    Mean    = round(mean(diff_3,        na.rm = TRUE), 2),
    SD      = round(sd(diff_3,          na.rm = TRUE), 2),
    PctPos  = round(mean(diff_3 > 0,    na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# Wide format: rows = tenor, column groups = regime
sumstats_wide <- sumstats %>%
  pivot_wider(
    names_from  = regime,
    values_from = c(N, Mean, SD, PctPos),
    names_glue  = "{regime}_{.value}"
  ) %>%
  select(tenor,
         PreFG_N    = `Pre-FG_N`,    PreFG_Mean  = `Pre-FG_Mean`,
         PreFG_SD   = `Pre-FG_SD`,   PreFG_Pct   = `Pre-FG_PctPos`,
         FG_N       = FG_N,          FG_Mean     = FG_Mean,
         FG_SD      = FG_SD,         FG_Pct      = FG_PctPos,
         PostFG_N   = `Post-FG_N`,   PostFG_Mean = `Post-FG_Mean`,
         PostFG_SD  = `Post-FG_SD`,  PostFG_Pct  = `Post-FG_PctPos`) %>%
  mutate(tenor = as.character(tenor))

cat("\n=== Summary statistics by regime ===\n")
print(sumstats_wide)

sumstats_wide %>%
  setNames(c("Tenor",
             "N", "Mean", "SD", "Pct>0",
             "N", "Mean", "SD", "Pct>0",
             "N", "Mean", "SD", "Pct>0")) %>%
  stargazer(
    summary  = FALSE,
    rownames = FALSE,
    out      = "../output/tables/regime_summary_stats.tex",
    title    = "MPU surprise by monetary policy regime",
    label    = "tab:regime_sumstats",
    notes    = paste0(
      "Pre-FG: before July 2013. ",
      "FG: July 2013 -- December 2022 (forward guidance, APP, NIRP). ",
      "Post-FG: January 2023 onwards. ",
      "Mean and SD in basis points. Pct>0 is the share of meetings with a positive MPU surprise."
    )
  )

# ==============================================================================
# 3. Figure: density by regime, faceted by tenor
# ==============================================================================

regime_colours <- c(
  "Pre-FG"  = "#4575b4",
  "FG"      = "#d73027",
  "Post-FG" = "#1a9641"
)

# Clip tails for display
display_limits <- regime_df %>%
  group_by(tenor) %>%
  summarise(xlo = quantile(diff_3, 0.02), xhi = quantile(diff_3, 0.98),
            .groups = "drop")

plot_dens <- regime_df %>%
  left_join(display_limits, by = "tenor") %>%
  filter(diff_3 >= xlo, diff_3 <= xhi)

ggplot(plot_dens, aes(x = diff_3, colour = regime)) +
  geom_density(linewidth = 0.9, bw = "SJ") +
  geom_vline(xintercept = 0, linetype = "dotted",
             colour = "grey50", linewidth = 0.5) +
  facet_wrap(~ tenor, scales = "free", nrow = 2) +
  scale_colour_manual(
    values = regime_colours,
    labels = c("Pre-FG (2005–2013)", "FG (2013–2022)", "Post-FG (2023–2025)")
  ) +
  labs(x = "MPU Surprise (Bps)", y = "Density", colour = "") +
  theme_bw() +
  theme(
    text            = element_text(family = "Segoe UI Light"),
    axis.text       = element_text(size = 14),
    axis.title      = element_text(size = 16),
    strip.text      = element_text(size = 16, face = "bold"),
    legend.text     = element_text(size = 13),
    legend.position = "bottom",
    panel.spacing   = unit(1.2, "lines")
  )

ggsave("../output/figures/mpu_regime_density.pdf",
       dpi = 320, width = 14, height = 9, bg = "white")

cat("\nDone.\n")
cat("  Table:  ../output/tables/regime_summary_stats.tex\n")
cat("  Figure: ../output/figures/mpu_regime_density.pdf\n")
