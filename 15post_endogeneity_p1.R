#===============================================================================
# POST ENDOGENEITY P1 — Figures and Table from Endogeneity Test Results
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Plots the main results from run_endogeneity_p1.R. No API calls.
#
# Inputs:
#   ../intermediate_data/endogeneity_p1/parsed/endo_vs_headline_comparison.rds
#   ../intermediate_data/endogeneity_p1/parsed/endo_ensemble.rds
#   ../intermediate_data/endogeneity_p1/selected_dates.rds
#   ../intermediate_data/range_difference_df.rds
#   ../intermediate_data/p1/p1_ensemble_sd.rds
#
# Outputs (../output/figures/endogeneity_p1/):
#   fig1_endo_main.pdf    — Δρ + correlation comparison (main result)
#   fig2_endo_scatter.pdf — endo_sd vs. headline_sd by tenor
#   endo_table.xlsx       — formatted summary table
#   endo_table.tex        — LaTeX version (if knitr/kableExtra available)
#===============================================================================

cat("\n", strrep("=", 70), "\n")
cat("POST ENDOGENEITY P1\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# 1. SETUP
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork, showtext, writexl, stargazer)

if (!("Segoe UI" %in% font_families())) {
  fp <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(fp)) {
    font_add("Segoe UI", regular = fp)
  } else {
    warning("segoeui.ttf not found — using default font.")
  }
}
showtext_auto()

output_dir <- "../output/figures/endogeneity_p1"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cat("Output directory:", output_dir, "\n\n")

parsed_dir <- "../intermediate_data/endogeneity_p1/parsed"

# --- Shared aesthetics (consistent with 09plot_llm_results.R) -----------------

col_tenor  <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")
col_model  <- c("Headline"   = "#4575b4",
                "Two-stage"  = "#91bfdb")
col_verdict <- c("Refuted" = "#2ca25f", "Concern" = "#d73027")

base_theme <- theme_minimal(base_family = "Segoe UI") +
  theme(
    panel.grid.minor  = element_blank(),
    panel.border      = element_rect(colour = "grey80", fill = NA),
    axis.text         = element_text(size = 13),
    axis.title        = element_text(size = 14),
    strip.text        = element_text(face = "bold", size = 13),
    plot.margin       = margin(8, 10, 8, 8),
    plot.caption      = element_text(size = 9, color = "grey40", hjust = 0,
                                     lineheight = 1.3, margin = margin(t = 8))
  )

# ==============================================================================
# 2. LOAD DATA
# ==============================================================================

cat("Loading comparison table...\n")
comparison_tbl <- readRDS(file.path(parsed_dir, "endo_vs_headline_comparison.rds"))

cat("Loading endo ensemble...\n")
endo_ensemble <- readRDS(file.path(parsed_dir, "endo_ensemble.rds"))

cat("Loading selected dates and market data...\n")
selected_dates <- readRDS("../intermediate_data/endogeneity_p1/selected_dates.rds")
range_df       <- readRDS("../intermediate_data/range_difference_df.rds")
headline_full  <- readRDS("../intermediate_data/p1/p1_ensemble_sd.rds")

the_dates <- as.character(selected_dates$date)

# Reconstruct the merged comparison dataset (mirrors run_endogeneity_p1.R §14)
headline_subset <- headline_full %>%
  mutate(date = as.character(date)) %>%
  filter(date %in% the_dates) %>%
  rename(sd_mean_head = sd_mean) %>%
  select(date, tenor, sd_mean_head)

market_vol <- range_df %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor),
         date  = as.character(date)) %>%
  filter(tenor %in% c("3M", "2Y", "10Y")) %>%
  select(date, tenor, vol_1d = correct_post_mean_1)

merged <- endo_ensemble %>%
  mutate(date = as.character(date)) %>%
  select(date, tenor, sd_mean_endo = sd_mean) %>%
  inner_join(headline_subset, by = c("date", "tenor")) %>%
  inner_join(market_vol,      by = c("date", "tenor")) %>%
  filter(!is.na(sd_mean_endo), !is.na(sd_mean_head), !is.na(vol_1d)) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

cat(paste0(
  "Merged: ", nrow(merged), " rows across ",
  n_distinct(merged$date), " dates and ",
  n_distinct(merged$tenor), " tenors.\n\n"
))

# ==============================================================================
# 3. TABLE 1 — Main result: Spearman correlations and Δρ
# ==============================================================================

cat("Building Table 1 (main correlation results)...\n")

# t-approximation p-value for H0: rho = 0 (valid for Spearman with n >= 10)
pval_spearman <- function(rho, n) {
  t_stat <- rho * sqrt(n - 2) / sqrt(1 - rho^2)
  2 * pt(abs(t_stat), df = n - 2, lower.tail = FALSE)
}

sig_stars <- function(p) {
  dplyr::case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
}

tbl1_data <- comparison_tbl %>%
  mutate(
    p_endo    = pval_spearman(spearman_endo_vs_vol,    n_conferences),
    p_head    = pval_spearman(spearman_headline_vs_vol, n_conferences),
    delta_sig = ifelse(ci_low_delta > 0, "†", ""),
    verdict   = dplyr::case_when(
      ci_low_delta <= 0 & 0 <= ci_high_delta ~ "Refuted",
      ci_low_delta > 0                        ~ "Concern",
      TRUE                                    ~ "Refuted+"
    )
  )

tbl1_out <- tbl1_data %>%
  transmute(
    Tenor                   = tenor,
    N                       = n_conferences,
    `rho_ts [95% CI]`       = sprintf("%.3f%s [%.3f, %.3f]",
                                       spearman_endo_vs_vol, sig_stars(p_endo),
                                       ci_low_endo, ci_high_endo),
    `rho_hl [95% CI]`       = sprintf("%.3f%s [%.3f, %.3f]",
                                       spearman_headline_vs_vol, sig_stars(p_head),
                                       ci_low_head, ci_high_head),
    `Delta_rho [95% CI]`    = sprintf("%.3f%s [%.3f, %.3f]",
                                       delta_rho, delta_sig,
                                       ci_low_delta, ci_high_delta),
    Verdict                 = verdict
  )

cat("\n")
print(tbl1_out)
cat("\n")

writexl::write_xlsx(tbl1_out, file.path(output_dir, "tbl1_correlation_comparison.xlsx"))
cat("  Saved: tbl1_correlation_comparison.xlsx\n")

tbl1_rows <- tbl1_data %>%
  transmute(
    tenor   = tenor,
    rho_ts  = sprintf("%.3f%s", spearman_endo_vs_vol,     sig_stars(p_endo)),
    rho_hl  = sprintf("%.3f%s", spearman_headline_vs_vol,  sig_stars(p_head)),
    delta   = sprintf("%.3f%s", delta_rho,
                      ifelse(!(ci_low_delta <= 0 & 0 <= ci_high_delta),
                             "$^{\\dagger}$", "")),
    ci      = sprintf("[%.3f,\\;%.3f]", ci_low_delta, ci_high_delta)
  )

tex_rows <- paste0(
  tbl1_rows$tenor, " & ",
  tbl1_rows$rho_ts, " & ",
  tbl1_rows$rho_hl, " & ",
  tbl1_rows$delta, " & ",
  tbl1_rows$ci, " \\\\",
  collapse = "\n"
)

tex_out <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Endogeneity test: Spearman correlations with realised market volatility}\n",
  "\\label{tab:endo_corr}\n",
  "\\begin{tabular}{lcccc}\n",
  "\\toprule\n",
  "Tenor & $\\rho_{ts}$ & $\\rho_{hl}$ & $\\Delta\\rho$ & 95\\% CI \\\\\n",
  "\\midrule\n",
  tex_rows, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\smallskip\n",
  "\\begin{minipage}{0.92\\linewidth}\n",
  "\\footnotesize\n",
  "\\textit{Notes:} $\\rho_{ts}$: two-stage model (panel from macro regime only); ",
  "$\\rho_{hl}$: headline zero-shot ensemble. ",
  "$\\Delta\\rho = \\rho_{hl} - \\rho_{ts}$; 95\\% bootstrap CI (5{,}000 reps, percentile method). ",
  "$^{*}\\,p<0.10$, $^{**}\\,p<0.05$, $^{***}\\,p<0.01$ (H$_0$: $\\rho = 0$). ",
  "$^{\\dagger}$: 0 lies outside the 95\\% CI for $\\Delta\\rho$.\n",
  "\\end{minipage}\n",
  "\\end{table}\n"
)

writeLines(tex_out, file.path(output_dir, "tbl1_correlation_comparison.tex"))
cat("  Saved: tbl1_correlation_comparison.tex\n")

# ==============================================================================
# 4. FIGURE 2 — Scatter: endo_sd vs. headline_sd, faceted by tenor
# ==============================================================================

cat("Building Figure 2...\n")

# Spearman ρ annotations (endo_sd vs. headline_sd per tenor)
rho_labels <- comparison_tbl %>%
  transmute(
    tenor = factor(tenor, levels = c("3M", "2Y", "10Y")),
    label = sprintf("ρ = %.2f", spearman_endo_vs_headline)
  )

merged_plot <- merged %>%
  filter(!(tenor == "3M" & sd_mean_head > 0.2))

fig2 <- ggplot(merged_plot, aes(x = sd_mean_endo, y = sd_mean_head, color = tenor)) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.9,
              color = "grey30", fill = "grey85") +
  geom_point(alpha = 0.8, size = 2.8) +
  geom_text(
    data     = rho_labels,
    aes(x = Inf, y = -Inf, label = label),
    hjust = 1.1, vjust = -0.6,
    size = 4.5, color = "grey20", fontface = "bold",
    inherit.aes = FALSE
  ) +
  facet_wrap(~ tenor, scales = "free", nrow = 1) +
  scale_color_manual(values = col_tenor, guide = "none") +
  labs(
    x       = "Two-stage disagreement (SD, %)",
    y       = "Baseline disagreement (SD, %)",
    title   = "",
    caption = "") +
  base_theme +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5)
  )

ggsave(file.path(output_dir, "fig2_endo_scatter.pdf"),
       fig2, dpi = 320, width = 11, height = 4.5, bg = "white")
cat("  Saved: fig2_endo_scatter.pdf\n")

# ==============================================================================
# 5. TABLE — formatted comparison summary
# ==============================================================================

cat("Building summary table...\n")

tbl_out <- comparison_tbl %>%
  mutate(
    verdict = dplyr::case_when(
      ci_low_delta <= 0 & 0 <= ci_high_delta ~ "Refuted",
      ci_low_delta > 0                        ~ "Concern",
      TRUE                                    ~ "Refuted+"
    )
  ) %>%
  transmute(
    Tenor   = tenor,
    N       = n_conferences,
    `ρ(endo, vol) [95% CI]` = sprintf("%.3f [%.3f, %.3f]",
                                            spearman_endo_vs_vol,
                                            ci_low_endo, ci_high_endo),
    `ρ(headline, vol) [95% CI]` = sprintf("%.3f [%.3f, %.3f]",
                                                spearman_headline_vs_vol,
                                                ci_low_head, ci_high_head),
    `ρ(endo, headline) [95% CI]` = sprintf("%.3f [%.3f, %.3f]",
                                                 spearman_endo_vs_headline,
                                                 ci_low_eh, ci_high_eh),
    `Δρ [95% CI]` = sprintf("%.3f [%.3f, %.3f]",
                                      delta_rho, ci_low_delta, ci_high_delta),
    Verdict = verdict
  )

cat("\n")
print(tbl_out)
cat("\n")

writexl::write_xlsx(tbl_out, file.path(output_dir, "endo_table.xlsx"))
cat("  Saved: endo_table.xlsx\n")

# LaTeX version via stargazer
tryCatch({

  endo_sg <- comparison_tbl %>%
    mutate(verdict = ifelse(ci_low_delta <= 0 & 0 <= ci_high_delta,
                            "Refuted", "Concern")) %>%
    transmute(
      "Tenor"                            = tenor,
      "N"                                = n_conferences,
      "$\\rho_{ts}$ [95\\% CI]"          = sprintf("%.3f [%.3f, %.3f]",
                                                    spearman_endo_vs_vol,
                                                    ci_low_endo, ci_high_endo),
      "$\\rho_{hl}$ [95\\% CI]"          = sprintf("%.3f [%.3f, %.3f]",
                                                    spearman_headline_vs_vol,
                                                    ci_low_head, ci_high_head),
      "$\\rho_{ts,hl}$ [95\\% CI]"       = sprintf("%.3f [%.3f, %.3f]",
                                                    spearman_endo_vs_headline,
                                                    ci_low_eh, ci_high_eh),
      "$\\Delta\\rho$ [95\\% CI]"        = sprintf("%.3f [%.3f, %.3f]",
                                                    delta_rho,
                                                    ci_low_delta, ci_high_delta),
      "Verdict"                          = verdict
    ) %>%
    as.data.frame()

  stargazer(
    endo_sg,
    type         = "latex",
    summary      = FALSE,
    rownames     = FALSE,
    header       = FALSE,
    title        = "Endogeneity test: Spearman correlations with 95\\% bootstrap confidence intervals",
    label        = "tab:endo_full",
    notes        = c(
      "\\textit{ts}: two-stage model (panel from macro regime only, no transcript).",
      "\\textit{hl}: headline zero-shot ensemble.",
      "$\\rho_{ts,hl}$: cross-correlation between the two disagreement measures.",
      "$\\Delta\\rho = \\rho_{hl} - \\rho_{ts}$. 95\\% bootstrap CIs: 5,000 reps (percentile method).",
      "Verdict: $0 \\in \\mathrm{CI}(\\Delta\\rho) \\Rightarrow$ endogeneity concern refuted."
    ),
    notes.append = FALSE,
    out          = file.path(output_dir, "endo_table.tex")
  )
  cat("  Saved: endo_table.tex\n")

}, error = function(e) {
  cat("  stargazer failed — skipping endo_table.tex:", conditionMessage(e), "\n")
})

# ==============================================================================
# 6. CONSOLE SUMMARY
# ==============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

for (i in seq_len(nrow(comparison_tbl))) {
  row         <- comparison_tbl[i, ]
  verdict_str <- dplyr::case_when(
    row$ci_low_delta <= 0 & 0 <= row$ci_high_delta ~ "[REFUTED]",
    row$ci_low_delta > 0                            ~ "[CONCERN]",
    TRUE                                            ~ "[REFUTED+]"
  )
  cat(sprintf(
    "  Tenor %-3s  |  rho_endo = %.3f  |  rho_head = %.3f  |  Drho = %.3f [%.3f, %.3f]  %s\n",
    row$tenor,
    row$spearman_endo_vs_vol,
    row$spearman_headline_vs_vol,
    row$delta_rho, row$ci_low_delta, row$ci_high_delta,
    verdict_str
  ))
}

cat("\nOutputs written to:", output_dir, "\n\n")

# ==============================================================================
# 7. PANEL COMPOSITION — load and parse all Stage 1 panels
# ==============================================================================

cat(strrep("-", 50), "\n")
cat("Loading Stage 1 panels...\n")

panels_dir <- "../intermediate_data/endogeneity_p1/panels"

parse_panel_rows <- function(response_text) {
  lines <- unlist(strsplit(as.character(response_text), "\n"))
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]
  pat <- paste0(
    "^\\|?\\s*(T\\d{3})\\s*\\|\\s*(.+?)\\s*\\|\\s*(.+?)\\s*\\|",
    "\\s*(.+?)\\s*\\|?\\s*$"
  )
  m <- str_match(lines, pat)
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  if (nrow(m) == 0) return(tibble())
  tibble(
    agent_id             = m[, 2],
    risk_aversion        = m[, 3],
    behavioral_biases    = m[, 4],
    interpretation_style = m[, 5]
  )
}

panel_files <- list.files(panels_dir, pattern = "\\.rds$", full.names = TRUE)
cat(paste0("  Panel files found: ", length(panel_files), "\n\n"))

ra_levels <- c("High", "Medium", "Low")
is_levels <- c("Fundamentalist", "Sentiment Reader", "Quantitative",
               "Skeptic", "Narrative-Driven")
bias_levels <- c("Confirmation Bias", "Overconfidence", "Anchoring",
                 "Herding", "Loss Aversion", "Recency Bias")

panels_long <- map_dfr(panel_files, function(f) {
  stem  <- tools::file_path_sans_ext(basename(f))
  parts <- str_match(stem, "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)$")
  if (is.na(parts[1, 1])) return(NULL)
  tryCatch({
    resp <- readRDS(f)
    df   <- parse_panel_rows(resp)
    if (nrow(df) == 0) return(NULL)
    df %>% mutate(date = parts[1, 2], run = as.integer(parts[1, 3]))
  }, error = function(e) NULL)
}) %>%
  mutate(
    risk_aversion        = str_trim(risk_aversion),
    interpretation_style = str_trim(interpretation_style),
    behavioral_biases    = str_trim(behavioral_biases),
    risk_aversion = case_when(
      str_detect(risk_aversion, regex("high", ignore_case = TRUE)) ~ "High",
      str_detect(risk_aversion, regex("med",  ignore_case = TRUE)) ~ "Medium",
      str_detect(risk_aversion, regex("low",  ignore_case = TRUE)) ~ "Low",
      TRUE ~ risk_aversion
    ),
    interpretation_style = case_when(
      str_detect(interpretation_style, regex("fundament",  ignore_case = TRUE)) ~ "Fundamentalist",
      str_detect(interpretation_style, regex("sentiment",  ignore_case = TRUE)) ~ "Sentiment Reader",
      str_detect(interpretation_style, regex("quant",      ignore_case = TRUE)) ~ "Quantitative",
      str_detect(interpretation_style, regex("skeptic",    ignore_case = TRUE)) ~ "Skeptic",
      str_detect(interpretation_style, regex("narrative",  ignore_case = TRUE)) ~ "Narrative-Driven",
      TRUE ~ interpretation_style
    ),
    date = as.Date(date)
  ) %>%
  filter(
    risk_aversion        %in% ra_levels,
    interpretation_style %in% is_levels,
    grepl("^T\\d{3}$", agent_id)
  ) %>%
  left_join(
    selected_dates %>% mutate(date = as.Date(date)) %>% select(date, tercile),
    by = "date"
  )

cat(paste0(
  "  Total agent-rows parsed: ", nrow(panels_long), "\n",
  "  Conferences covered:     ", n_distinct(panels_long$date), "\n",
  "  Runs per conference:     ", n_distinct(panels_long$run), "\n\n"
))

# ==============================================================================
# 8. FIGURE 3 — Panel composition over time (prior_view + risk_profile)
# ==============================================================================

cat("Building Figure 3 (panel composition over time)...\n")

# --- Compute distributions by date -------------------------------------------

ra_by_date <- panels_long %>%
  count(date, risk_aversion) %>%
  group_by(date) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    risk_aversion = factor(risk_aversion, levels = ra_levels),
    date_fct      = factor(as.character(date))
  ) %>%
  arrange(date)

is_by_date <- panels_long %>%
  count(date, interpretation_style) %>%
  group_by(date) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    interpretation_style = factor(interpretation_style, levels = is_levels),
    date_fct             = factor(as.character(date))
  ) %>%
  arrange(date)

# Explode semicolon-separated biases before counting
biases_by_date <- panels_long %>%
  mutate(bias = str_split(behavioral_biases, ";\\s*")) %>%
  unnest(bias) %>%
  mutate(bias = str_trim(bias)) %>%
  filter(bias %in% bias_levels) %>%
  count(date, bias) %>%
  group_by(date) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    bias     = factor(bias, levels = bias_levels),
    date_fct = factor(as.character(date))
  ) %>%
  arrange(date)

# Shared x-axis
date_levels <- levels(ra_by_date$date_fct)
x_breaks    <- date_levels[seq(1, length(date_levels), by = 5)]
x_labs      <- format(as.Date(x_breaks), "%b\n%Y")

heatmap_theme <- base_theme +
  theme(
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    plot.title        = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x       = element_text(size = 9,  lineheight = 1.1),
    axis.text.y       = element_text(size = 10),
    legend.position   = "right",
    legend.key.height = unit(1.2, "cm")
  )

# --- Heatmap 1: risk_aversion -------------------------------------------------
p_ra <- ggplot(ra_by_date, aes(x = date_fct, y = risk_aversion, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.35) +
  scale_fill_distiller(palette = "Reds", direction = 1, name = NULL,
                       limits = c(0, NA),
                       breaks = c(0, max(ra_by_date$pct)),
                       labels = c("0%", paste0(round(max(ra_by_date$pct)), "%"))) +
  scale_x_discrete(breaks = x_breaks, labels = x_labs) +
  scale_y_discrete(limits = rev(ra_levels)) +
  labs(x = NULL, y = NULL, title = "Risk Aversion") +
  heatmap_theme

# --- Heatmap 2: interpretation_style ------------------------------------------
p_is <- ggplot(is_by_date, aes(x = date_fct, y = interpretation_style, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.35) +
  scale_fill_distiller(palette = "Blues", direction = 1, name = NULL,
                       limits = c(0, NA),
                       breaks = c(0, max(is_by_date$pct)),
                       labels = c("0%", paste0(round(max(is_by_date$pct)), "%"))) +
  scale_x_discrete(breaks = x_breaks, labels = x_labs) +
  scale_y_discrete(limits = rev(is_levels)) +
  labs(x = NULL, y = NULL, title = "Interpretation Style") +
  heatmap_theme

# --- Heatmap 3: behavioral_biases ---------------------------------------------
p_bias <- ggplot(biases_by_date, aes(x = date_fct, y = bias, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.35) +
  scale_fill_distiller(palette = "Greens", direction = 1, name = NULL,
                       limits = c(0, NA),
                       breaks = c(0, max(biases_by_date$pct)),
                       labels = c("0%", paste0(round(max(biases_by_date$pct)), "%"))) +
  scale_x_discrete(breaks = x_breaks, labels = x_labs) +
  scale_y_discrete(limits = rev(bias_levels)) +
  labs(x = NULL, y = NULL, title = "Behavioral Biases") +
  heatmap_theme

fig3 <- p_ra / p_is / p_bias +
  plot_annotation(
    title   = "Stage 1 Panel Composition by Conference Date",
    caption = paste0(
      "Colour intensity = share of agents with that characteristic, ",
      "pooled across all ", n_distinct(panels_long$run), " runs per conference. ",
      "Biases are counted at agent-bias level (each agent may hold 1–2 biases)."
    ),
    theme = theme(
      plot.title   = element_text(size = 13, face = "bold", hjust = 0.5,
                                  margin = margin(b = 6)),
      plot.caption = element_text(size = 9, color = "grey40", hjust = 0,
                                  lineheight = 1.3, margin = margin(t = 8))
    )
  )

ggsave(file.path(output_dir, "fig3_panel_composition.pdf"),
       fig3, dpi = 320, width = 12, height = 10, bg = "white")
cat("  Saved: fig3_panel_composition.pdf\n")

# ==============================================================================
# 9. FIGURE 4 — Cross-run stability per conference
# ==============================================================================

cat("Building Figure 4 (cross-run stability)...\n")

# Per (date, run): % High and % Low risk_aversion
run_ra <- panels_long %>%
  group_by(date, run) %>%
  summarise(
    pct_high = mean(risk_aversion == "High") * 100,
    pct_low  = mean(risk_aversion == "Low")  * 100,
    .groups  = "drop"
  )

run_mean_ra <- run_ra %>%
  group_by(date) %>%
  summarise(
    mean_high = mean(pct_high),
    sd_high   = sd(pct_high),
    .groups   = "drop"
  )

run_ra_long <- run_ra %>%
  select(date, run, High = pct_high, Low = pct_low) %>%
  pivot_longer(c(High, Low), names_to = "level", values_to = "pct") %>%
  mutate(level = factor(level, levels = c("High", "Low")))

run_ra_mean_long <- run_ra_long %>%
  group_by(date, level) %>%
  summarise(mean_pct = mean(pct), .groups = "drop")

p_dots <- ggplot(run_ra_long, aes(x = date, y = pct, color = level)) +
  geom_hline(yintercept = 33, linetype = "dotted",
             color = "grey60", linewidth = 0.4) +
  geom_point(alpha = 0.35, size = 2.2,
             position = position_jitter(width = 10, seed = 42)) +
  geom_line(data = run_ra_mean_long, aes(y = mean_pct), linewidth = 0.8) +
  facet_wrap(~ level, nrow = 1) +
  scale_color_manual(values = c("High" = "#d73027", "Low" = "#4575b4"),
                     guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 80),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    x = NULL, y = "% of agents",
    title = "High / Low risk-aversion share across 10 runs per conference"
  ) +
  base_theme +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

p_sd <- ggplot(run_mean_ra, aes(x = date, y = sd_high)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey60", linewidth = 0.4) +
  geom_col(fill = "grey60", alpha = 0.8, width = 40) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                     expand  = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL,
    y = "SD across 10 runs (pp)",
    title = "Within-conference instability (% High risk aversion)"
  ) +
  base_theme +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

fig4 <- p_dots / p_sd +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title   = "Panel stability across 10 independent draws per conference",
    caption = paste0(
      "Top: semi-transparent dots = individual runs; solid line = mean. ",
      "Dotted reference = 33% (uniform split across three levels). ",
      "Bottom: SD of % High risk aversion across 10 runs — ",
      "low values indicate the LLM assigns consistent risk-aversion levels ",
      "for the same macro regime."
    ),
    theme = theme(
      plot.title   = element_text(size = 13, face = "bold", hjust = 0.5,
                                  margin = margin(b = 6)),
      plot.caption = element_text(size = 9, color = "grey40", hjust = 0,
                                  lineheight = 1.3, margin = margin(t = 8))
    )
  )

ggsave(file.path(output_dir, "fig4_panel_stability.pdf"),
       fig4, dpi = 320, width = 11, height = 7, bg = "white")
cat("  Saved: fig4_panel_stability.pdf\n\n")

# Quick console summary of stability
cat("Cross-run stability summary (SD of % High risk aversion across 10 runs):\n")
run_mean_ra %>%
  summarise(
    mean_sd = mean(sd_high, na.rm = TRUE),
    max_sd  = max(sd_high,  na.rm = TRUE),
    p90_sd  = quantile(sd_high, 0.9, na.rm = TRUE)
  ) %>%
  print()

cat("\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
