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
    delta_sig = ifelse(!(ci_low_delta <= 0 & 0 <= ci_high_delta), "†", ""),
    verdict   = ifelse(ci_low_delta <= 0 & 0 <= ci_high_delta, "Refuted", "Concern")
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

tryCatch({

  tbl1_sg <- tbl1_data %>%
    transmute(
      "Tenor"                     = tenor,
      "N"                         = n_conferences,
      "$\\rho_{ts}$ [95\\% CI]"   = sprintf("%.3f%s [%.3f, %.3f]",
                                             spearman_endo_vs_vol, sig_stars(p_endo),
                                             ci_low_endo, ci_high_endo),
      "$\\rho_{hl}$ [95\\% CI]"   = sprintf("%.3f%s [%.3f, %.3f]",
                                             spearman_headline_vs_vol, sig_stars(p_head),
                                             ci_low_head, ci_high_head),
      "$\\Delta\\rho$ [95\\% CI]" = sprintf("%.3f%s [%.3f, %.3f]",
                                             delta_rho,
                                             ifelse(!(ci_low_delta <= 0 & 0 <= ci_high_delta),
                                                    "$\\dagger$", ""),
                                             ci_low_delta, ci_high_delta),
      "Verdict"                   = verdict
    ) %>%
    as.data.frame()

  stargazer(
    tbl1_sg,
    type         = "latex",
    summary      = FALSE,
    rownames     = FALSE,
    header       = FALSE,
    title        = "Spearman correlations between synthetic disagreement and realised market volatility",
    label        = "tab:endo_corr",
    notes        = c(
      "\\textit{ts}: two-stage model (panel from macro regime only, no transcript).",
      "\\textit{hl}: headline zero-shot ensemble. $\\Delta\\rho = \\rho_{hl} - \\rho_{ts}$.",
      "Stars on $\\rho$: $t$-approx.\\ $p$-value (H$_0$: $\\rho=0$);",
      "$^{*}\\,p<0.10$, $^{**}\\,p<0.05$, $^{***}\\,p<0.01$.",
      "$\\dagger$: 0 outside the 95\\% bootstrap CI for $\\Delta\\rho$ (5,000 reps).",
      "Verdict: $0 \\in \\mathrm{CI}(\\Delta\\rho) \\Rightarrow$ endogeneity concern refuted."
    ),
    notes.append = FALSE,
    out          = file.path(output_dir, "tbl1_correlation_comparison.tex")
  )
  cat("  Saved: tbl1_correlation_comparison.tex\n")

}, error = function(e) {
  cat("  stargazer failed — skipping tbl1_correlation_comparison.tex:", conditionMessage(e), "\n")
})

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

fig2 <- ggplot(merged, aes(x = sd_mean_endo, y = sd_mean_head, color = tenor)) +
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
    y       = "Headline disagreement (SD, %)",
    title   = "Agreement between two-stage and headline disagreement measures",
    caption = paste0(
      "Each point is one conference-date. Shaded band = 95% confidence interval of OLS fit. ",
      "ρ = Spearman correlation between the two measures per tenor."
    )
  ) +
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
    verdict = ifelse(ci_low_delta <= 0 & 0 <= ci_high_delta, "Refuted", "Concern")
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
  zero_in_ci  <- row$ci_low_delta <= 0 & 0 <= row$ci_high_delta
  verdict_str <- if (zero_in_ci) "[REFUTED]" else "[CONCERN]"
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
    "\\s*(.+?)\\s*\\|\\s*(.+?)\\s*\\|\\s*(.+?)\\s*\\|?\\s*$"
  )
  m <- str_match(lines, pat)
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  if (nrow(m) == 0) return(tibble())
  tibble(
    agent_id     = m[, 2],
    archetype    = m[, 3],
    risk_profile = m[, 4],
    time_horizon = m[, 5],
    prior_view   = m[, 6],
    key_bias     = m[, 7]
  )
}

panel_files <- list.files(panels_dir, pattern = "\\.rds$", full.names = TRUE)
cat(paste0("  Panel files found: ", length(panel_files), "\n\n"))

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
    prior_view   = str_trim(prior_view),
    risk_profile = str_trim(risk_profile),
    # Normalise free-text categories to canonical values
    prior_view = case_when(
      str_detect(prior_view,   regex("hawk",  ignore_case = TRUE)) ~ "Hawkish",
      str_detect(prior_view,   regex("dovi?", ignore_case = TRUE)) ~ "Dovish",
      str_detect(prior_view,   regex("neut",  ignore_case = TRUE)) ~ "Neutral",
      TRUE ~ prior_view
    ),
    risk_profile = case_when(
      str_detect(risk_profile, regex("high", ignore_case = TRUE)) ~ "High",
      str_detect(risk_profile, regex("med",  ignore_case = TRUE)) ~ "Medium",
      str_detect(risk_profile, regex("low",  ignore_case = TRUE)) ~ "Low",
      TRUE ~ risk_profile
    ),
    date = as.Date(date)
  ) %>%
  filter(
    prior_view   %in% c("Hawkish", "Neutral", "Dovish"),
    risk_profile %in% c("High", "Medium", "Low"),
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

col_pv   <- c("Hawkish" = "#d73027", "Neutral" = "grey70", "Dovish" = "#4575b4")
col_risk <- c("High"    = "#d73027", "Medium"  = "#fee090", "Low"    = "#4575b4")

# Average across all 5 runs per date before computing proportions
pv_by_date <- panels_long %>%
  count(date, prior_view) %>%
  group_by(date) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    prior_view = factor(prior_view, levels = c("Hawkish", "Neutral", "Dovish")),
    date_fct   = factor(as.character(date))
  ) %>%
  arrange(date)

risk_by_date <- panels_long %>%
  count(date, risk_profile) %>%
  group_by(date) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(
    risk_profile = factor(risk_profile, levels = c("High", "Medium", "Low")),
    date_fct     = factor(as.character(date))
  ) %>%
  arrange(date)

arch_by_date <- panels_long %>%
  mutate(
    arch_cat = case_when(
      str_detect(archetype, regex("commercial",              ignore_case = TRUE)) ~ "Commercial Bank",
      str_detect(archetype, regex("investment\\s*bank",      ignore_case = TRUE)) ~ "Investment Bank",
      str_detect(archetype, regex("hedge",                   ignore_case = TRUE)) ~ "Hedge Fund",
      str_detect(archetype, regex("pension",                 ignore_case = TRUE)) ~ "Pension Fund",
      str_detect(archetype, regex("insur",                   ignore_case = TRUE)) ~ "Insurance",
      str_detect(archetype, regex("asset\\s*manag",          ignore_case = TRUE)) ~ "Asset Manager",
      str_detect(archetype, regex("proprietary|prop.*trad",  ignore_case = TRUE)) ~ "Prop. Trading",
      TRUE ~ "Other"
    )
  ) %>%
  count(date, arch_cat) %>%
  group_by(date) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(date_fct = factor(as.character(date))) %>%
  arrange(date)

# Order legend by overall prevalence
arch_levels <- arch_by_date %>%
  group_by(arch_cat) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(arch_cat)

arch_by_date <- mutate(arch_by_date,
                       arch_cat = factor(arch_cat, levels = arch_levels))

col_arch <- c(
  "Commercial Bank" = "#4575b4",
  "Investment Bank" = "#74add1",
  "Hedge Fund"      = "#d73027",
  "Asset Manager"   = "#2ca25f",
  "Pension Fund"    = "#fee090",
  "Insurance"       = "#fc8d59",
  "Prop. Trading"   = "#762a83",
  "Other"           = "grey65"
)

# Shared x-axis: every 5th date label to avoid clutter
date_levels <- levels(pv_by_date$date_fct)
x_breaks    <- date_levels[seq(1, length(date_levels), by = 5)]
x_labs      <- format(as.Date(x_breaks), "%b\n%Y")

# --- Net hawkishness: single derived metric, positive = net-hawk panel ----------
net_hawk_by_date <- panels_long %>%
  group_by(date) %>%
  summarise(
    pct_hawk = mean(prior_view == "Hawkish") * 100,
    pct_dove = mean(prior_view == "Dovish")  * 100,
    net_hawk = pct_hawk - pct_dove,
    .groups  = "drop"
  ) %>%
  arrange(date) %>%
  mutate(
    date_fct  = factor(as.character(date), levels = date_levels),
    direction = if_else(net_hawk >= 0, "Net Hawkish", "Net Dovish")
  )

p_pv <- ggplot(net_hawk_by_date,
               aes(x = date_fct, y = net_hawk, fill = direction)) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.5) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = c("Net Hawkish" = "#d73027",
                                "Net Dovish"  = "#4575b4"),
                    name = NULL) +
  scale_x_discrete(breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(labels = scales::number_format(suffix = " pp",
                                                    accuracy = 1)) +
  labs(x = NULL, y = "% Hawkish − % Dovish",
       title = "Net hawkishness of agent panel") +
  base_theme +
  theme(
    legend.position = "right",
    legend.text     = element_text(size = 11),
    plot.title      = element_text(size = 12, face = "bold", hjust = 0.5)
  )

# --- Archetype heatmap: z-score WITHIN each row so colour shows deviation ------
# from that archetype's own average — not absolute share (which is ~1/n_types)
max_pct_arch <- max(arch_by_date$pct)
min_pct_arch <- min(arch_by_date$pct)

p_arch <- ggplot(arch_by_date,
                 aes(x = date_fct, y = arch_cat, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.35) +
  scale_fill_distiller(
    palette   = "Blues",
    direction = 1,
    trans     = "sqrt",
    name      = NULL,
    breaks    = c(min_pct_arch, max_pct_arch),
    labels    = c("0%", paste0(round(max_pct_arch), "%"))
  ) +
  scale_x_discrete(breaks = x_breaks, labels = x_labs) +
  scale_y_discrete(limits = rev(arch_levels)) +
  labs(x = NULL, y = NULL, title = "Archetype share (%)") +
  base_theme +
  theme(
    panel.border      = element_blank(),
    panel.grid.major  = element_blank(),
    plot.title        = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x       = element_text(size = 10, lineheight = 1.1),
    axis.text.y       = element_text(size = 10),
    legend.position   = "right",
    legend.key.height = unit(1.5, "cm")
  )

fig3 <- p_arch +
  plot_annotation(
    title   = "",
    caption = "",
    theme = theme(
      plot.title   = element_text(size = 13, face = "bold", hjust = 0.5,
                                  margin = margin(b = 6)),
      plot.caption = element_text(size = 9, color = "grey40", hjust = 0,
                                  lineheight = 1.3, margin = margin(t = 8))
    )
  )

ggsave(file.path(output_dir, "fig3_panel_composition.pdf"),
       fig3, dpi = 320, width = 12, height = 5, bg = "white")
cat("  Saved: fig3_panel_composition.pdf\n")

# ==============================================================================
# 9. FIGURE 4 — Cross-run stability per conference
# ==============================================================================

cat("Building Figure 4 (cross-run stability)...\n")

# Per (date, run): net hawkishness = %Hawkish - %Dovish
run_pv <- panels_long %>%
  group_by(date, run) %>%
  summarise(
    pct_hawk  = mean(prior_view == "Hawkish") * 100,
    pct_dove  = mean(prior_view == "Dovish")  * 100,
    pct_neut  = mean(prior_view == "Neutral") * 100,
    net_hawk  = pct_hawk - pct_dove,   # positive = net hawkish panel
    .groups   = "drop"
  )

# Mean across runs per date (for the connecting line)
run_mean_pv <- run_pv %>%
  group_by(date) %>%
  summarise(
    mean_hawk  = mean(net_hawk),
    sd_hawk    = sd(net_hawk),
    mean_phawk = mean(pct_hawk),
    .groups    = "drop"
  )

# Three-panel plot: %Hawkish dots / %Dovish dots / net hawkishness dots
run_pv_long <- run_pv %>%
  select(date, run, Hawkish = pct_hawk, Dovish = pct_dove) %>%
  pivot_longer(c(Hawkish, Dovish), names_to = "view", values_to = "pct") %>%
  mutate(view = factor(view, levels = c("Hawkish", "Dovish")))

run_mean_long <- run_pv_long %>%
  group_by(date, view) %>%
  summarise(mean_pct = mean(pct), .groups = "drop")

p_dots <- ggplot(run_pv_long, aes(x = date, y = pct, color = view)) +
  geom_hline(yintercept = 33, linetype = "dotted",
             color = "grey60", linewidth = 0.4) +
  geom_point(alpha = 0.35, size = 2.2,
             position = position_jitter(width = 10, seed = 42)) +
  geom_line(data = run_mean_long, aes(y = mean_pct), linewidth = 0.8) +
  facet_wrap(~ view, nrow = 1) +
  scale_color_manual(values = c("Hawkish" = "#d73027", "Dovish" = "#4575b4"),
                     guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 80),
                     labels = scales::percent_format(scale = 1)) +
  labs(
    x = NULL, y = "% of agents",
    title = "Hawkish / Dovish share across 5 runs per conference"
  ) +
  base_theme +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

# Bottom panel: cross-run SD of net hawkishness — measures instability directly
p_sd <- ggplot(run_mean_pv, aes(x = date, y = sd_hawk)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey60", linewidth = 0.4) +
  geom_col(fill = "grey60", alpha = 0.8, width = 40) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                     expand  = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL,
    y = "SD across 5 runs (pp)",
    title = "Within-conference instability"
  ) +
  base_theme +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

fig4 <- p_dots / p_sd +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title   = "Panel stability across 5 independent draws per conference",
    caption = paste0(
      "Top: semi-transparent dots = individual runs; solid line = mean. ",
      "Dotted reference = 33% (uniform split across three views). ",
      "Bottom: SD of (% Hawkish − % Dovish) across 5 runs — ",
      "low values indicate the LLM assigns consistent prior views ",
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
cat("Cross-run stability summary (SD of net hawkishness across 5 runs):\n")
run_mean_pv %>%
  summarise(
    mean_sd = mean(sd_hawk, na.rm = TRUE),
    max_sd  = max(sd_hawk,  na.rm = TRUE),
    p90_sd  = quantile(sd_hawk, 0.9, na.rm = TRUE)
  ) %>%
  print()

cat("\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
