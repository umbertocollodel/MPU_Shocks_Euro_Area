#===============================================================================
# POST-ANALYSIS: FEW-SHOT vs. NAIVE ENSEMBLE
#===============================================================================
# Outputs:
#   ../output/figures/fewshot_vs_naive/fig1_correlation_bars.pdf
#   ../output/tables/fewshot_vs_naive_calibration.tex
#===============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, writexl, showtext, scales, stargazer)

font_add("Segoe UI", regular = "segoeui.ttf")
showtext_auto()

BASE_FONT    <- "Segoe UI"
BOOT_REPS    <- 5000
tenor_levels <- c("3M", "2Y", "10Y")
set.seed(20260512)

colors_prompt <- c("Naive" = "#4575b4", "Few-shot" = "#d73027")

dir.create("../output/figures/fewshot_vs_naive", recursive = TRUE, showWarnings = FALSE)
dir.create("../output/tables",                   recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. DATA
# ==============================================================================

fewshot_sd <- readRDS("../intermediate_data/fewshot_ensemble_p1/parsed/ensemble_sd.rds") %>%
  mutate(date = as.character(date), tenor = factor(tenor, levels = tenor_levels))

naive_sd <- readRDS("../intermediate_data/p1/p1_ensemble_sd.rds") %>%
  mutate(date = as.character(date), tenor = factor(tenor, levels = tenor_levels))

market_vol <- readRDS("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = if_else(tenor == "3mnt", "3M", tenor),
         tenor = factor(tenor, levels = tenor_levels),
         date  = as.character(date)) %>%
  filter(tenor %in% tenor_levels, !is.na(correct_post_mean_1)) %>%
  select(date, tenor, vol_1d = correct_post_mean_1)

the_dates <- readRDS("../intermediate_data/fewshot_ensemble_p1/selected_dates.rds") %>%
  pull(date) %>% as.character()

merged <- fewshot_sd %>%
  select(date, tenor, sd_fewshot = sd_mean) %>%
  inner_join(naive_sd %>% filter(date %in% the_dates) %>%
               select(date, tenor, sd_naive = sd_mean), by = c("date", "tenor")) %>%
  inner_join(market_vol, by = c("date", "tenor")) %>%
  filter(!is.na(sd_fewshot), !is.na(sd_naive), !is.na(vol_1d))

# ==============================================================================
# 2. FIGURE 1 — Correlation bar chart
# ==============================================================================

sig_stars <- function(p) case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")

boot_spearman <- function(x, y) {
  n  <- length(x)
  rh <- cor(x, y, method = "spearman", use = "complete.obs")
  ci <- quantile(replicate(BOOT_REPS, {
    i <- sample(n, n, TRUE); cor(x[i], y[i], method = "spearman", use = "complete.obs")
  }), c(0.025, 0.975))
  list(rho = rh, ci = ci)
}

cor_tbl <- merged %>%
  group_by(tenor) %>%
  group_modify(~ {
    fs <- boot_spearman(.x$sd_fewshot, .x$vol_1d)
    na <- boot_spearman(.x$sd_naive,   .x$vol_1d)
    tibble(rho_fs = fs$rho, ci_fs_lo = fs$ci[1], ci_fs_hi = fs$ci[2],
           rho_na = na$rho, ci_na_lo = na$ci[1], ci_na_hi = na$ci[2])
  }) %>%
  ungroup() %>%
  mutate(prompt_fs = "Few-shot", prompt_na = "Naive")

cor_long <- bind_rows(
  cor_tbl %>% transmute(tenor, prompt = "Few-shot",
                        rho = rho_fs, ci_lo = ci_fs_lo, ci_hi = ci_fs_hi),
  cor_tbl %>% transmute(tenor, prompt = "Naive",
                        rho = rho_na, ci_lo = ci_na_lo, ci_hi = ci_na_hi)
) %>% mutate(prompt = factor(prompt, levels = c("Naive", "Few-shot")))

p_bars <- ggplot(cor_long, aes(x = tenor, y = rho, fill = prompt)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.9) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                position = position_dodge(0.7),
                width = 0.2, linewidth = 0.6, color = "grey30") +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey50") +
  scale_fill_manual(values = colors_prompt) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(x = NULL, y = "Spearman ρ with market volatility", fill = NULL) +
  theme_minimal(base_family = BASE_FONT) +
  theme(legend.position  = "top",
        panel.border     = element_rect(colour = "grey80", fill = NA),
        panel.grid.minor = element_blank())

ggsave("../output/figures/fewshot_vs_naive/fig1_correlation_bars.pdf",
       p_bars, width = 7, height = 5, dpi = 300, bg = "white")

# ==============================================================================
# 3. TABLE — Calibration (MAE + bias vs market)
# ==============================================================================

calibration <- bind_rows(
  merged %>% group_by(tenor),
  merged %>% mutate(tenor = factor("Pooled", levels = c(tenor_levels, "Pooled"))) %>%
             group_by(tenor)
) %>%
  summarise(
    n            = n(),
    mean_market  = mean(vol_1d,     na.rm = TRUE),
    mean_naive   = mean(sd_naive,   na.rm = TRUE),
    mean_fewshot = mean(sd_fewshot, na.rm = TRUE),
    bias_naive   = mean(sd_naive   - vol_1d, na.rm = TRUE),
    bias_fewshot = mean(sd_fewshot - vol_1d, na.rm = TRUE),
    mae_naive    = mean(abs(sd_naive   - vol_1d), na.rm = TRUE),
    mae_fewshot  = mean(abs(sd_fewshot - vol_1d), na.rm = TRUE),
    delta_mae    = mae_naive - mae_fewshot,
    p_bias_na    = t.test(sd_naive   - vol_1d)$p.value,
    p_bias_fs    = t.test(sd_fewshot - vol_1d)$p.value,
    p_delta      = t.test(abs(sd_naive - vol_1d) - abs(sd_fewshot - vol_1d))$p.value,
    .groups = "drop"
  )

sg_tbl <- calibration %>%
  transmute(
    Tenor        = as.character(tenor),
    N            = as.integer(n),
    Market       = sprintf("%.3f", mean_market),
    Naive        = sprintf("%.3f", mean_naive),
    `Few-shot`   = sprintf("%.3f", mean_fewshot),
    `Bias naive` = sprintf("%.3f%s", bias_naive,   sig_stars(p_bias_na)),
    `Bias fs`    = sprintf("%.3f%s", bias_fewshot,  sig_stars(p_bias_fs)),
    `MAE naive`  = sprintf("%.3f", mae_naive),
    `MAE fs`     = sprintf("%.3f", mae_fewshot),
    `Delta MAE`  = sprintf("%.3f%s", delta_mae,    sig_stars(p_delta))
  ) %>%
  as.data.frame()

stargazer(sg_tbl,
          type         = "latex",
          summary      = FALSE,
          rownames     = FALSE,
          title        = "Calibration of LLM disagreement relative to realized market volatility",
          label        = "tab:calibration",
          notes        = c(
            "Bias $= \\bar{x}_{LLM} - \\bar{x}_{mkt}$; paired $t$-test $H_0$: bias $= 0$.",
            "Delta MAE $=$ MAE$_{naive}$ $-$ MAE$_{fs}$; paired $t$-test on absolute errors.",
            "Market $=$ 1-day post-conference OIS SD.",
            "\\sym{*} $p<0.10$, \\sym{**} $p<0.05$, \\sym{***} $p<0.01$."
          ),
          notes.align  = "l",
          notes.append = FALSE,
          out          = "../output/tables/fewshot_vs_naive_calibration.tex")

#===============================================================================
# END
#===============================================================================
