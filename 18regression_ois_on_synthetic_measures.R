#==============================================================================
# SCRIPT: Volatility-Persistence Regression — Table 3
#==============================================================================
# Regresses post-conference OIS volatility on pre-conference volatility and
# LLM ensemble synthetic disagreement, with a policy-surprise control.
#
# Output: ../output/tables/table3_volatility_persistence.tex

library(tidyverse)
library(readxl)
library(stargazer)
library(sandwich)  # vcovCL for clustered SEs

#------------------------------------------------------------------------------
## 1. LOAD SIMULATION DATA
#------------------------------------------------------------------------------

# Ensemble SD: mean of within-run SDs across R=10 runs (post_zero_shot_p1.R §3).
# Tenor is already "3M"/"2Y"/"10Y" (factor converted to char on save).
ensemble_sd_df <- readRDS("../intermediate_data/p1/p1_ensemble_sd.rds") %>%
  mutate(date = as.Date(date)) %>%
  select(date, tenor, synthetic_sd = sd_mean)

#------------------------------------------------------------------------------
## 2. LOAD ACTUAL OIS DATA
#------------------------------------------------------------------------------

ois_df <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date, correct_pre_mean_3, correct_post_mean_1) %>%
  mutate(date = as.Date(date))

#------------------------------------------------------------------------------
## 3. MERGE
#------------------------------------------------------------------------------

regression_df <- ois_df %>%
  inner_join(ensemble_sd_df, by = c("date", "tenor")) %>%
  drop_na(correct_pre_mean_3, correct_post_mean_1, synthetic_sd)

cat("\n=== SUMMARY STATISTICS ===\n")
print(summary(regression_df))

cat("\n=== NUMBER OF OBSERVATIONS BY TENOR ===\n")
print(table(regression_df$tenor))

output_dir <- "../output/tables"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

#------------------------------------------------------------------------------
## 4. TABLE 2: HIGH-LOW RANGE ~ SYNTHETIC SD × BID-ASK SPREAD
#------------------------------------------------------------------------------
# Addresses referee concern that the OIS high-low range reflects illiquidity
# rather than belief dispersion.
# Logic: if synthetic_sd still predicts the range after controlling for the
# bid-ask spread and its interaction with synthetic_sd, the belief-dispersion
# channel is separable from the liquidity channel.
# Bid-ask spread: ask minus bid close, 1 trading day post-conference (bps).

post_spread_df <- readRDS("../intermediate_data/post_spread_1d.rds") %>%
  mutate(date = as.Date(date))

regression_df_liq <- regression_df %>%
  left_join(post_spread_df %>% select(date, tenor, post_spread_1d_bps),
            by = c("date", "tenor")) %>%
  drop_na(post_spread_1d_bps) %>%
  # Standardise spread within-tenor so the interaction coefficient is
  # comparable across maturities (3M spreads are structurally smaller than 10Y).
  group_by(tenor) %>%
  mutate(spread_std = (post_spread_1d_bps - mean(post_spread_1d_bps)) /
                       sd(post_spread_1d_bps)) %>%
  ungroup()

cat("\n=== BID-ASK MERGE: N PER TENOR ===\n")
print(table(regression_df_liq$tenor))

clust_se_liq <- function(mod) sqrt(diag(vcovCL(mod, cluster = ~date,
                                               data = regression_df_liq)))

# (1) Baseline: synthetic SD only
liq1 <- lm(correct_post_mean_1 ~ synthetic_sd,
           data = regression_df_liq)

# (2) + standardised bid-ask spread (liquidity level control)
liq2 <- lm(correct_post_mean_1 ~ synthetic_sd + spread_std,
           data = regression_df_liq)

# (3) + interaction: does the synthetic-SD effect vary with illiquidity?
liq3 <- lm(correct_post_mean_1 ~ synthetic_sd * spread_std,
           data = regression_df_liq)

# (4) + maturity FE
liq4 <- lm(correct_post_mean_1 ~ synthetic_sd * spread_std + factor(tenor),
           data = regression_df_liq)

se_liq <- lapply(list(liq1, liq2, liq3, liq4), clust_se_liq)

stargazer(liq1, liq2, liq3, liq4,
          type = "latex",
          title = "OIS High-Low Range, Synthetic Disagreement, and Illiquidity",
          dep.var.labels = "Post-conf.\\ OIS high-low range (pp)",
          covariate.labels = c("Synthetic SD (pp)",
                               "Bid-ask spread, std.\\ (within-tenor)",
                               "Synthetic SD $\\times$ Bid-ask spread"),
          se = se_liq,
          omit = "factor\\(tenor\\)",
          add.lines = list(
            c("Maturity FE",       "No",  "No",  "No",  "Yes"),
            c("Date-clustered SE", "Yes", "Yes", "Yes", "Yes")
          ),
          out      = file.path(output_dir, "table2_liquidity_interaction.tex"),
          no.space = TRUE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          notes    = paste0(
            "Date-clustered SEs in parentheses. ",
            "Bid-ask spread = ask$-$bid close price, 1 trading day after the GovC meeting, ",
            "standardised within each tenor (mean 0, SD 1). ",
            "Synthetic SD and the dependent variable are in percentage points."
          ),
          notes.append = FALSE)

cat("\nTable 2 saved to:", file.path(output_dir, "table2_liquidity_interaction.tex"), "\n")

#------------------------------------------------------------------------------
## 5. LOAD AND RESHAPE POLICY SURPRISE DATA
#------------------------------------------------------------------------------
# Source: MPD "Press Conference Window" sheet — conference-window OIS changes (bps).
# Tenor mapping: simulation "3M" -> OIS_3M, "2Y" -> OIS_2Y, "10Y" -> OIS_10Y.
# Rows with a blank date are non-conference days and are dropped.
# Within-row blanks (tenor not yet traded on that date) are kept as NA.
# Unit conversion: bps / 100 -> percentage points, so abs_surprise aligns with
# correct_post_mean_1 and synthetic_sd (both are pp-scale rate values).

raw_surprise <- read_xlsx(
  "../raw_data/00EA_MPD_update_june2025.xlsx",
  sheet = "Press Conference Window"
) %>%
  filter(!is.na(date)) %>%
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"))

surprise_long <- raw_surprise %>%
  select(date, OIS_3M, OIS_2Y, OIS_10Y) %>%
  pivot_longer(
    cols      = c(OIS_3M, OIS_2Y, OIS_10Y),
    names_to  = "tenor",
    values_to = "surprise_bps"
  ) %>%
  mutate(
    tenor        = str_remove(tenor, "^OIS_"),  # "OIS_3M" -> "3M"
    surprise_pp  = surprise_bps / 100,           # bps -> pp
    abs_surprise = abs(surprise_pp)
    # alt: signed_surprise = surprise_pp
  )

cat("\n=== SURPRISE DATA: N PER TENOR (before merge) ===\n")
print(table(surprise_long$tenor, useNA = "ifany"))

# Left join: every regression row is retained; NAs where the surprise is
# unavailable for a given date-tenor pair (e.g. early dates, missing tenors).
regression_df_s <- regression_df %>%
  left_join(
    surprise_long %>% select(date, tenor, abs_surprise),
    by = c("date", "tenor")
  )

cat("\n=== ROWS LOST AT MERGE ===\n")
cat(sprintf(
  "Regression rows before: %d | after left join: %d | net change: %+d\n",
  nrow(regression_df), nrow(regression_df_s),
  nrow(regression_df_s) - nrow(regression_df)
))

cat("\n=== MATCHED N PER TENOR ===\n")
print(table(regression_df_s$tenor, useNA = "ifany"))

cat("\n=== CORR(|Surprise|, Synthetic SD) ===\n")
r_corr <- cor(regression_df_s$abs_surprise, regression_df_s$synthetic_sd,
              use = "complete.obs")
n_corr <- sum(!is.na(regression_df_s$abs_surprise) &
              !is.na(regression_df_s$synthetic_sd))
cat(sprintf("r = %.3f  (N = %d)\n", r_corr, n_corr))

#------------------------------------------------------------------------------
## 6. TABLE 3: VOLATILITY-PERSISTENCE REGRESSION (lm ladder, date-clustered SEs)
#------------------------------------------------------------------------------
# DV: correct_post_mean_1 (post-conference 1-day OIS volatility, pp).
# Maturity FE absorbed via factor(tenor); FE rows suppressed in output via omit.
# All SEs clustered by date using vcovCL (sandwich); passed to stargazer via se=.
# Two-way clustering (~date + tenor) computed for the preferred spec as robustness.
# abs_surprise enters as a regressor — NOT absorbed by any FE.

clust_se <- function(mod) sqrt(diag(vcovCL(mod, cluster = ~date,
                                           data = regression_df_s)))

# (1) Baseline persistence
m1 <- lm(correct_post_mean_1 ~ correct_pre_mean_3,
         data = regression_df_s)

# (2) + Synthetic SD
m2 <- lm(correct_post_mean_1 ~ correct_pre_mean_3 + synthetic_sd,
         data = regression_df_s)

# (3) + Maturity FE
m3 <- lm(correct_post_mean_1 ~ correct_pre_mean_3 + synthetic_sd + factor(tenor),
         data = regression_df_s)

# (4) + |Surprise|, no maturity FE
m4 <- lm(correct_post_mean_1 ~ correct_pre_mean_3 + synthetic_sd + abs_surprise,
         data = regression_df_s)

# (5) + |Surprise| + Maturity FE  [preferred]
m5 <- lm(correct_post_mean_1 ~ correct_pre_mean_3 + synthetic_sd + abs_surprise +
           factor(tenor),
         data = regression_df_s)

# Date-clustered SEs for all five specs
se_list <- lapply(list(m1, m2, m3, m4, m5), clust_se)

# Two-way clustering robustness for preferred spec (not in main table)
se_m5_2w <- sqrt(diag(vcovCL(m5, cluster = ~date + tenor,
                              data = regression_df_s)))

# --- Synthetic SD coefficient tracking (3) -> (5) ---
cat("\n=== SYNTHETIC SD COEFFICIENT: SPEC (3) vs (5) ===\n")
cat(sprintf("Spec (3)    coef = %+.5f  SE = %.5f\n",
            coef(m3)["synthetic_sd"], se_list[[3]]["synthetic_sd"]))
cat(sprintf("Spec (5)    coef = %+.5f  SE = %.5f\n",
            coef(m5)["synthetic_sd"], se_list[[5]]["synthetic_sd"]))
cat(sprintf("Spec (5) 2W coef = %+.5f  SE = %.5f  [two-way cluster robustness]\n",
            coef(m5)["synthetic_sd"], se_m5_2w["synthetic_sd"]))

# --- LaTeX output ---
stargazer(m1, m2, m3, m4, m5,
          type = "latex",
          title = "Volatility Persistence: Post-Conference OIS Volatility",
          dep.var.labels = "Post-conf.\\ OIS vol.\\ 1-day (pp)",
          covariate.labels = c("Pre-conf.\\ OIS vol.\\ 3-day (pp)",
                               "Synthetic SD (pp)",
                               "$|\\text{Surprise}|$ (pp)"),
          se      = se_list,
          omit    = "factor\\(tenor\\)",
          add.lines = list(
            c("Maturity FE",        "No",  "No",  "Yes", "No",  "Yes"),
            c("Date-clustered SE",  "Yes", "Yes", "Yes", "Yes", "Yes")
          ),
          out      = file.path(output_dir, "table3_volatility_persistence.tex"),
          no.space = TRUE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          notes    = paste0(
            "Date-clustered SEs in parentheses. ",
            "Surprise = conference-window OIS change (bps) $\\div$ 100."
          ),
          notes.append = FALSE)

cat("\nTable 3 saved to:", file.path(output_dir, "table3_volatility_persistence.tex"), "\n")
cat("\n=== ANALYSIS COMPLETE ===\n")
