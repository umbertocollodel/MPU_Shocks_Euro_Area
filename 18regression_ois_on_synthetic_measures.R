#==============================================================================
# SCRIPT: Volatility-Persistence Regression — Table 3
#==============================================================================
# Regresses post-conference OIS volatility on pre-conference volatility and
# LLM ensemble synthetic disagreement, with a policy-surprise control.
#
# Output: ../output/tables/table3_volatility_persistence.tex

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, stargazer, sandwich)

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

ois_df <- readRDS("../intermediate_data/range_difference_df.rds") %>%
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
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------------
## 4. LOAD POLICY SURPRISE DATA
#------------------------------------------------------------------------------
# Source: MPD "Press Conference Window" sheet — conference-window OIS changes (bps).
# Unit conversion: bps / 100 -> pp, so abs_surprise aligns with correct_post_mean_1
# and synthetic_sd.

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
    tenor        = str_remove(tenor, "^OIS_"),
    abs_surprise = abs(surprise_bps / 100)
  )

#------------------------------------------------------------------------------
## 5. LOAD BID-ASK SPREAD DATA
#------------------------------------------------------------------------------
# Bid-ask spread: ask minus bid close, 1 trading day post-conference (bps).
# Standardised within-tenor so the coefficient is comparable across maturities.

spread_path <- "../intermediate_data/post_spread_1d.rds"
if (!file.exists(spread_path)) stop("post_spread_1d.rds not found. Generate it from the bid-ask spread pipeline before running this script.")
post_spread_df <- readRDS(spread_path) %>%
  mutate(date = as.Date(date))

#------------------------------------------------------------------------------
## 6. BUILD COMBINED DATASET
#------------------------------------------------------------------------------
# Both variables joined via left_join — no upfront drop_na on surprise or spread.
# lm() handles NAs per-spec via na.omit, so each column uses its natural sample:
#   m1–m2: ~540 obs (no spread/surprise predictors)
#   m3:    ~surprise-available subset
#   m4:    ~528 obs (spread available)
#   m5:    ~484 obs (intersection — kitchen sink)
# vcovCL with data = regression_df_all is safe: sandwich subsets the cluster
# vector to complete rows automatically via the model's na.action attribute.

regression_df_all <- regression_df %>%
  left_join(post_spread_df %>% select(date, tenor, post_spread_1d_bps),
            by = c("date", "tenor")) %>%
  left_join(surprise_long %>% select(date, tenor, abs_surprise),
            by = c("date", "tenor")) %>%
  group_by(tenor) %>%
  mutate(spread_std = (post_spread_1d_bps - mean(post_spread_1d_bps, na.rm = TRUE)) /
                       sd(post_spread_1d_bps, na.rm = TRUE)) %>%
  ungroup()

cat("\n=== DATA AVAILABILITY ===\n")
cat(sprintf("Total rows:         %d\n", nrow(regression_df_all)))
cat(sprintf("With surprise:      %d\n", sum(!is.na(regression_df_all$abs_surprise))))
cat(sprintf("With spread:        %d\n", sum(!is.na(regression_df_all$post_spread_1d_bps))))
cat(sprintf("With both (m5 N):   %d\n", sum(!is.na(regression_df_all$abs_surprise) &
                                              !is.na(regression_df_all$post_spread_1d_bps))))

cat("\n=== CORR(|Surprise|, Synthetic SD) ===\n")
r_corr <- cor(regression_df_all$abs_surprise, regression_df_all$synthetic_sd,
              use = "complete.obs")
cat(sprintf("r = %.3f\n", r_corr))

#------------------------------------------------------------------------------
## 7. COMBINED TABLE: VOLATILITY PERSISTENCE + LIQUIDITY ROBUSTNESS
#------------------------------------------------------------------------------
# DV: correct_post_mean_1 (post-conference 1-day OIS high-low range, pp).
# Column progression: AR baseline -> + synthetic SD -> + surprise (preferred) ->
#   + liquidity -> all controls (kitchen sink).
# Maturity FE absorbed via factor(tenor); date-clustered SEs throughout.

clust_se <- function(mod) sqrt(diag(vcovCL(mod, cluster = ~date,
                                           data = regression_df_all)))

# (1) Synthetic SD only
m1 <- lm(correct_post_mean_1 ~ synthetic_sd,
         data = regression_df_all)

# (2) + AR: pre-conference volatility
m2 <- lm(correct_post_mean_1 ~ synthetic_sd + correct_pre_mean_3,
         data = regression_df_all)

# (3) + Bid-ask spread level + Maturity FE
m3 <- lm(correct_post_mean_1 ~ synthetic_sd + correct_pre_mean_3 +
           spread_std + factor(tenor),
         data = regression_df_all)

# (4) + Bid-ask spread × interaction + Maturity FE (liquidity robustness)
m4 <- lm(correct_post_mean_1 ~ synthetic_sd + correct_pre_mean_3 +
           spread_std + synthetic_sd:spread_std + factor(tenor),
         data = regression_df_all)

# (5) + |Surprise| + Maturity FE  [preferred]
m5 <- lm(correct_post_mean_1 ~ synthetic_sd + correct_pre_mean_3 +
           abs_surprise + factor(tenor),
         data = regression_df_all)

# (6) All controls + Maturity FE [kitchen sink]
m6 <- lm(correct_post_mean_1 ~ synthetic_sd + correct_pre_mean_3 +
           spread_std + synthetic_sd:spread_std + abs_surprise + factor(tenor),
         data = regression_df_all)

se_list <- lapply(list(m1, m2, m3, m4, m5, m6), clust_se)

# Two-way clustering robustness for preferred spec (not in main table)
se_m5_2w <- sqrt(diag(vcovCL(m5, cluster = ~date + tenor,
                              data = regression_df_all)))

cat("\n=== SYNTHETIC SD COEFFICIENT ACROSS SPECS ===\n")
for (i in 1:6) {
  mod_i <- get(sprintf("m%d", i))
  cat(sprintf("Spec (%d): coef = %+.5f  SE = %.5f\n",
              i, coef(mod_i)["synthetic_sd"], se_list[[i]]["synthetic_sd"]))
}
cat(sprintf("Spec (5) 2W: coef = %+.5f  SE = %.5f  [two-way cluster robustness]\n",
            coef(m5)["synthetic_sd"], se_m5_2w["synthetic_sd"]))

stargazer(m1, m2, m3, m4, m5, m6,
          type = "latex",
          title = "Post-Conference OIS Volatility: Synthetic Disagreement, Policy Surprise, and Liquidity",
          dep.var.labels = "Post-conf.\\ OIS high-low range, 1-day",
          order = c("^synthetic_sd$", "correct_pre_mean_3", "^spread_std$",
                    "synthetic_sd:spread_std", "abs_surprise"),
          covariate.labels = c("Synthetic SD",
                               "Pre-conf.\\ OIS vol.\\ 3-day",
                               "Bid-ask spread, std.",
                               "Synthetic SD $\\times$ Bid-ask spread",
                               "$|\\text{Surprise}|$"),
          se      = se_list,
          omit    = "factor\\(tenor\\)",
          add.lines = list(
            c("Maturity FE",        "No",  "No",  "Yes", "Yes", "Yes", "Yes"),
            c("Date-clustered SE",  "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
          ),
          out      = file.path(output_dir, "table3_volatility_persistence.tex"),
          no.space = TRUE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          notes    = paste0(
            "Date-clustered SEs in parentheses. ",
            "Surprise = conference-window OIS change (bps) $\\div$ 100. ",
            "Bid-ask spread standardised within each tenor (mean 0, SD 1). ",
            "Column (6) includes all controls simultaneously."
          ),
          notes.append = FALSE)

cat("\nTable saved to:", file.path(output_dir, "table3_volatility_persistence.tex"), "\n")
cat("\n=== ANALYSIS COMPLETE ===\n")
