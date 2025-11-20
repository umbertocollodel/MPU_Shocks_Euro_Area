#==============================================================================
# SCRIPT: Regression Analysis of OIS Target on Synthetic Measures
#==============================================================================
# This script studies the interaction between synthetic SD and average
# confidence of agents. It runs three regression models of the actual OIS
# target (correct_post_mean_3) on:
#   1. Synthetic SD (standard deviation of simulated rate changes)
#   2. Average confidence score of agents for each conference
#   3. Interaction effect between synthetic SD and average confidence
#
# Output: LaTeX table with regression results

# Load necessary libraries
library(tidyverse)
library(readxl)

#------------------------------------------------------------------------------
## 1. LOAD AND PROCESS SIMULATION DATA (NAIVE PROMPT)
#------------------------------------------------------------------------------

# Read LLM simulation results
clean_df <- read_xlsx(paste0("../intermediate_data/aggregate_gemini_result/prompt_naive/",
                             "2.5flash_",
                             "2025-07-21",
                             ".xlsx"))

# Calculate synthetic SD (standard deviation of rate) by date and tenor
synthetic_sd_df <- clean_df %>%
  group_by(date, tenor) %>%
  summarise(
    synthetic_sd = sd(rate, na.rm = TRUE),
    avg_confidence = mean(confidence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(date))

#------------------------------------------------------------------------------
## 2. LOAD ACTUAL OIS TARGET DATA
#------------------------------------------------------------------------------

# Read actual OIS target data (correct_post_mean)
ois_df <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date, correct_post_mean_3) %>%
  mutate(date = as.Date(date))

#------------------------------------------------------------------------------
## 3. MERGE DATASETS
#------------------------------------------------------------------------------

# Combine synthetic measures with actual OIS target
regression_df <- ois_df %>%
  inner_join(synthetic_sd_df, by = c("date", "tenor")) %>%
  drop_na(correct_post_mean_3, synthetic_sd, avg_confidence)

# Display summary statistics of the merged data
cat("\n=== SUMMARY STATISTICS ===\n")
print(summary(regression_df))

cat("\n=== NUMBER OF OBSERVATIONS BY TENOR ===\n")
print(table(regression_df$tenor))

#------------------------------------------------------------------------------
## 4. RUN THREE REGRESSION MODELS
#------------------------------------------------------------------------------

# Model 1: Only synthetic_sd
model1 <- lm(correct_post_mean_3 ~ synthetic_sd, data = regression_df)

# Model 2: synthetic_sd + avg_confidence
model2 <- lm(correct_post_mean_3 ~ synthetic_sd + avg_confidence, data = regression_df)

# Model 3: with interaction effect
model3 <- lm(correct_post_mean_3 ~ synthetic_sd * avg_confidence, data = regression_df)

# Display regression results
cat("\n=== MODEL 1: synthetic_sd only ===\n")
print(summary(model1))

cat("\n=== MODEL 2: synthetic_sd + avg_confidence ===\n")
print(summary(model2))

cat("\n=== MODEL 3: with interaction ===\n")
print(summary(model3))

#------------------------------------------------------------------------------
## 5. CREATE LATEX TABLE
#------------------------------------------------------------------------------

library(stargazer)

# Create output directory if it doesn't exist
output_dir <- "../output/tables"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Generate LaTeX table
stargazer(model1, model2, model3,
          type = "latex",
          title = "Regression Results: OIS Target on Synthetic Measures",
          dep.var.labels = "OIS Target (bps)",
          covariate.labels = c("Synthetic SD", "Avg. Confidence", "Synthetic SD $\\times$ Avg. Confidence"),
          out = file.path(output_dir, "regression_results_confidence.tex"),
          no.space = TRUE,
          keep.stat = c("n", "rsq", "adj.rsq"),
          notes = "Standard errors in parentheses.",
          notes.append = FALSE)

cat("\nLaTeX table saved to:", file.path(output_dir, "regression_results_confidence.tex"), "\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
