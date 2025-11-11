#===============================================================================
# COMPLETE PIPELINE EXECUTION SCRIPT
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Master script to run the complete analysis pipeline from start to finish.
#   Executes all scripts in the correct order with proper error handling.
#
# Usage:
#   source("00run_complete_pipeline.R")
#
# IMPORTANT:
#   - Ensure .Renviron file exists with GEMINI_API_KEY
#   - Stage 3 (LLM models) takes 10-20 hours
#   - Total estimated time: 15-25 hours
#   - Total estimated cost: $40-60 in API calls
#
#===============================================================================

# Track overall timing
pipeline_start <- Sys.time()

# Create log file
log_file <- paste0("pipeline_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
log_msg <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste0("[", timestamp, "] ", msg)
  cat(message, "\n")
  cat(message, "\n", file = log_file, append = TRUE)
}

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║  COMPLETE PIPELINE EXECUTION                                               ║\n")
cat("║  Interpreting the Interpreter - ECB Communication Analysis                 ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

log_msg("Pipeline execution started")
log_msg(paste("Log file:", log_file))

# Check prerequisites
log_msg("Checking prerequisites...")

if (!file.exists(".Renviron")) {
  stop("❌ .Renviron file not found. Please create it with your GEMINI_API_KEY")
}

if (Sys.getenv("GEMINI_API_KEY") == "") {
  stop("❌ GEMINI_API_KEY not set in .Renviron file")
}

log_msg("✅ Prerequisites checked")

#===============================================================================
# STAGE 1: BASELINE MPU INDEX (5-10 minutes)
#===============================================================================
cat("\n")
cat(crayon::blue("╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 1: BASELINE MPU INDEX                                               ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n"))
cat("\n")

stage1_start <- Sys.time()

# Script 01: Create MPU index
log_msg("[1/3] Running 01create_MPU.R...")
tryCatch({
  source("01create_MPU.R")
  log_msg("✅ 01create_MPU.R completed")
}, error = function(e) {
  log_msg(paste("❌ Error in 01create_MPU.R:", e$message))
  stop(e)
})

# Script 02: Plot MPU and compare with MP surprises
log_msg("[2/3] Running 02plot_MPU_and_compare_with_MP_surprises.R...")
tryCatch({
  source("02plot_MPU_and_compare_with_MP_surprises.R")
  log_msg("✅ 02plot_MPU_and_compare_with_MP_surprises.R completed")
}, error = function(e) {
  log_msg(paste("❌ Error in 02plot_MPU_and_compare_with_MP_surprises.R:", e$message))
  stop(e)
})

# Script 03: Exogeneity tests
log_msg("[3/3] Running 03appendix_run_exogeneity_tests_MPU.R...")
tryCatch({
  source("03appendix_run_exogeneity_tests_MPU.R")
  log_msg("✅ 03appendix_run_exogeneity_tests_MPU.R completed")
}, error = function(e) {
  log_msg(paste("❌ Error in 03appendix_run_exogeneity_tests_MPU.R:", e$message))
  stop(e)
})

stage1_end <- Sys.time()
stage1_time <- stage1_end - stage1_start
log_msg(paste("Stage 1 completed in", round(stage1_time, 2), attr(stage1_time, "units")))

#===============================================================================
# STAGE 2: DATA PREPARATION (30-60 minutes)
#===============================================================================
cat("\n")
cat(crayon::blue("╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 2: DATA PREPARATION                                                 ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n"))
cat("\n")

stage2_start <- Sys.time()

# Script 04: Scraping ECB press conferences (optional - skip if transcripts exist)
log_msg("[1/3] Running 04scraping_ecb_pressconf.R...")
tryCatch({
  source("04scraping_ecb_pressconf.R")
  log_msg("✅ 04scraping_ecb_pressconf.R completed")
}, error = function(e) {
  log_msg(paste("⚠️  Warning in 04scraping_ecb_pressconf.R:", e$message))
  log_msg("    Continuing with existing transcripts...")
})

# Script 05: Calculate document complexity
log_msg("[2/3] Running 05calculate_complexity_documents.R...")
tryCatch({
  source("05calculate_complexity_documents.R")
  log_msg("✅ 05calculate_complexity_documents.R completed")
}, error = function(e) {
  log_msg(paste("❌ Error in 05calculate_complexity_documents.R:", e$message))
  stop(e)
})

# Script: US releases calendar (optional analysis)
log_msg("[3/3] Running calendar_us_releases.R...")
tryCatch({
  source("calendar_us_releases.R")
  log_msg("✅ calendar_us_releases.R completed")
}, error = function(e) {
  log_msg(paste("⚠️  Warning in calendar_us_releases.R:", e$message))
  log_msg("    This is optional - continuing...")
})

stage2_end <- Sys.time()
stage2_time <- stage2_end - stage2_start
log_msg(paste("Stage 2 completed in", round(stage2_time, 2), attr(stage2_time, "units")))

#===============================================================================
# STAGE 3: RUN ALL LLM MODELS (10-20 hours)
#===============================================================================
cat("\n")
cat(crayon::blue("╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 3: RUN ALL LLM MODELS                                               ║\n"))
cat(crayon::blue("║  ⚠️  WARNING: This stage takes 10-20 hours and costs $30-50               ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n"))
cat("\n")

stage3_start <- Sys.time()

log_msg("Running 07run_all_models_sequential.R...")
log_msg("This will run: naive → historical_surprise → llm_as_judge")
tryCatch({
  source("07run_all_models_sequential.R")
  log_msg("✅ 07run_all_models_sequential.R completed")
}, error = function(e) {
  log_msg(paste("❌ Error in 07run_all_models_sequential.R:", e$message))
  stop(e)
})

stage3_end <- Sys.time()
stage3_time <- stage3_end - stage3_start
log_msg(paste("Stage 3 completed in", round(stage3_time, 2), attr(stage3_time, "units")))

#===============================================================================
# STAGE 4: MAIN ANALYSIS (10 minutes)
#===============================================================================
cat("\n")
cat(crayon::blue("╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 4: MAIN ANALYSIS                                                    ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n"))
cat("\n")

stage4_start <- Sys.time()

# Script 08: Clean LLM results
log_msg("[1/2] Running 08clean_llm_result.R...")
tryCatch({
  source("08clean_llm_result.R")
  log_msg("✅ 08clean_llm_result.R completed")
}, error = function(e) {
  log_msg(paste("❌ Error in 08clean_llm_result.R:", e$message))
  stop(e)
})

# Script 09: Plot LLM results
log_msg("[2/2] Running 09plot_llm_results.R...")
tryCatch({
  source("09plot_llm_results.R")
  log_msg("✅ 09plot_llm_results.R completed")
}, error = function(e) {
  log_msg(paste("❌ Error in 09plot_llm_results.R:", e$message))
  stop(e)
})

stage4_end <- Sys.time()
stage4_time <- stage4_end - stage4_start
log_msg(paste("Stage 4 completed in", round(stage4_time, 2), attr(stage4_time, "units")))

#===============================================================================
# STAGE 5: ROBUSTNESS TESTS (1-2 hours)
#===============================================================================
cat("\n")
cat(crayon::blue("╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 5: ROBUSTNESS TESTS                                                 ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n"))
cat("\n")

stage5_start <- Sys.time()

# Script 12: Bootstrap robustness
log_msg("[1/4] Running 12run_bootstrap_robustness.R...")
tryCatch({
  source("12run_bootstrap_robustness.R")
  log_msg("✅ 12run_bootstrap_robustness.R completed")
}, error = function(e) {
  log_msg(paste("⚠️  Warning in 12run_bootstrap_robustness.R:", e$message))
})

# Script 14: Prompt stability test
log_msg("[2/4] Running 14run_prompt_stability_test_robustness.R...")
tryCatch({
  source("14run_prompt_stability_test_robustness.R")
  log_msg("✅ 14run_prompt_stability_test_robustness.R completed")
}, error = function(e) {
  log_msg(paste("⚠️  Warning in 14run_prompt_stability_test_robustness.R:", e$message))
})

# Script 15: Model stability test
log_msg("[3/4] Running 15run_model_stability_test_robustness.R...")
tryCatch({
  source("15run_model_stability_test_robustness.R")
  log_msg("✅ 15run_model_stability_test_robustness.R completed")
}, error = function(e) {
  log_msg(paste("⚠️  Warning in 15run_model_stability_test_robustness.R:", e$message))
})

# Script 16: Counterfactual exercise
log_msg("[4/4] Running 16run_counterfactual_exercise.R...")
tryCatch({
  source("16run_counterfactual_exercise.R")
  log_msg("✅ 16run_counterfactual_exercise.R completed")
}, error = function(e) {
  log_msg(paste("⚠️  Warning in 16run_counterfactual_exercise.R:", e$message))
})

stage5_end <- Sys.time()
stage5_time <- stage5_end - stage5_start
log_msg(paste("Stage 5 completed in", round(stage5_time, 2), attr(stage5_time, "units")))

#===============================================================================
# STAGE 6: OUT-OF-SAMPLE TEST (1-2 hours)
#===============================================================================
cat("\n")
cat(crayon::blue("╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 6: OUT-OF-SAMPLE TEST                                               ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n"))
cat("\n")

stage6_start <- Sys.time()

log_msg("Running 17run_real_oos_test.R...")
tryCatch({
  source("17run_real_oos_test.R")
  log_msg("✅ 17run_real_oos_test.R completed")
}, error = function(e) {
  log_msg(paste("⚠️  Warning in 17run_real_oos_test.R:", e$message))
})

stage6_end <- Sys.time()
stage6_time <- stage6_end - stage6_start
log_msg(paste("Stage 6 completed in", round(stage6_time, 2), attr(stage6_time, "units")))

#===============================================================================
# PIPELINE SUMMARY
#===============================================================================
pipeline_end <- Sys.time()
pipeline_time <- pipeline_end - pipeline_start

cat("\n")
cat(crayon::green("╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::green("║  PIPELINE EXECUTION COMPLETE                                               ║\n"))
cat(crayon::green("╚════════════════════════════════════════════════════════════════════════════╝\n"))
cat("\n")

log_msg("Pipeline execution completed successfully!")
log_msg("")
log_msg("═══════════════════════════════════════════════════════════════════════════")
log_msg("TIMING SUMMARY")
log_msg("═══════════════════════════════════════════════════════════════════════════")
log_msg(sprintf("Stage 1 (MPU Index):        %6.1f %s", as.numeric(stage1_time), attr(stage1_time, "units")))
log_msg(sprintf("Stage 2 (Data Prep):        %6.1f %s", as.numeric(stage2_time), attr(stage2_time, "units")))
log_msg(sprintf("Stage 3 (LLM Models):       %6.1f %s", as.numeric(stage3_time), attr(stage3_time, "units")))
log_msg(sprintf("Stage 4 (Main Analysis):    %6.1f %s", as.numeric(stage4_time), attr(stage4_time, "units")))
log_msg(sprintf("Stage 5 (Robustness):       %6.1f %s", as.numeric(stage5_time), attr(stage5_time, "units")))
log_msg(sprintf("Stage 6 (Out-of-Sample):    %6.1f %s", as.numeric(stage6_time), attr(stage6_time, "units")))
log_msg("───────────────────────────────────────────────────────────────────────────")
log_msg(sprintf("TOTAL PIPELINE TIME:        %6.1f %s", as.numeric(pipeline_time), attr(pipeline_time, "units")))
log_msg("═══════════════════════════════════════════════════════════════════════════")
log_msg("")
log_msg("✅ All results saved to:")
log_msg("   - ../intermediate_data/")
log_msg("   - ../output/figures/")
log_msg("   - ../output/tables/")
log_msg("")
log_msg(paste("📋 Full log saved to:", log_file))

cat("\n")
cat(crayon::green(paste("Total execution time:", round(pipeline_time, 2), attr(pipeline_time, "units"), "\n")))
cat(crayon::green(paste("Log file:", log_file, "\n")))
cat("\n")

#===============================================================================
# END OF PIPELINE
#===============================================================================
