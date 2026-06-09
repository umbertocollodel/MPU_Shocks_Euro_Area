#===============================================================================
# COMPLETE PIPELINE EXECUTION SCRIPT
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Usage:
#   source("00run_complete_pipeline.R")
#
# IMPORTANT:
#   - Ensure .Renviron exists with GEMINI_API_KEY
#   - Stage 3 (LLM ensemble) takes 10-20 hours
#   - Total estimated time: 15-25 hours
#   - Total estimated cost: $40-60 in API calls
#
#===============================================================================

pipeline_start <- Sys.time()

log_file <- paste0("pipeline_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
log_msg <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message  <- paste0("[", timestamp, "] ", msg)
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

log_msg("Checking prerequisites...")
if (!file.exists(".Renviron")) stop("❌ .Renviron not found. Create it with GEMINI_API_KEY.")
if (Sys.getenv("GEMINI_API_KEY") == "") stop("❌ GEMINI_API_KEY not set in .Renviron.")
log_msg("✅ Prerequisites checked")

#===============================================================================
# STAGE 1: BASELINE MPU INDEX — Paper 1 (5-10 minutes)
#===============================================================================
cat(crayon::blue("\n╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 1: BASELINE MPU INDEX                                               ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n\n"))

stage1_start <- Sys.time()

scripts_stage1 <- list(
  list(file = "01create_MPU.R",                           label = "01create_MPU"),
  list(file = "02plot_MPU_and_compare_with_MP_surprises.R",label = "02plot_MPU"),
  list(file = "03relationship_liquidity_mpu_testing.R",   label = "03liquidity_test"),
  list(file = "04appendix_exogeneity_tests_MPU.R",        label = "04exogeneity_tests"),
  list(file = "05mpu_all_days_vs_govc.R",                 label = "05mpu_all_days"),
  list(file = "06get_calendar_us_releases.R",             label = "06us_calendar")
)

for (s in scripts_stage1) {
  log_msg(sprintf("Running %s...", s$file))
  tryCatch({
    source(s$file)
    log_msg(sprintf("✅ %s completed", s$file))
  }, error = function(e) {
    log_msg(sprintf("❌ Error in %s: %s", s$file, e$message))
    stop(e)
  })
}

stage1_time <- difftime(Sys.time(), stage1_start, units = "mins")
log_msg(sprintf("Stage 1 completed in %.1f min", as.numeric(stage1_time)))

#===============================================================================
# STAGE 2: DATA PREPARATION — transcripts + complexity (30-60 minutes)
#===============================================================================
cat(crayon::blue("\n╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 2: DATA PREPARATION                                                 ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n\n"))

stage2_start <- Sys.time()

# Scraping is optional: skip if transcripts already downloaded
log_msg("Running 07scraping_ecb_pressconf.R...")
tryCatch({
  source("07scraping_ecb_pressconf.R")
  log_msg("✅ 07scraping_ecb_pressconf.R completed")
}, error = function(e) {
  log_msg(sprintf("⚠️  07scraping_ecb_pressconf.R skipped (transcripts may already exist): %s", e$message))
})

log_msg("Running 08calculate_complexity_documents.R...")
tryCatch({
  source("08calculate_complexity_documents.R")
  log_msg("✅ 08calculate_complexity_documents.R completed")
}, error = function(e) {
  log_msg(sprintf("❌ Error in 08calculate_complexity_documents.R: %s", e$message))
  stop(e)
})

stage2_time <- difftime(Sys.time(), stage2_start, units = "mins")
log_msg(sprintf("Stage 2 completed in %.1f min", as.numeric(stage2_time)))

#===============================================================================
# STAGE 3: RUN LLM ENSEMBLE — main results (10-20 hours, ~$30-50)
#===============================================================================
cat(crayon::blue("\n╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 3: RUN LLM ENSEMBLE                                                 ║\n"))
cat(crayon::blue("║  ⚠️  WARNING: 283 conferences × R=10 seeds = 2830 API calls               ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n\n"))

stage3_start <- Sys.time()

log_msg("Running 09run_full_ensemble_p1.R...")
tryCatch({
  source("09run_full_ensemble_p1.R")
  log_msg("✅ 09run_full_ensemble_p1.R completed")
}, error = function(e) {
  log_msg(sprintf("❌ Error in 09run_full_ensemble_p1.R: %s", e$message))
  stop(e)
})

stage3_time <- difftime(Sys.time(), stage3_start, units = "hours")
log_msg(sprintf("Stage 3 completed in %.1f hours", as.numeric(stage3_time)))

#===============================================================================
# STAGE 4: MAIN ANALYSIS — figures and regression (10 minutes)
#===============================================================================
cat(crayon::blue("\n╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 4: MAIN ANALYSIS                                                    ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n\n"))

stage4_start <- Sys.time()

for (s in list(
  list(file = "10post_zero_shot_p1.R",                  label = "10post_zero_shot"),
  list(file = "18regression_ois_on_synthetic_measures.R",label = "18regression_ois")
)) {
  log_msg(sprintf("Running %s...", s$file))
  tryCatch({
    source(s$file)
    log_msg(sprintf("✅ %s completed", s$file))
  }, error = function(e) {
    log_msg(sprintf("❌ Error in %s: %s", s$file, e$message))
    stop(e)
  })
}

stage4_time <- difftime(Sys.time(), stage4_start, units = "mins")
log_msg(sprintf("Stage 4 completed in %.1f min", as.numeric(stage4_time)))

#===============================================================================
# STAGE 5: ROBUSTNESS TESTS (1-3 hours)
#===============================================================================
cat(crayon::blue("\n╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::blue("║  STAGE 5: ROBUSTNESS TESTS                                                 ║\n"))
cat(crayon::blue("╚════════════════════════════════════════════════════════════════════════════╝\n\n"))

stage5_start <- Sys.time()

scripts_stage5 <- list(
  list(file = "11run_prompt_stability_robustness.R", warn_only = TRUE),
  list(file = "12run_model_stability_robustness.R",  warn_only = TRUE),
  list(file = "13run_real_oos_test.R",               warn_only = TRUE),
  list(file = "14run_endogeneity_p1.R",              warn_only = TRUE),
  list(file = "15post_endogeneity_p1.R",             warn_only = FALSE),
  list(file = "16run_fewshot_ensemble_p1.R",         warn_only = TRUE),
  list(file = "17post_fewshot_ensemble_p1.R",        warn_only = FALSE)
)

for (s in scripts_stage5) {
  log_msg(sprintf("Running %s...", s$file))
  tryCatch({
    source(s$file)
    log_msg(sprintf("✅ %s completed", s$file))
  }, error = function(e) {
    if (s$warn_only) {
      log_msg(sprintf("⚠️  Warning in %s: %s", s$file, e$message))
    } else {
      log_msg(sprintf("❌ Error in %s: %s", s$file, e$message))
      stop(e)
    }
  })
}

stage5_time <- difftime(Sys.time(), stage5_start, units = "mins")
log_msg(sprintf("Stage 5 completed in %.1f min", as.numeric(stage5_time)))

#===============================================================================
# PIPELINE SUMMARY
#===============================================================================
pipeline_time <- difftime(Sys.time(), pipeline_start, units = "hours")

cat(crayon::green("\n╔════════════════════════════════════════════════════════════════════════════╗\n"))
cat(crayon::green("║  PIPELINE EXECUTION COMPLETE                                               ║\n"))
cat(crayon::green("╚════════════════════════════════════════════════════════════════════════════╝\n\n"))

log_msg("Pipeline execution completed successfully!")
log_msg("═══════════════════════════════════════════════════════════════════════════")
log_msg("TIMING SUMMARY")
log_msg("═══════════════════════════════════════════════════════════════════════════")
log_msg(sprintf("Stage 1 (MPU Index):        %5.1f min",   as.numeric(stage1_time)))
log_msg(sprintf("Stage 2 (Data Prep):        %5.1f min",   as.numeric(stage2_time)))
log_msg(sprintf("Stage 3 (LLM Ensemble):     %5.1f hours", as.numeric(stage3_time)))
log_msg(sprintf("Stage 4 (Main Analysis):    %5.1f min",   as.numeric(stage4_time)))
log_msg(sprintf("Stage 5 (Robustness):       %5.1f min",   as.numeric(stage5_time)))
log_msg("───────────────────────────────────────────────────────────────────────────")
log_msg(sprintf("TOTAL PIPELINE TIME:        %5.1f hours", as.numeric(pipeline_time)))
log_msg("═══════════════════════════════════════════════════════════════════════════")
log_msg("✅ Results saved to ../intermediate_data/, ../output/figures/, ../output/tables/")
log_msg(paste("📋 Full log:", log_file))

cat(crayon::green(sprintf("Total execution time: %.1f hours\n", as.numeric(pipeline_time))))
cat(crayon::green(paste("Log file:", log_file, "\n\n")))
