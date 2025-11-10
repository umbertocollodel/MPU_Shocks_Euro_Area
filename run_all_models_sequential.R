#===============================================================================
# RUN ALL MODELS SEQUENTIALLY
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Convenience script to run all three model variants sequentially.
#   Each model completes fully before the next one starts.
#
# Usage:
#   source("run_all_models_sequential.R")
#
# Total estimated time: 10-20 hours
#===============================================================================

# Load unified runner
source("src/run_model.R")

# Track overall timing
overall_start <- Sys.time()

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║  SEQUENTIAL MODEL EXECUTION                                                ║\n")
cat("║  Running all three model variants in sequence                              ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Model 1: Naive ---------------------------------------------------------------
cat(crayon::blue("\n[1/3] Starting NAIVE model...\n"))
model1_start <- Sys.time()

tryCatch({
  run_model(model_name = "naive")
  model1_end <- Sys.time()
  model1_time <- model1_end - model1_start
  cat(crayon::green(paste0("\n✅ NAIVE model completed in ",
                          round(model1_time, 2), " ",
                          attr(model1_time, "units"), "\n")))
}, error = function(e) {
  cat(crayon::red(paste0("\n❌ NAIVE model failed: ", e$message, "\n")))
  stop("Stopping sequential execution due to error in NAIVE model")
})

# Model 2: Historical Surprise -------------------------------------------------
cat(crayon::blue("\n[2/3] Starting HISTORICAL SURPRISE model...\n"))
model2_start <- Sys.time()

tryCatch({
  run_model(model_name = "historical_surprise")
  model2_end <- Sys.time()
  model2_time <- model2_end - model2_start
  cat(crayon::green(paste0("\n✅ HISTORICAL SURPRISE model completed in ",
                          round(model2_time, 2), " ",
                          attr(model2_time, "units"), "\n")))
}, error = function(e) {
  cat(crayon::red(paste0("\n❌ HISTORICAL SURPRISE model failed: ", e$message, "\n")))
  stop("Stopping sequential execution due to error in HISTORICAL SURPRISE model")
})

# Model 3: LLM-as-Judge --------------------------------------------------------
cat(crayon::blue("\n[3/3] Starting LLM-AS-JUDGE model...\n"))
cat(crayon::yellow("⚠️  This may take 6-12 hours...\n"))
model3_start <- Sys.time()

tryCatch({
  run_model(model_name = "llm_as_judge")
  model3_end <- Sys.time()
  model3_time <- model3_end - model3_start
  cat(crayon::green(paste0("\n✅ LLM-AS-JUDGE model completed in ",
                          round(model3_time, 2), " ",
                          attr(model3_time, "units"), "\n")))
}, error = function(e) {
  cat(crayon::red(paste0("\n❌ LLM-AS-JUDGE model failed: ", e$message, "\n")))
  cat(crayon::yellow("Note: First two models completed successfully.\n"))
})

# Summary ----------------------------------------------------------------------
overall_end <- Sys.time()
overall_time <- overall_end - overall_start

cat("\n")
cat(crayon::blue(strrep("=", 80), "\n"))
cat(crayon::blue("SEQUENTIAL EXECUTION COMPLETE\n"))
cat(crayon::blue(strrep("=", 80), "\n\n"))
cat("Total execution time: ", round(overall_time, 2), " ",
    attr(overall_time, "units"), "\n")
cat("\nModel timings:\n")
cat(paste0("  1. Naive:              ", round(model1_time, 2), " ",
          attr(model1_time, "units"), "\n"))
cat(paste0("  2. Historical Surprise: ", round(model2_time, 2), " ",
          attr(model2_time, "units"), "\n"))
if (exists("model3_time")) {
  cat(paste0("  3. LLM-as-Judge:        ", round(model3_time, 2), " ",
            attr(model3_time, "units"), "\n"))
}
cat("\n")
cat(crayon::green("✅ All results saved to ../intermediate_data/\n"))
cat(crayon::blue(strrep("=", 80), "\n\n"))

#===============================================================================
# END OF SCRIPT
#===============================================================================
