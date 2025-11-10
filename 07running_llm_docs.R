#===============================================================================
# SCRIPT: Running LLM Analysis on ECB Press Conferences (Legacy Wrapper)
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
# Last Modified: November 2025
#
# IMPORTANT: This script is now a wrapper around the unified model runner.
#            For new analyses, please use: src/run_model.R
#
# Purpose:
#   Backward-compatible entry point for running LLM models.
#   This script maintains the original interface while delegating to the
#   new unified system in src/run_model.R
#
# Usage:
#   1. Set the model you want to run by editing MODEL_TO_RUN below
#   2. Run this script: source("07running_llm_docs.R")
#
# Configuration:
#   Edit config/model_config.yaml for detailed model parameters
#
# Available Models:
#   - "naive"               : Basic prompt without historical context
#   - "historical_surprise" : Includes past 3 conferences' volatility
#   - "llm_as_judge"        : Meta-learning with prompt optimization
#===============================================================================


# Configuration ----------------------------------------------------------------

# SET THE MODEL YOU WANT TO RUN HERE:
MODEL_TO_RUN <- "naive"  # Options: "naive", "historical_surprise", "llm_as_judge"


# Load Unified System ----------------------------------------------------------

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║  ECB Communication Analysis - Unified Model Runner                         ║\n")
cat("║  Legacy Entry Point (07running_llm_docs.R)                                 ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")
cat("ℹ️  This script now uses the unified model system.\n")
cat("   For direct access, use: source('src/run_model.R')\n\n")


# Verify model selection
valid_models <- c("naive", "historical_surprise", "llm_as_judge")
if (!MODEL_TO_RUN %in% valid_models) {
  stop(paste0("Invalid MODEL_TO_RUN: ", MODEL_TO_RUN,
              "\n   Valid options: ", paste(valid_models, collapse = ", ")))
}

cat(paste0("📋 Selected model: ", MODEL_TO_RUN, "\n\n"))


# Run Model --------------------------------------------------------------------

source("src/run_model.R")
run_model(model_name = MODEL_TO_RUN)


# Done -------------------------------------------------------------------------

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║  Execution Complete                                                        ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
