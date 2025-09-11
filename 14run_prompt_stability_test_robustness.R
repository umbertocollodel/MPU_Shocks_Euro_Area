# ==============================================================================
# SCRIPT: Prompt Robustness Analysis - Minor and Medium Variations
# ==============================================================================
# This script tests the reliability of LLM outputs across different prompt 
# formulations, following the measurement reliability framework for LLM research.
# We implement 10 minor and 5 medium variations of the naive prompt to assess
# signal vs noise in our monetary policy uncertainty measurements.

# Load necessary libraries and set parameters: -----
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  gemini.R,
  cli,
  httr2,
  readtext,
  crayon,
  stringr,
  purrr,
  readr,
  writexl,
  scales,
  showtext,
  readxl,
  tidyverse,
  future,
  furrr,
  lubridate,
  boot,
  RColorBrewer
)

# Set working directory and API key: ----
setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")
setAPI(Sys.getenv("GEMINI_API_KEY"))

# Configuration parameters: ----
N_MINOR_VARIATIONS <- 10
N_MEDIUM_VARIATIONS <- 5
N_TOTAL_VARIATIONS <- N_MINOR_VARIATIONS + N_MEDIUM_VARIATIONS
MAX_WORKERS <- 6  # Adjust based on your system and API limits
OUTPUT_DIR <- "../intermediate_data/robustness_analysis/"
FIGURES_DIR <- "../output/figures/robustness/"

# Create output directories: ----
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURES_DIR, recursive = TRUE, showWarnings = FALSE)

# Define the base naive prompt: ----
source("create_prompts.R")
base_prompt <- prompt_naive

# Enhanced Gemini function with better error handling: ----
new_gemini_robust <- function(prompt, model = "2.5-flash", temperature = 1, 
                              maxOutputTokens = 1000000, topK = 40, topP = 0.95, 
                              seed = 1234, max_retries = 3) {
  
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")
  
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )
  
  request_body <- list(
    contents = list(parts = list(list(text = prompt))),
    generationConfig = generation_config
  )
  
  for (attempt in 1:max_retries) {
    tryCatch({
      req <- request(url) |>
        req_url_query(key = api_key) |>
        req_headers("Content-Type" = "application/json") |>
        req_body_json(request_body) |>
        req_timeout(120)
      
      resp <- req_perform(req)
      
      if (resp$status_code != 200) {
        stop(paste0("HTTP Error: ", resp$status_code))
      }
      
      candidates <- resp_body_json(resp)$candidates
      outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
      return(outputs)
      
    }, error = function(e) {
      if (attempt == max_retries) {
        stop(paste0("Failed after ", max_retries, " attempts: ", e$message))
      }
      Sys.sleep(2 ^ attempt)  # Exponential backoff
    })
  }
}

# Generate prompt variations: ----
generate_prompt_variations <- function(base_prompt) {
  
  # Minor variations (10): Small word substitutions and reordering
  minor_variations <- list(
    # 1. Substitute synonyms in trader description
    str_replace_all(base_prompt, "individual traders", "distinct market participants"),
    
    # 2. Change decision-making terminology
    str_replace_all(base_prompt, "trading decision", "investment choice"),
    
    # 3. Reorder risk aversion levels
    str_replace_all(base_prompt, "High / Medium / Low", "Low / Medium / High"),
    
    # 4. Substitute interpretation terminology
    str_replace_all(base_prompt, "interpretation of the conference", "analysis of the meeting"),
    
    # 5. Change output formatting slightly
    str_replace_all(base_prompt, "markdown table", "structured table format"),
    
    # 6. Substitute confidence terminology
    str_replace_all(base_prompt, "confidence score", "certainty level"),
    
    # 7. Change monetary policy terminology
    str_replace_all(base_prompt, "monetary policy decisions", "interest rate policies"),
    
    # 8. Substitute profit terminology
    str_replace_all(base_prompt, "maximize profit", "optimize returns"),
    
    # 9. Change bias examples order
    str_replace_all(base_prompt, "Confirmation Bias, Overconfidence, Anchoring", "Anchoring, Confirmation Bias, Overconfidence"),
    
    # 10. Substitute meeting terminology
    str_replace_all(base_prompt, "press conference", "policy meeting")
  )
  
  # Medium variations (5): Substantial language and structure changes
  medium_variations <- list(
    # 1. More academic/formal language
    str_replace_all(base_prompt, 
                   "You are simulating the Euro area interest rate swap market, composed of 30 individual traders.",
                   "Your task involves modeling Euro area interest rate swap market dynamics through 30 heterogeneous market agents.") |>
      str_replace_all("Each trader then makes a trading decision", "Each agent subsequently formulates an investment decision"),
    
    # 2. Behavioral finance emphasis
    str_replace_all(base_prompt,
                   "These traders interpret the ECB Governing Council press conference",
                   "These market participants process ECB Governing Council communication through their individual behavioral lenses") |>
      str_replace_all("unique characteristics", "distinct psychological profiles"),
    
    # 3. Risk management focus
    str_replace_all(base_prompt,
                   "to maximize profit based on their interpretation",
                   "to optimize risk-adjusted returns considering their analytical framework") |>
      str_replace_all("Risk Aversion: High / Medium / Low", "Risk Tolerance: Conservative / Moderate / Aggressive"),
    
    # 4. Information processing emphasis
    str_replace_all(base_prompt,
                   "communicates monetary policy decisions, economic assessments",
                   "transmits policy signals, economic outlook, and forward guidance") |>
      str_replace_all("Interpretation Style", "Information Processing Approach"),
    
    # 5. Market microstructure focus
    str_replace_all(base_prompt,
                   "simulate their individual trading action in the interest rate swap market",
                   "model their position-taking behavior in the Euro OIS market") |>
      str_replace_all("Expected Direction", "Rate Trajectory") |>
      str_replace_all("New Expected Rate", "Target Rate Level")
  )
  
  # Combine all variations
  all_variations <- c(minor_variations, medium_variations)
  
  # Add variation metadata
  variation_metadata <- data.frame(
    variation_id = 1:N_TOTAL_VARIATIONS,
    variation_type = c(rep("minor", N_MINOR_VARIATIONS), rep("medium", N_MEDIUM_VARIATIONS)),
    stringsAsFactors = FALSE
  )
  
  return(list(
    variations = all_variations,
    metadata = variation_metadata
  ))
}

# Load ECB press conference data: ----
load_conference_data <- function() {
  dates_ecb <- list.files("../intermediate_data/texts/") %>%
    str_subset("\\d") %>%
    str_remove("\\.txt") %>%
    str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
    sort()
  
  names_ecb <- list.files("../intermediate_data/texts/") %>%
    str_subset("\\d") %>%
    str_remove("\\.txt") %>%
    sort()
  
  ecb_texts <- list.files("../intermediate_data/texts/") %>%
    str_subset("\\d") %>%
    paste0("../intermediate_data/texts/", .) %>%
    map(~ readtext(.x)) %>%
    map(~ .$text) %>%
    set_names(names_ecb) %>%
    .[names_ecb]
  
  return(list(
    dates = dates_ecb,
    names = names_ecb,
    texts = ecb_texts
  ))
}

# Process single conference with specific prompt variation: ----
process_conference_variation <- function(conf_date, conf_text, prompt_variation, 
                                       variation_id, variation_type, 
                                       log_file_path, seed = 120) {
  
  cat(crayon::yellow(paste0("🔄 Processing ", conf_date, " with variation ", variation_id, " (", variation_type, ")\n")))
  
  # Construct full prompt
  full_prompt <- gsub("\\[date\\]", conf_date, prompt_variation)
  full_prompt <- paste0(
    full_prompt,
    "Press Conference on ", conf_date, "\n",
    "Text:", conf_text, "\n\n"
  )
  
  tryCatch({
    result <- new_gemini_robust(full_prompt, seed = seed, temperature = 1)
    
    # Save result with variation metadata
    output_filename <- paste0(OUTPUT_DIR, "variation_", variation_id, "_", conf_date, ".rds")
    saveRDS(list(
      result = result,
      variation_id = variation_id,
      variation_type = variation_type,
      conference_date = conf_date
    ), file = output_filename)
    
    cat(crayon::green(paste0("✅ ", conf_date, " variation ", variation_id, " completed\n")))
    return(TRUE)
    
  }, error = function(e) {
    error_msg <- paste0(conf_date, " variation ", variation_id, ": ", e$message)
    cat(crayon::red(paste0("❌ ", error_msg, "\n")))
    write(error_msg, file = log_file_path, append = TRUE)
    return(FALSE)
  })
}

# Main robustness analysis function: ----
run_robustness_analysis <- function() {
  
  cat(crayon::blue("Starting Prompt Robustness Analysis\n"))
  cat(crayon::blue("=====================================\n"))
  
  # Generate prompt variations
  cat("Generating prompt variations...\n")
  prompt_variations <- generate_prompt_variations(base_prompt)
  
  # Load conference data
  cat("Loading ECB conference data...\n")
  conference_data <- load_conference_data()
  
  # Subset for testing (use first 20 conferences for computational efficiency)
  n_conferences_test <- 30
  test_dates <- conference_data$dates[1:n_conferences_test]
  test_texts <- conference_data$texts[1:n_conferences_test]
  
  cat(paste0("Testing with ", n_conferences_test, " conferences and ", N_TOTAL_VARIATIONS, " prompt variations\n"))
  cat(paste0("Total API calls: ", n_conferences_test * N_TOTAL_VARIATIONS, "\n"))
  
  # Set up parallel processing
  log_file <- paste0(OUTPUT_DIR, "robustness_errors_", Sys.Date(), ".log")
  if (file.exists(log_file)) file.remove(log_file)
  
  plan(multisession, workers = MAX_WORKERS)
  
  # Create parameter grid for parallel processing
  param_grid <- expand_grid(
    conf_date = test_dates,
    variation_id = 1:N_TOTAL_VARIATIONS
  ) %>%
    mutate(
      conf_text = map_chr(conf_date, ~ test_texts[[which(test_dates == .x)]]),
      prompt_variation = map_chr(variation_id, ~ prompt_variations$variations[[.x]]),
      variation_type = map_chr(variation_id, ~ prompt_variations$metadata$variation_type[.x])
    )
  
  start_time <- Sys.time()
  
  # Run parallel processing
  cat(crayon::blue("Starting parallel processing...\n"))
  
  results <- future_pmap_lgl(
    list(
      param_grid$conf_date,
      param_grid$conf_text,
      param_grid$prompt_variation,
      param_grid$variation_id,
      param_grid$variation_type
    ),
    process_conference_variation,
    log_file_path = log_file,
    seed = 120,
    .options = furrr_options(seed = TRUE)
  )
  
  end_time <- Sys.time()
  total_time <- end_time - start_time
  
  # Report results
  successful_runs <- sum(results)
  total_runs <- length(results)
  success_rate <- successful_runs / total_runs
  
  cat(crayon::blue("\nRobustness Analysis Complete\n"))
  cat(crayon::blue("============================\n"))
  cat(paste0("Successful runs: ", successful_runs, "/", total_runs, " (", round(success_rate * 100, 1), "%)\n"))
  cat(paste0("Total time: ", round(total_time, 2), " ", units(total_time), "\n"))
  
  plan(sequential)
  
  return(list(
    results = results,
    param_grid = param_grid,
    prompt_variations = prompt_variations,
    success_rate = success_rate,
    total_time = total_time
  ))
}

# Clean and analyze robustness results: ----
analyze_robustness_results <- function() {
  
  cat(crayon::blue("Analyzing robustness results...\n"))
  
  # Load all result files
  result_files <- list.files(OUTPUT_DIR, pattern = "variation_.*\\.rds", full.names = TRUE)
  
  if (length(result_files) == 0) {
    cat(crayon::red("No result files found. Run robustness analysis first.\n"))
    return(NULL)
  }
  
  # Process results
  cat("Processing LLM outputs...\n")
  
  # Column names for parsing
  names_col <- c("date", "id", "tenor", "direction", "rate", "confidence")
  
  # Load and parse all results
  all_results <- map_dfr(result_files, function(file) {
    
    tryCatch({
      data <- readRDS(file)
      
      # Parse LLM output
      parsed_result <- data$result %>%
        readr::read_delim(delim = "|", trim_ws = TRUE, skip = 1, 
                         show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
        select(-1, -ncol(.)) %>%  # Remove first and last columns (usually empty)
        slice(-nrow(.))  # Remove last row (usually empty)
      
      # Clean and standardize
      if (ncol(parsed_result) == length(names_col)) {
        names(parsed_result) <- names_col
        
        parsed_result <- parsed_result %>%
          slice(-1) %>%  # Remove header row if still present
          mutate(
            variation_id = data$variation_id,
            variation_type = data$variation_type,
            conference_date = data$conference_date,
            date = as.character(date),
            rate = as.numeric(rate),
            confidence = as.numeric(confidence)
          ) %>%
          filter(
            tenor %in% c("3M", "2Y", "10Y"),
            !is.na(rate)
          )
        
        return(parsed_result)
      } else {
        cat(crayon::yellow(paste0("Skipping file with wrong number of columns: ", basename(file), "\n")))
        return(NULL)
      }
      
    }, error = function(e) {
      cat(crayon::red(paste0("Error processing ", basename(file), ": ", e$message, "\n")))
      return(NULL)
    })
  })
  
  if (nrow(all_results) == 0) {
    cat(crayon::red("No valid results to analyze.\n"))
    return(NULL)
  }
  
  cat(paste0("Successfully processed ", nrow(all_results), " individual predictions\n"))
  
  # Compute disagreement measures for each variation and conference
  disagreement_measures <- all_results %>%
    group_by(conference_date, variation_id, variation_type, tenor) %>%
    summarise(
      std_rate = sd(rate, na.rm = TRUE),
      mean_rate = mean(rate, na.rm = TRUE),
      n_agents = n(),
      .groups = "drop"
    ) %>%
    filter(n_agents >= 20)  # Ensure sufficient observations
  
  # Compute reliability statistics following the framework
  reliability_stats <- disagreement_measures %>%
    group_by(conference_date, tenor) %>%
    summarise(
      mean_std = mean(std_rate, na.rm = TRUE),          # η̄_d (signal)
      var_across_variations = var(std_rate, na.rm = TRUE),  # σ²_ε (noise across variations)
      var_signal = var(mean_std, na.rm = TRUE),             # σ²_η (signal variance)
      reliability_ratio = var_signal / (var_signal + var_across_variations),  # R_d
      n_variations = n(),
      .groups = "drop"
    ) %>%
    mutate(
      # Handle edge cases
      reliability_ratio = ifelse(is.na(reliability_ratio) | is.infinite(reliability_ratio), 
                                 0, reliability_ratio),
      reliability_ratio = pmax(0, pmin(1, reliability_ratio))  # Bound between 0 and 1
    )
  
  # Overall reliability by tenor
  overall_reliability <- reliability_stats %>%
    group_by(tenor) %>%
    summarise(
      mean_reliability = mean(reliability_ratio, na.rm = TRUE),
      median_reliability = median(reliability_ratio, na.rm = TRUE),
      sd_reliability = sd(reliability_ratio, na.rm = TRUE),
      min_reliability = min(reliability_ratio, na.rm = TRUE),
      max_reliability = max(reliability_ratio, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat(crayon::green("Reliability analysis complete!\n"))
  
  return(list(
    all_results = all_results,
    disagreement_measures = disagreement_measures,
    reliability_stats = reliability_stats,
    overall_reliability = overall_reliability
  ))
}

# Generate robustness visualizations: ----
create_robustness_plots <- function(analysis_results) {
  
  if (is.null(analysis_results)) {
    cat(crayon::red("No analysis results to plot.\n"))
    return(NULL)
  }
  
  cat(crayon::blue("Creating robustness visualizations...\n"))
  
  # Color palette
  colors_tenor <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")
  colors_variation <- c("minor" = "#2166ac", "medium" = "#762a83")
  
  # Plot 1: Standard deviation by variation type and tenor
  p1 <- analysis_results$disagreement_measures %>%
    ggplot(aes(x = variation_type, y = std_rate, fill = variation_type)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~ tenor, scales = "free_y", nrow = 1) +
    scale_fill_manual(values = colors_variation) +
    labs(
      title = "LLM Response Disagreement Across Prompt Variations",
      subtitle = "Distribution of standard deviations by variation type and tenor",
      x = "Prompt Variation Type",
      y = "Standard Deviation of Predicted Rates",
      fill = "Variation Type"
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "grey30"),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 10),
      legend.position = "none"
    )
  
  ggsave(file.path(FIGURES_DIR, "disagreement_by_variation.pdf"), 
         plot = p1, width = 12, height = 6, dpi = 320, bg = "white")
  
  # Plot 2: Reliability ratios by tenor
  p2 <- analysis_results$overall_reliability %>%
    mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) %>%
    ggplot(aes(x = tenor, y = mean_reliability, fill = tenor)) +
    geom_col(width = 0.6, alpha = 0.8) +
    geom_errorbar(aes(ymin = mean_reliability - sd_reliability, 
                      ymax = mean_reliability + sd_reliability),
                  width = 0.3, alpha = 0.8) +
    geom_text(aes(label = sprintf("%.3f", mean_reliability)), 
              vjust = -0.5, size = 4, fontface = "bold") +
    scale_fill_manual(values = colors_tenor) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title = "Measurement Reliability by Tenor",
      subtitle = "Higher values indicate more signal relative to prompt-induced noise",
      x = "OIS Tenor",
      y = "Reliability Ratio (R_d)",
      caption = "Error bars show ±1 standard deviation. Values closer to 1 indicate higher reliability."
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "grey30"),
      plot.caption = element_text(size = 10, color = "grey50", hjust = 0),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "none"
    )
  
  ggsave(file.path(FIGURES_DIR, "reliability_ratios.pdf"), 
         plot = p2, width = 8, height = 6, dpi = 320, bg = "white")
  
  # Plot 3: Variation in disagreement across prompt types
  p3 <- analysis_results$disagreement_measures %>%
    group_by(conference_date, tenor, variation_type) %>%
    summarise(mean_std = mean(std_rate, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = variation_type, values_from = mean_std) %>%
    ggplot(aes(x = minor, y = medium, color = tenor)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = colors_tenor) +
    facet_wrap(~ tenor, scales = "free") +
    labs(
      title = "Consistency Between Minor and Medium Prompt Variations",
      subtitle = "Points near the diagonal line indicate consistent responses across variation types",
      x = "Mean Disagreement - Minor Variations",
      y = "Mean Disagreement - Medium Variations",
      color = "Tenor"
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "grey30"),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )
  
  ggsave(file.path(FIGURES_DIR, "variation_consistency.pdf"), 
         plot = p3, width = 10, height = 8, dpi = 320, bg = "white")
  
  cat(crayon::green("Robustness plots saved to ", FIGURES_DIR, "\n"))
  
  return(list(p1 = p1, p2 = p2, p3 = p3))
}

# Export comprehensive results: ----
export_robustness_results <- function(analysis_results, prompt_variations) {
  
  if (is.null(analysis_results)) {
    cat(crayon::red("No results to export.\n"))
    return(NULL)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- file.path(OUTPUT_DIR, paste0("robustness_analysis_results_", timestamp, ".xlsx"))
  
  # Prepare export data
  export_list <- list(
    "Overall_Reliability" = analysis_results$overall_reliability,
    "Reliability_by_Conference" = analysis_results$reliability_stats,
    "Disagreement_Measures" = analysis_results$disagreement_measures,
    "Prompt_Variations_Metadata" = prompt_variations$metadata,
    "Summary_Statistics" = analysis_results$disagreement_measures %>%
      group_by(variation_type, tenor) %>%
      summarise(
        mean_disagreement = mean(std_rate, na.rm = TRUE),
        sd_disagreement = sd(std_rate, na.rm = TRUE),
        median_disagreement = median(std_rate, na.rm = TRUE),
        n_observations = n(),
        .groups = "drop"
      )
  )
  
  # Export to Excel
  writexl::write_xlsx(export_list, output_file)
  
  cat(crayon::green("Results exported to: ", output_file, "\n"))
  
  return(output_file)
}

# Main execution function: ----
main_robustness_analysis <- function() {
  
  cat(crayon::blue("PROMPT ROBUSTNESS ANALYSIS\n"))
  cat(crayon::blue("==========================\n\n"))
  
  # Step 1: Run robustness analysis
  cat("Step 1: Running robustness analysis with prompt variations...\n")
  robustness_results <- run_robustness_analysis()
  
  if (robustness_results$success_rate < 0.5) {
    cat(crayon::yellow("Warning: Low success rate. Consider reducing parallelization or checking API limits.\n"))
  }
  
  # Step 2: Analyze results
  cat("\nStep 2: Analyzing results...\n")
  analysis_results <- analyze_robustness_results()
  
  if (is.null(analysis_results)) {
    cat(crayon::red("Analysis failed. Check error logs.\n"))
    return(NULL)
  }
  
  # Step 3: Create visualizations
  cat("\nStep 3: Creating visualizations...\n")
  plots <- create_robustness_plots(analysis_results)
  
  # Step 4: Export results
  cat("\nStep 4: Exporting results...\n")
  output_file <- export_robustness_results(analysis_results, robustness_results$prompt_variations)
  
  # Step 5: Print summary
  cat(crayon::green("\n=== ROBUSTNESS ANALYSIS SUMMARY ===\n"))
  cat("Sampling Strategy: ", SAMPLING_STRATEGY, "\n")
  cat("Sample Size: ", length(robustness_results$param_grid$conf_date[!duplicated(robustness_results$param_grid$conf_date)]), 
      " conferences\n")
  cat("Total Variations: ", N_TOTAL_VARIATIONS, " (", N_MINOR_VARIATIONS, " minor + ", N_MEDIUM_VARIATIONS, " medium)\n")
  cat("Success Rate: ", round(robustness_results$success_rate * 100, 1), "%\n\n")
  
  cat("Overall Reliability by Tenor:\n")
  print(analysis_results$overall_reliability)
  
  cat(crayon::green("\nAnalysis complete! Check the following locations:\n"))
  cat("- Raw results: ", OUTPUT_DIR, "\n")
  cat("- Figures: ", FIGURES_DIR, "\n")
  cat("- Comprehensive results: ", output_file, "\n")
  
  return(list(
    robustness_results = robustness_results,
    analysis_results = analysis_results,
    plots = plots,
    output_file = output_file
  ))
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Run the complete robustness analysis
if (interactive()) {
  cat("Ready to run robustness analysis. Execute: main_robustness_analysis()\n")
} else {
  # Uncomment the next line to run automatically
   final_results <- main_robustness_analysis()
}