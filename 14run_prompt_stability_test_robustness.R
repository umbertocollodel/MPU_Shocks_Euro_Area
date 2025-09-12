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
process_conference_variation <- function(params, log_file_path, seed = 120) {
  
  conf_date <- params$conf_date
  conf_text <- params$conf_text
  prompt_variation <- params$prompt_variation
  variation_id <- params$variation_id
  variation_type <- params$variation_type
  
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

# Subset for testing (use 30 RANDOM conferences for computational efficiency)
n_conferences_test <- 30
set.seed(42)  # Set seed for reproducibility
random_indices <- sample(1:length(conference_data$dates), n_conferences_test, replace = FALSE)

test_dates <- conference_data$dates[random_indices]
test_texts <- conference_data$texts[random_indices]

cat(paste0("Testing with ", n_conferences_test, " randomly selected conferences and ", N_TOTAL_VARIATIONS, " prompt variations\n"))
cat(paste0("Selected conference dates: ", paste(sort(test_dates), collapse = ", "), "\n"))
cat(paste0("Total API calls: ", n_conferences_test * N_TOTAL_VARIATIONS, "\n"))

# Set up log file
log_file <- paste0(OUTPUT_DIR, "robustness_errors_", Sys.Date(), ".log")
if (file.exists(log_file)) file.remove(log_file)

# Create parameter list for sequential processing
param_list <- expand_grid(
  conf_date = test_dates,
  variation_id = 1:N_TOTAL_VARIATIONS
) %>%
  mutate(
    conf_text = map_chr(conf_date, ~ test_texts[[which(test_dates == .x)]]),
    prompt_variation = map_chr(variation_id, ~ prompt_variations$variations[[.x]]),
    variation_type = map_chr(variation_id, ~ prompt_variations$metadata$variation_type[.x])
  ) %>%
# Convert to list of lists for map function
transpose()

start_time <- Sys.time()

# Run sequential processing
cat(crayon::blue("Starting sequential processing...\n"))

# Initialize progress counter
total_tasks <- length(param_list)
completed <- 0

results <- map_lgl(param_list, function(params) {
  # Update progress
  completed <<- completed + 1
  cat(crayon::cyan(paste0("Progress: ", completed, "/", total_tasks, " (",
                          round(completed/total_tasks * 100, 1), "%)\n")))
  
  # Process the task
  process_conference_variation(params, log_file_path = log_file, seed = 120)
})

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

return(list(
  results = results,
  param_list = param_list,
  prompt_variations = prompt_variations,
  success_rate = success_rate,
  total_time = total_time,
  selected_indices = random_indices,  # Save the random indices for reproducibility
  selected_dates = test_dates         # Save the selected dates for reference
))
}

# Clean and analyze robustness results: ----
clean_prompt_robustness_results <- function() {
  
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

  write_xlsx(all_results, path = paste0(OUTPUT_DIR, "df_prompt_robustness.xlsx"))
  
# Compute disagreement measures for each variation and conference
disagreement_measures <- all_results %>%
  group_by(conference_date, variation_id, variation_type, tenor) %>%
  summarise(
    std_rate = sd(rate, na.rm = TRUE),
    mean_rate = mean(rate, na.rm = TRUE),
    n_agents = n(),
    .groups = "drop"
  ) %>%
  filter(n_agents >= 20)  
}

# Main execution function: ----
main_robustness_analysis <- function() {
  
  cat(crayon::blue("PROMPT ROBUSTNESS ANALYSIS\n"))
  cat(crayon::blue("==========================\n\n"))
  
  # Run robustness analysis i.e. LLM calls
  cat("Running robustness analysis with prompt variations...\n")
  robustness_results <- run_robustness_analysis()
  
  if (robustness_results$success_rate < 0.5) {
    cat(crayon::yellow("Warning: Low success rate. Consider checking API limits or connection.\n"))
  }
  
  return(llm_results)
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

llm_results <- main_robustness_analysis()
final_results <- clean_prompt_robustness_results()


# ============================================================================
# SIMPLE ICC ANALYSIS WITH CLEAN VISUALIZATION
# ============================================================================
if (is.null(final_results)) {
  stop("No results available for ICC analysis. Please run the robustness analysis first.")
}


# Simple ICC calculation function
calculate_icc <- function(data) {
  conference_means <- data %>%
    group_by(conference_date) %>%
    summarise(conf_mean = mean(std_rate, na.rm = TRUE), .groups = "drop")
  
  within_variances <- data %>%
    group_by(conference_date) %>%
    summarise(within_var = var(std_rate, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(within_var), within_var > 0)
  
  var_between <- var(conference_means$conf_mean, na.rm = TRUE)
  var_within <- mean(within_variances$within_var, na.rm = TRUE)
  icc <- var_between / (var_between + var_within)
  
  return(round(icc, 3))
}

# Calculate ICC by tenor and variation type
icc_results <- final_results %>%
  group_by(tenor, variation_type) %>%
  group_split() %>%
  map_dfr(function(data) {
    if(nrow(data) < 15) return(NULL)  # Skip if too few observations
    
    tibble(
      tenor = data$tenor[1],
      variation_type = data$variation_type[1],
      icc = calculate_icc(data),
      n_obs = nrow(data)
    )
  }) %>%
  filter(!is.na(icc))

# Display results table
cat("=== ICC RESULTS ===\n")
print(icc_results %>% arrange(desc(icc)))

# Summary by variation type
summary_variation <- icc_results %>%
  group_by(variation_type) %>%
  summarise(avg_icc = round(mean(icc), 3), .groups = "drop") %>%
  arrange(desc(avg_icc))

cat("\nAverage ICC by Variation Type:\n")
print(summary_variation)

# Summary by tenor
summary_tenor <- icc_results %>%
  group_by(tenor) %>%
  summarise(avg_icc = round(mean(icc), 3), .groups = "drop") %>%
  arrange(desc(avg_icc))

cat("\nAverage ICC by Tenor:\n")
print(summary_tenor)

# Color palette (consistent with your previous code)
color_palette <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")


# Visualization 1: Bar chart by variation type
p1 <- ggplot(summary_variation, aes(x = reorder(variation_type, avg_icc), y = avg_icc, fill = variation_type)) +
  geom_col(width = 0.6, alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", avg_icc)), 
            hjust = -0.1, size = 4, fontface = "bold") +
  geom_hline(yintercept = c(0.5, 0.7), linetype = "dashed", alpha = 0.6) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(summary_variation$avg_icc) * 1.1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Average ICC by Variation Type",
    subtitle = "Dashed lines: 0.5 (minimum), 0.7 (good)",
    x = "Variation Type",
    y = "Average ICC"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Visualization 2: Bar chart by tenor
p2 <- ggplot(summary_tenor, aes(x = factor(tenor, levels = c("3M", "2Y", "10Y")), y = avg_icc, fill = tenor)) +
  geom_col(width = 0.6, alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", avg_icc)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  geom_hline(yintercept = c(0.5, 0.7), linetype = "dashed", alpha = 0.6) +
  scale_y_continuous(limits = c(0, max(summary_tenor$avg_icc) * 1.1)) +
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Average ICC by Tenor",
    subtitle = "Dashed lines: 0.5 (minimum), 0.7 (good)",
    x = "OIS Tenor",
    y = "Average ICC"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey40"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

# Display plots
print(p1)
print(p2) 
print(p3)

# Simple interpretation
cat("\n=== INTERPRETATION ===\n")
best_variation <- summary_variation$variation_type[1]
best_tenor <- summary_tenor$tenor[1]
best_combo <- icc_results %>% slice_max(icc, n = 1)

cat(sprintf("🏆 BEST VARIATION TYPE: %s (avg ICC = %.3f)\n", best_variation, summary_variation$avg_icc[1]))
cat(sprintf("🏆 BEST TENOR: %s (avg ICC = %.3f)\n", best_tenor, summary_tenor$avg_icc[1]))
cat(sprintf("🏆 BEST COMBINATION: %s %s (ICC = %.3f)\n", best_combo$tenor, best_combo$variation_type, best_combo$icc))

overall_avg <- round(mean(icc_results$icc), 3)
cat(sprintf("\n📊 OVERALL AVERAGE ICC: %.3f\n", overall_avg))

if(overall_avg >= 0.7) {
  cat("✅ EXCELLENT: Your method is highly reliable!\n")
} else if(overall_avg >= 0.5) {
  cat("📈 GOOD: Your method captures more signal than noise\n")
} else {
  cat("⚠️  NEEDS IMPROVEMENT: Consider refining your approach\n")
}

