# Text Scrambling for Data Leakage Detection
# Author: Based on your macroeconomics research style
# Purpose: Test whether LLM is actually reading text or using memorized patterns

library(tidyverse)
library(stringr)
library(readtext)
library(httr2)

# Load your conference data (adjust path as needed)
setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# Function to scramble sentences within a text
scramble_sentences <- function(text) {
  # Handle edge cases
  if (is.na(text) || nchar(text) == 0) return(text)
  
  # Split by sentence endings, keeping the punctuation
  sentences <- str_split(text, "(?<=[\\.\\!\\?])\\s+", simplify = FALSE)[[1]]
  sentences <- sentences[nchar(sentences) > 0]  # Remove empty strings
  
  if (length(sentences) <= 1) return(text)  # Can't scramble single sentence
  
  # Randomly reorder sentences
  scrambled_sentences <- sample(sentences)
  
  # Rejoin with spaces
  paste(scrambled_sentences, collapse = " ")
}

# Function to scramble words within each sentence
scramble_words_in_sentences <- function(text) {
  # Handle edge cases
  if (is.na(text) || nchar(text) == 0) return(text)
  
  # Split by sentences first
  sentences <- str_split(text, "(?<=[\\.\\!\\?])\\s+", simplify = FALSE)[[1]]
  sentences <- sentences[nchar(sentences) > 0]
  
  # Scramble words within each sentence
  scrambled_sentences <- map_chr(sentences, function(sentence) {
    # Remove punctuation temporarily, scramble words, then add back punctuation
    # Extract ending punctuation
    ending_punct <- str_extract(sentence, "[\\.\\!\\?]+$")
    if (is.na(ending_punct)) ending_punct <- ""
    
    # Remove ending punctuation for word scrambling
    clean_sentence <- str_remove(sentence, "[\\.\\!\\?]+$")
    
    # Split into words and scramble
    words <- str_split(clean_sentence, "\\s+", simplify = FALSE)[[1]]
    words <- words[nchar(words) > 0]  # Remove empty strings
    
    if (length(words) <= 1) return(sentence)  # Can't scramble single word
    
    scrambled_words <- sample(words)
    
    # Reconstruct with punctuation
    paste0(paste(scrambled_words, collapse = " "), ending_punct)
  })
  
  # Rejoin sentences
  paste(scrambled_sentences, collapse = " ")
}

# Strategic sampling function to balance cost vs. coverage
create_strategic_sample <- function(conference_dates, n_sample = 30, method = "strategic") {
  
  conference_df <- data.frame(
    date = as.Date(conference_dates),
    stringsAsFactors = FALSE
  ) %>%
    arrange(date) %>%
    mutate(
      year = year(date),
      # Create volatility periods based on known ECB history
      period = case_when(
        year >= 2022 ~ "Recent_Tightening",
        year >= 2015 & year <= 2021 ~ "ZLB_Era", 
        year >= 2010 & year <= 2014 ~ "Sovereign_Crisis",
        year >= 2008 & year <= 2009 ~ "Financial_Crisis",
        TRUE ~ "Pre_Crisis"
      )
    )
  
  if (method == "strategic") {
    # Stratified sampling across periods to ensure coverage of different regimes
    sampled_conferences <- conference_df %>%
      group_by(period) %>%
      # CORRECTED PART: Use group_modify to apply a custom sampling function to each group.
      # Inside the function, `.x` refers to the data for the current group,
      # so `nrow(.x)` correctly calculates the group size.
      group_modify(~ .x %>% slice_sample(
        n = max(2, round(n_sample * nrow(.x) / nrow(conference_df)))
      )) %>%
      ungroup() %>%
      slice_sample(n = min(n_sample, nrow(.)))  # Final cap at n_sample
    
  } else if (method == "recent") {
    # Focus on recent conferences (less likely to be in training data)
    sampled_conferences <- conference_df %>%
      filter(year >= 2020) %>%
      slice_sample(n = min(n_sample, nrow(.)))
    
  } else if (method == "high_volatility") {
    # Focus on periods of known high volatility (crisis periods)
    sampled_conferences <- conference_df %>%
      filter(period %in% c("Financial_Crisis", "Sovereign_Crisis", "Recent_Tightening")) %>%
      slice_sample(n = min(n_sample, nrow(.)))
    
  } else {
    # Random sampling
    sampled_conferences <- conference_df %>%
      slice_sample(n = min(n_sample, nrow(.)))
  }
  
  cat("Sampling Summary:\n")
  cat("Total conferences available:", nrow(conference_df), "\n")
  cat("Sample size:", nrow(sampled_conferences), "\n")
  cat("Coverage by period:\n")
  print(table(sampled_conferences$period))
  
  return(sampled_conferences$date)
}

# Main function to create scrambled versions of sampled conferences
create_scrambled_conferences <- function(sample_dates, scrambling_method = "both") {
  
  # Load original conference texts
  conference_files <- list.files("../intermediate_data/texts/", 
                                pattern = "\\d{4}-\\d{2}-\\d{2}", 
                                full.names = TRUE)
  
  # Filter to sample dates
  sample_date_strings <- as.character(sample_dates)
  sampled_files <- conference_files[str_detect(conference_files, 
                                              paste(sample_date_strings, collapse = "|"))]
  
  cat("Found", length(sampled_files), "conference files matching sample dates\n")
  
  # Process each conference
  results_list <- list()
  
  for (i in seq_along(sampled_files)) {
    file_path <- sampled_files[i]
    
    # Extract date from filename
    conf_date <- str_extract(basename(file_path), "\\d{4}-\\d{2}-\\d{2}")
    
    cat("Processing conference:", conf_date, "(", i, "of", length(sampled_files), ")\n")
    
    # Read original text
    original_text <- readtext(file_path)$text
    
    # Apply scrambling methods
    if (scrambling_method %in% c("both", "sentences")) {
      sentence_scrambled <- scramble_sentences(original_text)
      
      # Save scrambled version
      scrambled_filename <- paste0("../intermediate_data/texts_scrambled_sentences/", 
                                  conf_date, "_sentence_scrambled.txt")
      
      # Create directory if it doesn't exist
      dir.create("../intermediate_data/texts_scrambled_sentences/", 
                showWarnings = FALSE, recursive = TRUE)
      
      writeLines(sentence_scrambled, scrambled_filename)
    }
    
    if (scrambling_method %in% c("both", "words")) {
      word_scrambled <- scramble_words_in_sentences(original_text)
      
      # Save scrambled version  
      scrambled_filename <- paste0("../intermediate_data/texts_scrambled_words/", 
                                  conf_date, "_word_scrambled.txt")
      
      # Create directory if it doesn't exist
      dir.create("../intermediate_data/texts_scrambled_words/", 
                showWarnings = FALSE, recursive = TRUE)
      
      writeLines(word_scrambled, scrambled_filename)
    }
    
    # Store results for comparison
    results_list[[i]] <- data.frame(
      date = conf_date,
      original_length = nchar(original_text),
      original_sentences = length(str_split(original_text, "[\\.\\!\\?]")[[1]]),
      scrambled_available = TRUE,
      stringsAsFactors = FALSE
    )
  }
  
  # Combine results
  results_df <- bind_rows(results_list)
  
  cat("\nScrambling complete!\n")
  cat("Processed", nrow(results_df), "conferences\n")
  cat("Average text length:", mean(results_df$original_length), "characters\n")
  cat("Average sentences per conference:", mean(results_df$original_sentences), "\n")
  
  return(results_df)
}

# Example usage:

# 1. Get your conference dates (adjust based on your data structure)
dates_ecb_presconf <- list.files("../intermediate_data/texts/") %>%
  str_subset("\\d") %>%
  str_remove("\\.txt") %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
  as.Date() %>%
  sort()

# 2. Create strategic sample (recommended: 30 conferences for cost efficiency)
sample_dates <- create_strategic_sample(dates_ecb_presconf, 
                                       n_sample = 30, 
                                       method = "strategic")

# 3. Create scrambled versions
scrambling_results <- create_scrambled_conferences(sample_dates, 
                                                  scrambling_method = "both")

# 4. Verify scrambling worked by checking a sample
cat("\n=== VERIFICATION SAMPLE ===\n")
if (file.exists("../intermediate_data/texts_scrambled_sentences/")) {
  sample_files <- list.files("../intermediate_data/texts_scrambled_sentences/", full.names = TRUE)
  if (length(sample_files) > 0) {
    sample_text <- readLines(sample_files[1], n = 3)
    cat("First 3 lines of scrambled text:\n")
    cat(paste(sample_text, collapse = "\n"))
  }
}

cat("\n\n=== NEXT STEPS ===\n")
cat("1. Run your LLM analysis on both original and scrambled texts\n")
cat("2. Compare correlations: High correlation with scrambled = data leakage\n") 
cat("3. Sample directories created:\n")
cat("   - ../intermediate_data/texts_scrambled_sentences/\n")
cat("   - ../intermediate_data/texts_scrambled_words/\n")

# Export sample info for tracking
write_csv(scrambling_results, "../intermediate_data/scrambling_sample_info.csv")
cat("4. Sample info saved to: ../intermediate_data/scrambling_sample_info.csv\n")

# ==============================================================================
# PARALLEL LLM EXECUTION AND EVALUATION
# ==============================================================================

# Load additional libraries for LLM execution
library(future)
library(furrr)
library(readxl)
library(writexl)

# Source your existing LLM functions (adjust path as needed)
source("create_prompts.R")

# Function to run LLM analysis on a directory of conferences
run_llm_on_conference_directory <- function(conference_dir, 
                                           output_dir,
                                           experiment_name,
                                           prompt_template = prompt_naive) {
  
   # ✅ FIX: Add this block to clear previous results
  if (dir.exists(output_dir)) {
    cat(crayon::yellow(paste("⚠️ Deleting previous results in:", output_dir, "\n")))
    unlink(output_dir, recursive = TRUE)
  }
  
  # Create output directory
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Get conference files from directory
  conference_files <- list.files(conference_dir, 
                                pattern = "\\.txt$", 
                                full.names = TRUE)
  
  if (length(conference_files) == 0) {
    cat("No conference files found in", conference_dir, "\n")
    return(NULL)
  }
  
  # Extract dates and prepare data structure
  conference_data <- map_dfr(conference_files, function(file_path) {
    filename <- basename(file_path)
    conf_date <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
    
    if (is.na(conf_date)) {
      cat("Warning: Could not extract date from", filename, "\n")
      return(NULL)
    }
    
    # Read text
    text_content <- paste(readLines(file_path, warn = FALSE), collapse = " ")
    
    data.frame(
      date = conf_date,
      filename = filename,
      file_path = file_path,
      text = text_content,
      stringsAsFactors = FALSE
    )
  })
  
  cat("Processing", nrow(conference_data), "conferences for", experiment_name, "\n")
  
  # Set up parallel processing
  plan(multisession, workers = 4)
  
  # Process conferences in parallel
  results_parallel <- map2(
    conference_data$date,
    conference_data$text,
    ~ process_single_conference_scrambling(
      conf_date = .x,
      conf_text = .y,
      prompt_template = prompt_template,
      output_dir = output_dir,
      experiment_name = experiment_name
    ),
       .options = furrr_options(
        seed = TRUE,
        globals = c("new_gemini","process_single_conference_scrambling","prompt_naive"),  # Export your custom API function
        packages = "crayon,httr2"      # Make the crayon package available for logging
    )
  )
  
  # Clean up parallel workers
  plan(sequential)
  
  # Count successful runs
  successful_runs <- sum(unlist(results_parallel), na.rm = TRUE)
  cat("Successfully processed", successful_runs, "out of", nrow(conference_data), "conferences\n")
  
  return(list(
    total_conferences = nrow(conference_data),
    successful_runs = successful_runs,
    conference_dates = conference_data$date
  ))
}

# Modified processing function for scrambling experiments
process_single_conference_scrambling <- function(conf_date, conf_text, prompt_template, 
                                               output_dir, experiment_name, 
                                               max_attempts = 3) {
  
  cat(crayon::yellow(paste0("🔄 Processing ", experiment_name, " for ", conf_date, "\n")))
  
  # Construct the full prompt
  full_prompt <- gsub("\\[date\\]", conf_date, prompt_template)
  full_prompt <- paste0(
    full_prompt,
    "Press Conference on ", conf_date, "\n",
    "Text:", conf_text, "\n\n"
  )
  
  for (attempt in 1:max_attempts) {
    if (attempt > 1) Sys.sleep(5 * attempt) # Exponential backoff
    
    result <- tryCatch({
      res <- new_gemini(full_prompt, seed = 120, temperature = 1)
      
      # Save result
      output_file <- file.path(output_dir, paste0(conf_date, "_", experiment_name, ".rds"))
      saveRDS(res, file = output_file)
      
      cat(crayon::green(paste0("✅ ", experiment_name, " for ", conf_date, " completed\n")))
      return(TRUE)
    }, error = function(e) {
      cat(crayon::red(paste0("❌ Error in ", experiment_name, " for ", conf_date, ": ", e$message, "\n")))
      return(FALSE)
    })
    
    if (result) break # Exit loop if successful
  }
  
  if (!result) {
    cat(crayon::red(paste0("❌ All attempts failed for ", experiment_name, " - ", conf_date, "\n")))
  }
  
  return(result)
}

# Function to clean and process LLM results
clean_llm_results_scrambling <- function(results_dir, experiment_name) {
  
  cat("Cleaning results for", experiment_name, "\n")
  
  # Column names for results
  names_col <- c("date", "id", "tenor", "direction", "rate", "confidence")
  
  # Get result files
  result_files <- list.files(results_dir, 
                           pattern = paste0(".*", experiment_name, "\\.rds$"),
                           full.names = TRUE)
  
  if (length(result_files) == 0) {
    cat("No result files found for", experiment_name, "\n")
    return(NULL)
  }
  
  # Process results
  results <- map(result_files, function(file_path) {
    tryCatch({
      raw_result <- readRDS(file_path)
      
      # Parse the markdown table
      parsed_result <- raw_result %>% 
        readr::read_delim(delim = "|", trim_ws = TRUE, skip = 1, show_col_types = FALSE) %>% 
        select(-1, -ncol(.)) %>%  # Remove empty first and last columns
        slice(-nrow(.))  # Remove empty last row
      
      return(parsed_result)
    }, error = function(e) {
      cat("Error processing", basename(file_path), ":", e$message, "\n")
      return(NULL)
    })
  })
  
  # Clean and combine results
  clean_df <- results %>% 
    keep(~ !is.null(.x) && nrow(.x) > 0) %>%
    map(~ tryCatch({
      .x %>% 
        setNames(names_col) %>%
        slice(-1) %>%  # Remove header row if present
        mutate(
          date = as.character(date),
          rate = as.numeric(rate),
          confidence = as.numeric(confidence)
        ) %>%
        filter(tenor %in% c("3M", "2Y", "10Y"))
    }, error = function(e) {
      cat("Error in data cleaning:", e$message, "\n")
      return(NULL)
    })) %>%
    keep(~ !is.null(.x)) %>%
    bind_rows()
  
  if (nrow(clean_df) == 0) {
    cat("No clean data produced for", experiment_name, "\n")
    return(NULL)
  }
  
  cat("Cleaned", nrow(clean_df), "observations for", experiment_name, "\n")
  
  # Export cleaned data
  output_file <- paste0("../intermediate_data/scrambling_results_", experiment_name, "_", Sys.Date(), ".xlsx")
  writexl::write_xlsx(clean_df, output_file)
  cat("Results saved to:", output_file, "\n")
  
  return(clean_df)
}

# Function to evaluate scrambling experiment results
evaluate_scrambling_results <- function(original_results, scrambled_results, experiment_type = "sentences") {
  
  cat("=== EVALUATING SCRAMBLING EXPERIMENT:", toupper(experiment_type), "===\n")
  
  if (is.null(original_results) || is.null(scrambled_results)) {
    cat("Missing results data. Cannot evaluate.\n")
    return(NULL)
  }
  
  # Load market volatility data
  range_df <- tryCatch({
    read_rds("../intermediate_data/range_difference_df.rds") %>%
      mutate(
        tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor),
        date = as.Date(date)
      ) %>%
      select(tenor, date, correct_post_mean) %>%
      filter(tenor %in% c("3M", "2Y", "10Y"))
  }, error = function(e) {
    cat("Error loading market volatility data:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(range_df)) {
    cat("Cannot proceed without market volatility data\n")
    return(NULL)
  }
  
  # Function to compute correlations for a dataset
  compute_correlations <- function(llm_data, label) {
    
    # Compute standard deviations
    llm_std <- llm_data %>%
      group_by(date, tenor) %>%
      summarise(
        llm_std = sd(rate, na.rm = TRUE),
        n_agents = n(),
        .groups = "drop"
      ) %>%
      mutate(date = as.Date(date))
    
    # Join with market data
    combined_data <- llm_std %>%
      inner_join(range_df, by = c("date", "tenor")) %>%
      filter(!is.na(llm_std), !is.na(correct_post_mean))
    
    if (nrow(combined_data) == 0) {
      cat("No matching data for", label, "\n")
      return(NULL)
    }
    
    # Compute correlations by tenor
    correlations <- combined_data %>%
      group_by(tenor) %>%
      summarise(
        correlation = cor(llm_std, correct_post_mean, method = "spearman", use = "complete.obs"),
        n_obs = n(),
        .groups = "drop"
      ) %>%
      mutate(experiment = label)
    
    cat("Correlations for", label, ":\n")
    print(correlations)
    
    return(list(
      correlations = correlations,
      combined_data = combined_data
    ))
  }
  
  # Compute correlations for both datasets
  original_corr <- compute_correlations(original_results, "Original")
  scrambled_corr <- compute_correlations(scrambled_results, paste("Scrambled", experiment_type))
  
  if (is.null(original_corr) || is.null(scrambled_corr)) {
    cat("Cannot compute correlations for comparison\n")
    return(NULL)
  }
  
  # Combine results for comparison
  comparison_df <- bind_rows(
    original_corr$correlations,
    scrambled_corr$correlations
  )
  
  # Create comparison summary
  comparison_summary <- comparison_df %>%
    select(tenor, experiment, correlation) %>%
    pivot_wider(names_from = experiment, values_from = correlation) %>%
    mutate(
      difference = Original - get(paste("Scrambled", experiment_type)),
      ratio = Original / get(paste("Scrambled", experiment_type)),
      leakage_evidence = case_when(
        get(paste("Scrambled", experiment_type)) > 0.3 ~ "Strong Evidence",
        get(paste("Scrambled", experiment_type)) > 0.15 ~ "Moderate Evidence", 
        get(paste("Scrambled", experiment_type)) > 0.05 ~ "Weak Evidence",
        TRUE ~ "No Evidence"
      )
    )
  
  cat("\n=== COMPARISON SUMMARY ===\n")
  print(comparison_summary)
  
  # Overall assessment
  avg_original <- mean(comparison_summary$Original, na.rm = TRUE)
  avg_scrambled <- mean(comparison_summary[[paste("Scrambled", experiment_type)]], na.rm = TRUE)
  
  cat("\n=== OVERALL ASSESSMENT ===\n")
  cat("Average Original Correlation:", round(avg_original, 3), "\n")
  cat("Average Scrambled Correlation:", round(avg_scrambled, 3), "\n")
  cat("Ratio (Original/Scrambled):", round(avg_original/avg_scrambled, 2), "\n")
  
  # Verdict
  if (avg_scrambled > 0.3) {
    cat("🚨 VERDICT: Strong evidence of data leakage\n")
    cat("   The model maintains high correlation even with scrambled text\n")
  } else if (avg_scrambled > 0.15) {
    cat("⚠️  VERDICT: Moderate evidence of data leakage\n") 
    cat("   Some correlation persists with scrambled text - investigate further\n")
  } else if (avg_scrambled > 0.05) {
    cat("✅ VERDICT: Weak evidence of data leakage\n")
    cat("   Low scrambled correlation suggests model is reading text\n")
  } else {
    cat("✅ VERDICT: No evidence of data leakage\n")
    cat("   Scrambled text produces near-zero correlation - model is genuine\n")
  }
  
  # Save results
  results_summary <- list(
    comparison_table = comparison_summary,
    original_data = original_corr$combined_data,
    scrambled_data = scrambled_corr$combined_data,
    experiment_type = experiment_type,
    verdict = case_when(
      avg_scrambled > 0.3 ~ "Strong Leakage",
      avg_scrambled > 0.15 ~ "Moderate Leakage",
      avg_scrambled > 0.05 ~ "Weak Leakage", 
      TRUE ~ "No Leakage"
    )
  )
  
  # Export results
  output_file <- paste0("../output/scrambling_evaluation_", experiment_type, "_", Sys.Date(), ".xlsx")
  writexl::write_xlsx(
    list(
      "Comparison_Summary" = comparison_summary,
      "Original_Correlations" = original_corr$correlations,
      "Scrambled_Correlations" = scrambled_corr$correlations
    ), 
    output_file
  )
  
  cat("Evaluation results saved to:", output_file, "\n")
  
  return(results_summary)
}

# ==============================================================================
# COMPLETE SCRAMBLING EXPERIMENT PIPELINE
# ==============================================================================

run_complete_scrambling_experiment <- function(n_sample = 30, 
                                             scrambling_methods = c("sentences", "words"),
                                             prompt_template = prompt_naive) {
  
  cat("=== STARTING COMPLETE SCRAMBLING EXPERIMENT ===\n")


# ✅ DEFINITIVE FIX: Clear old scrambled text files and samples before starting.
  # This ensures each run is independent and processes only the current sample.
  cat(crayon::yellow("⚠️ Clearing directories for a clean run...\n"))
  unlink("../intermediate_data/texts_scrambled_sentences/", recursive = TRUE)
  unlink("../intermediate_data/texts_scrambled_words/", recursive = TRUE)
  unlink("../intermediate_data/texts_sample_original/", recursive = TRUE)
  
  cat("Sample size:", n_sample, "\n")
  cat("Scrambling methods:", paste(scrambling_methods, collapse = ", "), "\n")

  cat("Sample size:", n_sample, "\n")
  cat("Scrambling methods:", paste(scrambling_methods, collapse = ", "), "\n")
  
  # Step 1: Create sample and scrambled texts (if not already done)
  dates_ecb_presconf <- list.files("../intermediate_data/texts/") %>%
    str_subset("\\d") %>%
    str_remove("\\.txt") %>%
    str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
    as.Date() %>%
    sort()
  
  sample_dates <- create_strategic_sample(dates_ecb_presconf, n_sample = n_sample)
  
  if ("sentences" %in% scrambling_methods || "words" %in% scrambling_methods) {
    method_param <- if(length(scrambling_methods) > 1) "both" else scrambling_methods[1]
    scrambling_results <- create_scrambled_conferences(sample_dates, scrambling_method = method_param)
  }
  
  # Step 2: Run LLM on original conferences (using sample)
  cat("\n=== STEP 2: RUNNING LLM ON ORIGINAL CONFERENCES ===\n")
  
  # Create temporary directory with sampled original conferences
  dir.create("../intermediate_data/texts_sample_original/", showWarnings = FALSE)
  
  # Copy sampled conferences to temporary directory
  sample_date_strings <- as.character(sample_dates)
  original_files <- list.files("../intermediate_data/texts/", full.names = TRUE)
  sampled_original_files <- original_files[str_detect(original_files, paste(sample_date_strings, collapse = "|"))]
  
  file.copy(sampled_original_files, "../intermediate_data/texts_sample_original/")
  
  original_run_results <- run_llm_on_conference_directory(
    conference_dir = "../intermediate_data/texts_sample_original/",
    output_dir = "../intermediate_data/scrambling_llm_results/original/",
    experiment_name = "original",
    prompt_template = prompt_template
  )
  
  # Step 3: Run LLM on scrambled conferences
  experiment_results <- list()
  
  for (method in scrambling_methods) {
    cat("\n=== STEP 3:", toupper(method), "SCRAMBLING ===\n")
    
    scrambled_dir <- paste0("../intermediate_data/texts_scrambled_", method, "/")
    
    if (!dir.exists(scrambled_dir)) {
      cat("Scrambled directory not found:", scrambled_dir, "\n")
      next
    }
    
    scrambled_run_results <- run_llm_on_conference_directory(
      conference_dir = scrambled_dir,
      output_dir = paste0("../intermediate_data/scrambling_llm_results/scrambled_", method, "/"),
      experiment_name = paste0("scrambled_", method),
      prompt_template = prompt_template
    )
    
    experiment_results[[method]] <- scrambled_run_results
  }
  
  # Step 4: Clean results
  cat("\n=== STEP 4: CLEANING RESULTS ===\n")
  
  original_cleaned <- clean_llm_results_scrambling(
    "../intermediate_data/scrambling_llm_results/original/", 
    "original"
  )
  
  scrambled_cleaned <- list()
  for (method in scrambling_methods) {
    scrambled_cleaned[[method]] <- clean_llm_results_scrambling(
      paste0("../intermediate_data/scrambling_llm_results/scrambled_", method, "/"),
      paste0("scrambled_", method)
    )
  }
  
  # Step 5: Evaluate results
  cat("\n=== STEP 5: EVALUATING RESULTS ===\n")
  
  evaluation_results <- list()
  for (method in scrambling_methods) {
    if (!is.null(scrambled_cleaned[[method]])) {
      evaluation_results[[method]] <- evaluate_scrambling_results(
        original_results = original_cleaned,
        scrambled_results = scrambled_cleaned[[method]], 
        experiment_type = method
      )
    }
  }
  
  # Clean up temporary files
  unlink("../intermediate_data/texts_sample_original/", recursive = TRUE)
  
  cat("\n=== SCRAMBLING EXPERIMENT COMPLETE ===\n")
  return(list(
    sample_info = list(n_sample = n_sample, sample_dates = sample_dates),
    original_results = original_cleaned,
    scrambled_results = scrambled_cleaned,
    evaluations = evaluation_results
  ))
}

# Run the complete experiment
cat("\n=== READY TO RUN COMPLETE SCRAMBLING EXPERIMENT ===\n")
cat("To execute, run: experiment_results <- run_complete_scrambling_experiment()\n")
cat("This will test both sentence and word scrambling on 30 strategically sampled conferences\n")


experiment_results <- run_complete_scrambling_experiment(
  n_sample = 30,
  scrambling_methods = c("words"),
  prompt_template = prompt_naive
)

# ==============================================================================
# BOOTSTRAP CONFIDENCE INTERVALS FOR DATA LEAKAGE DETECTION
# Extension to your existing scrambling experiment code
# ==============================================================================

library(boot)
library(ggplot2)
library(RColorBrewer)

# Function to bootstrap correlation confidence intervals using your experiment_results
bootstrap_scrambling_analysis <- function(experiment_results, n_bootstrap = 1000, conf_level = 0.95) {
  
  cat("=== BOOTSTRAP ANALYSIS FOR DATA LEAKAGE DETECTION ===\n")
  cat("Bootstrap samples:", n_bootstrap, "\n")
  cat("Confidence level:", conf_level, "\n\n")
  
  # Extract results from your experiment structure
  original_results <- experiment_results$original_results
  scrambled_results <- experiment_results$scrambled_results$words  # Focus on words as you requested
  
  if (is.null(original_results) || is.null(scrambled_results)) {
    cat("Error: Missing original or scrambled results\n")
    return(NULL)
  }
  
  # Load market volatility data (same as in your existing code)
  range_df <- tryCatch({
    read_rds("../intermediate_data/range_difference_df.rds") %>%
      mutate(
        tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor),
        date = as.Date(date)
      ) %>%
      select(tenor, date, correct_post_mean) %>%
      filter(tenor %in% c("3M", "2Y", "10Y"))
  }, error = function(e) {
    cat("Error loading market volatility data:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(range_df)) {
    cat("Cannot proceed without market volatility data\n")
    return(NULL)
  }
  
  # Function to compute correlation for a single dataset
  compute_single_correlation <- function(llm_data) {
    # Compute standard deviations by date and tenor
    llm_std <- llm_data %>%
      group_by(date, tenor) %>%
      summarise(llm_std = sd(rate, na.rm = TRUE), .groups = "drop") %>%
      mutate(date = as.Date(date))
    
    # Join with market data
    combined_data <- llm_std %>%
      inner_join(range_df, by = c("date", "tenor")) %>%
      filter(!is.na(llm_std), !is.na(correct_post_mean))
    
    if (nrow(combined_data) == 0) return(NA)
    
    # Compute average correlation across all tenors
    avg_correlation <- combined_data %>%
      group_by(tenor) %>%
      summarise(corr = cor(llm_std, correct_post_mean, method = "spearman", use = "complete.obs"), .groups = "drop") %>%
      summarise(avg_corr = mean(corr, na.rm = TRUE)) %>%
      pull(avg_corr)
    
    return(avg_correlation)
  }
  
  # Compute original correlations
  original_corr <- compute_single_correlation(original_results)
  scrambled_corr <- compute_single_correlation(scrambled_results)
  
  cat("Point estimates:\n")
  cat("Original correlation:", round(original_corr, 3), "\n")
  cat("Scrambled correlation:", round(scrambled_corr, 3), "\n\n")
  
  # Bootstrap function for original data
  bootstrap_original <- function(data, indices) {
    sample_data <- data[indices, ]
    compute_single_correlation(sample_data)
  }
  
  # Bootstrap function for scrambled data
  bootstrap_scrambled <- function(data, indices) {
    sample_data <- data[indices, ]
    compute_single_correlation(sample_data)
  }
  
  # Perform bootstrap resampling
  cat("Performing bootstrap resampling...\n")
  
  set.seed(1234)  # For reproducibility
  
  # Bootstrap original correlations
  original_boot <- boot(
    data = original_results,
    statistic = bootstrap_original,
    R = n_bootstrap
  )
  
  # Bootstrap scrambled correlations
  scrambled_boot <- boot(
    data = scrambled_results,
    statistic = bootstrap_scrambled,
    R = n_bootstrap
  )
  
  # Compute confidence intervals
  alpha <- 1 - conf_level
  
  original_ci <- boot.ci(original_boot, type = "perc", conf = conf_level)
  scrambled_ci <- boot.ci(scrambled_boot, type = "perc", conf = conf_level)
  
  # Extract CI bounds
  original_lower <- original_ci$percent[4]
  original_upper <- original_ci$percent[5]
  scrambled_lower <- scrambled_ci$percent[4]
  scrambled_upper <- scrambled_ci$percent[5]
  
  # Bootstrap the difference
  bootstrap_difference <- function(data1, data2, indices1, indices2) {
    corr1 <- compute_single_correlation(data1[indices1, ])
    corr2 <- compute_single_correlation(data2[indices2, ])
    return(corr1 - corr2)
  }
  
  # Create paired bootstrap samples for difference
  n_orig <- nrow(original_results)
  n_scram <- nrow(scrambled_results)
  
  diff_bootstrap <- replicate(n_bootstrap, {
    indices1 <- sample(1:n_orig, n_orig, replace = TRUE)
    indices2 <- sample(1:n_scram, n_scram, replace = TRUE)
    bootstrap_difference(original_results, scrambled_results, indices1, indices2)
  })
  
  # Remove NAs
  diff_bootstrap <- diff_bootstrap[!is.na(diff_bootstrap)]
  
  # Compute difference statistics
  mean_diff <- mean(diff_bootstrap)
  diff_ci_lower <- quantile(diff_bootstrap, alpha/2)
  diff_ci_upper <- quantile(diff_bootstrap, 1 - alpha/2)
  
  # Create results summary
  results_summary <- data.frame(
    experiment = c("Original", "Scrambled", "Difference"),
    correlation = c(original_corr, scrambled_corr, mean_diff),
    ci_lower = c(original_lower, scrambled_lower, diff_ci_lower),
    ci_upper = c(original_upper, scrambled_upper, diff_ci_upper),
    bootstrap_se = c(sd(original_boot$t, na.rm = TRUE), 
                     sd(scrambled_boot$t, na.rm = TRUE), 
                     sd(diff_bootstrap, na.rm = TRUE)),
    n_bootstrap = c(sum(!is.na(original_boot$t)), 
                    sum(!is.na(scrambled_boot$t)), 
                    length(diff_bootstrap))
  )
  
  # Data leakage assessment
  scrambled_significant <- scrambled_lower > 0
  difference_significant <- diff_ci_lower > 0
  
  if (scrambled_corr > 0.3) {
    verdict <- "Strong Evidence of Data Leakage"
    verdict_emoji <- "🚨"
  } else if (scrambled_corr > 0.15) {
    verdict <- "Moderate Evidence of Data Leakage" 
    verdict_emoji <- "⚠️"
  } else if (scrambled_corr > 0.05) {
    verdict <- "Weak Evidence of Data Leakage"
    verdict_emoji <- "⚠️"
  } else {
    verdict <- "No Evidence of Data Leakage"
    verdict_emoji <- "✅"
  }
  
  cat("=== BOOTSTRAP RESULTS SUMMARY ===\n")
  print(results_summary)
  
  cat("\n=== DATA LEAKAGE ASSESSMENT ===\n")
  cat("Scrambled correlation significantly > 0:", scrambled_significant, "\n")
  cat("Difference significantly > 0:", difference_significant, "\n")
  cat(verdict_emoji, "VERDICT:", verdict, "\n")
  
  # Return comprehensive results
  return(list(
    results_summary = results_summary,
    original_bootstrap = original_boot$t,
    scrambled_bootstrap = scrambled_boot$t,
    difference_bootstrap = diff_bootstrap,
    verdict = verdict,
    original_corr = original_corr,
    scrambled_corr = scrambled_corr,
    mean_difference = mean_diff,
    conf_level = conf_level
  ))
}

# Function to create comprehensive bootstrap visualizations with consistent aesthetics
# Follows the visual style established in the codebase (09plot_llm_results.R, 11compare_prompts.R)

create_bootstrap_plots <- function(bootstrap_results, output_dir = "../output/figures/") {
  
  # Load required libraries
  library(tidyverse)
  library(RColorBrewer)
  library(showtext)
  
  # Enable Segoe UI font (consistent with codebase)
  if (!("Segoe UI" %in% font_families())) {
    # Try different potential font paths
    font_paths <- c(
      "C:/Windows/Fonts/segoeui.ttf",
      "C:/Users/collodelu/OneDrive - centralbankmalta.org/Desktop/Projects/Uncertainty_surprises/code/segoeui.ttf"
    )
    
    for (path in font_paths) {
      if (file.exists(path)) {
        font_add("Segoe UI", regular = path)
        break
      }
    }
  }
  showtext_auto()
  
  # Ensure output directory exists
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  results_df <- bootstrap_results$results_summary
  
  # Define professional color scheme (consistent with codebase style)
  colors <- c("Original" = "#4575b4", "Scrambled" = "#d73027", "Difference" = "#91bfdb")
  
  #------------------------------------------------------------------------------
  ## Plot 1: Main results with confidence intervals
  #------------------------------------------------------------------------------
  
  p1 <- results_df %>%
    filter(experiment != "Difference") %>%
    mutate(experiment = factor(experiment, levels = c("Original", "Scrambled"))) %>%
    ggplot(aes(x = experiment, y = correlation, fill = experiment)) +
    geom_col(width = 0.5, col = "white", alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.2, color = "black", linewidth = 1) +
    geom_text(aes(label = sprintf("%.3f", correlation)), 
              vjust = -0.5, size = 4, fontface = "bold") +
    scale_fill_manual(values = colors) +
    scale_y_continuous(limits = c(0, max(results_df$ci_upper[1:2], na.rm = TRUE) * 1.15),
                       breaks = pretty) +
    labs(
      title = NULL,  # Remove title for consistency
      subtitle = NULL,  # Remove subtitle for consistency
      x = "",
      y = "Average Spearman Correlation",
      caption = ""
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.caption = element_text(hjust = 0, size = 9, color = "grey50"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 24),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18),
      strip.text = element_text(size = 24),
      legend.position = "none",  # No legend needed as colors map to x-axis
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
    )
  
  # Save Plot 1
  ggsave(
    filename = file.path(output_dir, "bootstrap_main_results.png"),
    plot = p1,
    dpi = 320,
    width = 8,
    height = 6,
    bg = "white"
  )
  
  #------------------------------------------------------------------------------
  ## Plot 2: Bootstrap distributions
  #------------------------------------------------------------------------------
  
  bootstrap_data <- data.frame(
    correlation = c(bootstrap_results$original_bootstrap, bootstrap_results$scrambled_bootstrap),
    experiment = rep(c("Original", "Scrambled"), 
                    c(length(bootstrap_results$original_bootstrap), 
                      length(bootstrap_results$scrambled_bootstrap)))
  ) %>%
    filter(!is.na(correlation)) %>%
    mutate(experiment = factor(experiment, levels = c("Original", "Scrambled")))
  
  p2 <- ggplot(bootstrap_data, aes(x = correlation, fill = experiment)) +
    geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
    geom_vline(data = results_df %>% filter(experiment != "Difference"), 
               aes(xintercept = correlation, color = experiment), 
               linetype = "dashed", linewidth = 1.2) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    facet_wrap(~ experiment, nrow = 2, scales = "free_y") +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Bootstrap Correlation Estimate",
      y = "Frequency",
      caption = ""
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.caption = element_text(hjust = 0, size = 9, color = "grey50"),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18),
      strip.text = element_text(size = 24, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "grey80", fill = NA),
    )
  
  # Save Plot 2
  ggsave(
    filename = file.path(output_dir, "bootstrap_distributions.png"),
    plot = p2,
    dpi = 320,
    width = 10,
    height = 8,
    bg = "white"
  )
  
  #------------------------------------------------------------------------------
  ## Plot 3: Difference analysis
  #------------------------------------------------------------------------------
  
  diff_data <- data.frame(difference = bootstrap_results$difference_bootstrap) %>%
    filter(!is.na(difference))
  
  diff_result <- results_df %>% filter(experiment == "Difference")
  
  p3 <- ggplot(diff_data, aes(x = difference)) +
    geom_histogram(fill = colors["Difference"], alpha = 0.8, bins = 40, color = "white") +
    geom_vline(xintercept = diff_result$correlation, 
               color = "#d73027", linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = diff_result$ci_lower, 
               color = "#d73027", linetype = "dotted", linewidth = 1) +
    geom_vline(xintercept = diff_result$ci_upper, 
               color = "#d73027", linetype = "dotted", linewidth = 1) +
    geom_vline(xintercept = 0, color = "grey30", linetype = "solid", linewidth = 0.8) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "Correlation Difference (Original - Scrambled)",
      y = "Frequency",
      caption = "") +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.caption = element_text(hjust = 0, size = 9, color = "grey50"),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18),
      strip.text = element_text(size = 24),
      panel.grid.minor = element_blank(),
    )
  
  # Save Plot 3
  ggsave(
    filename = file.path(output_dir, "bootstrap_difference_analysis.png"),
    plot = p3,
    dpi = 320,
    width = 10,
    height = 6,
    bg = "white"
  )
  
  #------------------------------------------------------------------------------
  ## Plot 4: Summary statistics bar chart
  #------------------------------------------------------------------------------
  
  summary_stats_df <- results_df %>%
    filter(experiment != "Difference") %>%
    mutate(
      experiment = factor(experiment, levels = c("Original", "Scrambled")),
      bootstrap_se_formatted = sprintf("%.3f", bootstrap_se)
    )
  
  p4 <- ggplot(summary_stats_df, aes(x = experiment, y = bootstrap_se, fill = experiment)) +
    geom_col(width = 0.5, col = "white", alpha = 0.8) +
    geom_text(aes(label = bootstrap_se_formatted), 
              vjust = -0.5, size = 4, fontface = "bold") +
    scale_fill_manual(values = colors) +
    labs(
      title = NULL,
      subtitle = NULL,
      x = "",
      y = "Bootstrap Standard Error",
      caption = ""
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.caption = element_text(hjust = 0, size = 9, color = "grey50"),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 24),
      axis.text.y = element_text(size = 14),
      axis.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18),
      strip.text = element_text(size = 24),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
    )
  
  # Save Plot 4
  ggsave(
    filename = file.path(output_dir, "bootstrap_standard_errors.png"),
    plot = p4,
    dpi = 320,
    width = 8,
    height = 6,
    bg = "white"
  )
  
  #------------------------------------------------------------------------------
  ## Return summary information
  #------------------------------------------------------------------------------
  
  cat("Bootstrap visualization plots created successfully!\n")
  cat("Files saved to:", output_dir, "\n")
  cat("- bootstrap_main_results.png\n")
  cat("- bootstrap_distributions.png\n") 
  cat("- bootstrap_difference_analysis.png\n")
  cat("- bootstrap_standard_errors.png\n")
  
  # Create summary table for console output
  summary_table <- results_df %>%
    mutate(
      ci_text = paste0("[", sprintf("%.3f", ci_lower), ", ", sprintf("%.3f", ci_upper), "]"),
      correlation_text = sprintf("%.3f", correlation),
      se_text = sprintf("%.3f", bootstrap_se)
    ) %>%
    select(experiment, correlation_text, ci_text, se_text) %>%
    rename(
      "Experiment" = experiment,
      "Correlation" = correlation_text,
      "95% CI" = ci_text,
      "Bootstrap SE" = se_text
    )
  
  cat("\nSummary Statistics:\n")
  print(summary_table)
  
  # Return verdict
  verdict <- bootstrap_results$verdict
  cat("\nData Leakage Test Verdict:", verdict, "\n")
  
  return(list(
    plot1 = p1,
    plot2 = p2, 
    plot3 = p3,
    plot4 = p4,
    summary_table = summary_table,
    verdict = verdict
  ))
}

# Function to save detailed results
save_bootstrap_results <- function(bootstrap_results, filename = NULL) {
  
  if (is.null(filename)) {
    filename <- paste0("../output/bootstrap_data_leakage_analysis_", Sys.Date(), ".xlsx")
  }
  
  # Create comprehensive results list for export
  export_data <- list(
    "Summary" = bootstrap_results$results_summary,
    "Original_Bootstrap" = data.frame(
      sample = 1:length(bootstrap_results$original_bootstrap),
      correlation = bootstrap_results$original_bootstrap
    ),
    "Scrambled_Bootstrap" = data.frame(
      sample = 1:length(bootstrap_results$scrambled_bootstrap),
      correlation = bootstrap_results$scrambled_bootstrap
    ),
    "Difference_Bootstrap" = data.frame(
      sample = 1:length(bootstrap_results$difference_bootstrap),
      difference = bootstrap_results$difference_bootstrap
    ),
    "Assessment" = data.frame(
      metric = c("Verdict", "Original_Correlation", "Scrambled_Correlation", "Mean_Difference"),
      value = c(bootstrap_results$verdict, 
                bootstrap_results$original_corr,
                bootstrap_results$scrambled_corr,
                bootstrap_results$mean_difference)
    )
  )
  
  writexl::write_xlsx(export_data, filename)
  cat("Detailed results saved to:", filename, "\n")
}

# ==============================================================================
# USAGE EXAMPLE WITH YOUR EXPERIMENT_RESULTS
# ==============================================================================

# After running your experiment, use these functions:

# 1. Perform bootstrap analysis
cat("\n=== RUNNING BOOTSTRAP ANALYSIS ON YOUR EXPERIMENT RESULTS ===\n")
bootstrap_analysis <- bootstrap_scrambling_analysis(experiment_results, n_bootstrap = 5000)

# 2. Create visualizations
if (!is.null(bootstrap_analysis)) {
  plots <- create_bootstrap_plots(bootstrap_analysis)
  
  # 3. Save detailed results
  save_bootstrap_results(bootstrap_analysis)
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Check the ../output/figures/ directory for visualizations\n")
  cat("Check for Excel file with detailed bootstrap results\n")
}