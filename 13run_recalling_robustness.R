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
  scrambling_methods = c("sentences", "words"),
  prompt_template = prompt_naive
)
