# Simplified Word Scrambling for Data Leakage Detection
# Author: Based on your macroeconomics research style
# Purpose: Test whether LLM is reading text or using memorized patterns

library(tidyverse)
library(stringr)
library(readtext)
library(future)
library(furrr)
library(writexl)
library(crayon)

setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# ==============================================================================
# SIMPLE WORD SCRAMBLING FUNCTION
# ==============================================================================

scramble_words <- function(text) {
  if (is.na(text) || nchar(text) == 0) return(text)
  
  # Split by sentences to preserve sentence structure
  sentences <- str_split(text, "(?<=[.!?])\\s+", simplify = FALSE)[[1]]
  sentences <- sentences[nchar(sentences) > 0]
  
  # Scramble words within each sentence
  scrambled_sentences <- map_chr(sentences, function(sentence) {
    # Extract punctuation
    ending_punct <- str_extract(sentence, "[.!?]+$")
    if (is.na(ending_punct)) ending_punct <- ""
    
    # Get words (remove punctuation)
    clean_sentence <- str_remove(sentence, "[.!?]+$")
    words <- str_split(clean_sentence, "\\s+", simplify = FALSE)[[1]]
    words <- words[nchar(words) > 0]
    
    if (length(words) <= 1) return(sentence)
    
    # Scramble the words
    scrambled_words <- sample(words)
    
    # Reconstruct with punctuation
    paste0(paste(scrambled_words, collapse = " "), ending_punct)
  })
  
  paste(scrambled_sentences, collapse = " ")
}

# ==============================================================================
# SIMPLIFIED SAMPLING
# ==============================================================================

create_sample <- function(conference_dates, n_sample = 30) {
  sample_dates <- sample(conference_dates, n_sample)
  
  cat("Sample size:", length(sample_dates), "\n")
  cat("Date range:", min(sample_dates), "to", max(sample_dates), "\n")
  
  return(sample_dates)
}

# ==============================================================================
# CREATE SCRAMBLED CONFERENCES
# ==============================================================================

create_scrambled_conferences <- function(sample_dates) {
  
  # Clear previous directories
  unlink("../intermediate_data/texts_scrambled_words/", recursive = TRUE)
  unlink("../intermediate_data/texts_sample_original/", recursive = TRUE)
  
  # Create directories
  dir.create("../intermediate_data/texts_scrambled_words/", showWarnings = FALSE, recursive = TRUE)
  dir.create("../intermediate_data/texts_sample_original/", showWarnings = FALSE, recursive = TRUE)
  
  # Get original files
  conference_files <- list.files("../intermediate_data/texts/", pattern = "\\d{4}-\\d{2}-\\d{2}", full.names = TRUE)
  sample_date_strings <- as.character(sample_dates)
  sampled_files <- conference_files[str_detect(conference_files, paste(sample_date_strings, collapse = "|"))]
  
  cat("Found", length(sampled_files), "conference files\n")
  
  for (i in seq_along(sampled_files)) {
    file_path <- sampled_files[i]
    conf_date <- str_extract(basename(file_path), "\\d{4}-\\d{2}-\\d{2}")
    
    cat("Processing:", conf_date, "(", i, "of", length(sampled_files), ")\n")
    
    # Read original text
    original_text <- readtext(file_path)$text
    
    # Copy original to sample directory
    file.copy(file_path, "../intermediate_data/texts_sample_original/")
    
    # Create scrambled version
    scrambled_text <- scramble_words(original_text)
    
    # Save scrambled version
    scrambled_filename <- paste0("../intermediate_data/texts_scrambled_words/", conf_date, "_word_scrambled.txt")
    writeLines(scrambled_text, scrambled_filename)
  }
  
  cat("Scrambling complete!\n")
}

# ==============================================================================
# RUN LLM ON CONFERENCES (SEQUENTIAL)
# ==============================================================================

process_single_conference <- function(conf_date, conf_text, prompt_template, output_dir, experiment_name) {
  
  cat(crayon::yellow(paste0("Processing ", experiment_name, " for ", conf_date, "\n")))
  
  # Construct prompt
  full_prompt <- gsub("\\[date\\]", conf_date, prompt_template)
  full_prompt <- paste0(full_prompt, "Press Conference on ", conf_date, "\n", "Text:", conf_text, "\n\n")
  
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
  
  return(result)
}

run_llm_on_directory <- function(conference_dir, output_dir, experiment_name, prompt_template = prompt_naive) {
  
  # Clear previous results
  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Get conference files
  conference_files <- list.files(conference_dir, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(conference_files) == 0) {
    cat("No conference files found in", conference_dir, "\n")
    return(NULL)
  }
  
  # Prepare data
  conference_data <- map_dfr(conference_files, function(file_path) {
    filename <- basename(file_path)
    conf_date <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
    
    if (is.na(conf_date)) return(NULL)
    
    text_content <- paste(readLines(file_path, warn = FALSE), collapse = " ")
    data.frame(date = conf_date, text = text_content, stringsAsFactors = FALSE)
  })
  
  cat("Processing", nrow(conference_data), "conferences for", experiment_name, "sequentially\n")
  
  # Process conferences sequentially with map
  results_sequential <- map2_lgl(
    conference_data$date,
    conference_data$text,
    ~ {
      # Add small delay between requests to be API-friendly
      if (.x != conference_data$date[1]) {
        Sys.sleep(2)  # 2 second delay between requests
      }
      
      process_single_conference(
        conf_date = .x,
        conf_text = .y,
        prompt_template = prompt_template,
        output_dir = output_dir,
        experiment_name = experiment_name
      )
    }
  )
  
  successful_runs <- sum(results_sequential, na.rm = TRUE)
  cat("Successfully processed", successful_runs, "out of", nrow(conference_data), "conferences\n")
  
  return(list(total = nrow(conference_data), successful = successful_runs))
}

# ==============================================================================
# CLEAN RESULTS
# ==============================================================================

clean_llm_results <- function(results_dir, experiment_name) {
  
  cat("Cleaning results for", experiment_name, "\n")
  
  names_col <- c("date", "id", "tenor", "direction", "rate", "confidence")
  
  result_files <- list.files(results_dir, pattern = paste0(".*", experiment_name, "\\.rds$"), full.names = TRUE)
  
  if (length(result_files) == 0) {
    cat("No result files found for", experiment_name, "\n")
    return(NULL)
  }
  
  results <- map(result_files, function(file_path) {
    tryCatch({
      raw_result <- readRDS(file_path)
      parsed_result <- raw_result %>% 
        readr::read_delim(delim = "|", trim_ws = TRUE, skip = 1, show_col_types = FALSE) %>% 
        select(-1, -ncol(.)) %>%
        slice(-nrow(.))
      return(parsed_result)
    }, error = function(e) {
      cat("Error processing", basename(file_path), ":", e$message, "\n")
      return(NULL)
    })
  })
  
  clean_df <- results %>% 
    keep(~ !is.null(.x) && nrow(.x) > 0) %>%
    map(~ tryCatch({
      .x %>% 
        setNames(names_col) %>%
        slice(-1) %>%
        mutate(
          date = as.character(date),
          rate = as.numeric(rate),
          confidence = as.numeric(confidence)
        ) %>%
        filter(tenor %in% c("3M", "2Y", "10Y"))
    }, error = function(e) NULL)) %>%
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

# ==============================================================================
# EVALUATE SCRAMBLING RESULTS
# ==============================================================================

evaluate_scrambling_results <- function(original_results, scrambled_results) {
  
  cat("=== EVALUATING WORD SCRAMBLING EXPERIMENT ===\n")
  
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
  
  # Function to compute correlations
  compute_correlations <- function(llm_data, label) {
    llm_std <- llm_data %>%
      group_by(date, tenor) %>%
      summarise(llm_std = sd(rate, na.rm = TRUE), .groups = "drop") %>%
      mutate(date = as.Date(date))
    
    combined_data <- llm_std %>%
      inner_join(range_df, by = c("date", "tenor")) %>%
      filter(!is.na(llm_std), !is.na(correct_post_mean))
    
    if (nrow(combined_data) == 0) {
      cat("No matching data for", label, "\n")
      return(NULL)
    }
    
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
    
    return(list(correlations = correlations, combined_data = combined_data))
  }
  
  # Compute correlations for both datasets
  original_corr <- compute_correlations(original_results, "Original")
  scrambled_corr <- compute_correlations(scrambled_results, "Scrambled Words")
  
  if (is.null(original_corr) || is.null(scrambled_corr)) {
    cat("Cannot compute correlations for comparison\n")
    return(NULL)
  }
  
  # Create comparison
  comparison_df <- bind_rows(original_corr$correlations, scrambled_corr$correlations) %>%
    select(tenor, experiment, correlation) %>%
    pivot_wider(names_from = experiment, values_from = correlation) %>%
    mutate(
      difference = Original - `Scrambled Words`,
      ratio = Original / `Scrambled Words`
    )
  
  cat("\n=== COMPARISON SUMMARY ===\n")
  print(comparison_df)
  
  # Overall assessment
  avg_original <- mean(comparison_df$Original, na.rm = TRUE)
  avg_scrambled <- mean(comparison_df$`Scrambled Words`, na.rm = TRUE)
  
  cat("\n=== OVERALL ASSESSMENT ===\n")
  cat("Average Original Correlation:", round(avg_original, 3), "\n")
  cat("Average Scrambled Correlation:", round(avg_scrambled, 3), "\n")
  cat("Ratio (Original/Scrambled):", round(avg_original/avg_scrambled, 2), "\n")
  
  # Verdict
  if (avg_scrambled > 0.3) {
    verdict <- "STRONG EVIDENCE of data leakage"
    cat("🚨 VERDICT:", verdict, "\n")
    cat("   The model maintains high correlation even with scrambled text\n")
  } else if (avg_scrambled > 0.15) {
    verdict <- "MODERATE EVIDENCE of data leakage"
    cat("⚠️ VERDICT:", verdict, "\n")
    cat("   Some correlation persists with scrambled text - investigate further\n")
  } else if (avg_scrambled > 0.05) {
    verdict <- "WEAK EVIDENCE of data leakage"
    cat("✅ VERDICT:", verdict, "\n")
    cat("   Low scrambled correlation suggests model is reading text\n")
  } else {
    verdict <- "NO EVIDENCE of data leakage"
    cat("✅ VERDICT:", verdict, "\n")
    cat("   Scrambled text produces near-zero correlation - model is genuine\n")
  }
  
  # Save results
  output_file <- paste0("../output/word_scrambling_evaluation_", Sys.Date(), ".xlsx")
  writexl::write_xlsx(list(
    "Comparison_Summary" = comparison_df,
    "Original_Correlations" = original_corr$correlations,
    "Scrambled_Correlations" = scrambled_corr$correlations
  ), output_file)
  
  cat("Evaluation results saved to:", output_file, "\n")
  
  return(list(
    comparison = comparison_df,
    verdict = verdict,
    avg_original = avg_original,
    avg_scrambled = avg_scrambled
  ))
}

# ==============================================================================
# COMPLETE PIPELINE
# ==============================================================================

run_complete_word_scrambling <- function(n_sample = 30, prompt_template = prompt_naive) {
  
  cat("=== STARTING WORD SCRAMBLING EXPERIMENT ===\n")
  cat("Sample size:", n_sample, "\n")
  
  # Step 1: Create sample and scrambled texts
  dates_ecb_presconf <- list.files("../intermediate_data/texts/") %>%
    str_subset("\\d") %>%
    str_remove("\\.txt") %>%
    str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
    as.Date() %>%
    sort()
  
  sample_dates <- create_sample(dates_ecb_presconf, n_sample = n_sample)
  create_scrambled_conferences(sample_dates)
  
  # Step 2: Run LLM on original conferences
  cat("\n=== RUNNING LLM ON ORIGINAL CONFERENCES ===\n")
  original_run_results <- run_llm_on_directory(
    conference_dir = "../intermediate_data/texts_sample_original/",
    output_dir = "../intermediate_data/scrambling_llm_results/original/",
    experiment_name = "original",
    prompt_template = prompt_template
  )
  
  # Step 3: Run LLM on scrambled conferences
  cat("\n=== RUNNING LLM ON SCRAMBLED CONFERENCES ===\n")
  scrambled_run_results <- run_llm_on_directory(
    conference_dir = "../intermediate_data/texts_scrambled_words/",
    output_dir = "../intermediate_data/scrambling_llm_results/scrambled_words/",
    experiment_name = "scrambled_words",
    prompt_template = prompt_template
  )
  
  # Step 4: Clean results
  cat("\n=== CLEANING RESULTS ===\n")
  original_cleaned <- clean_llm_results("../intermediate_data/scrambling_llm_results/original/", "original")
  scrambled_cleaned <- clean_llm_results("../intermediate_data/scrambling_llm_results/scrambled_words/", "scrambled_words")
  
  # Step 5: Evaluate results
  cat("\n=== EVALUATING RESULTS ===\n")
  evaluation_results <- evaluate_scrambling_results(original_cleaned, scrambled_cleaned)
  
  cat("\n=== WORD SCRAMBLING EXPERIMENT COMPLETE ===\n")
  
  return(list(
    sample_info = list(n_sample = n_sample, sample_dates = sample_dates),
    execution_results = list(original = original_run_results, scrambled = scrambled_run_results),
    cleaned_results = list(original = original_cleaned, scrambled = scrambled_cleaned),
    evaluation = evaluation_results
  ))
}

# ==============================================================================
# READY TO RUN
# ==============================================================================

cat("\n=== SIMPLIFIED WORD SCRAMBLING READY ===\n")
cat("To execute: word_scrambling_results <- run_complete_word_scrambling()\n")

# Uncomment to run:
word_scrambling_results <- run_complete_word_scrambling()