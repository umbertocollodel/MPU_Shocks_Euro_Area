# Text Scrambling for Data Leakage Detection
# Author: Based on your macroeconomics research style
# Purpose: Test whether LLM is actually reading text or using memorized patterns

library(tidyverse)
library(stringr)
library(readtext)

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
      # Sample proportionally but ensure minimum representation
      slice_sample(n = max(2, round(n_sample * n() / nrow(conference_df)))) %>%
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