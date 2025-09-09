# Controlled Experiment: Information Content Test with PARALLEL PROCESSING
# Testing 50 conferences across different ECB policy regimes

# Load necessary libraries and set parameters
setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

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
  future,     # For parallel processing
  furrr       # For parallel map functions
)


# ============================================================================
# SETUP PARALLEL PROCESSING
# ============================================================================

# Set up parallel processing plan
# Adjust workers based on your API rate limits - Gemini typically allows 60 requests/minute
plan(multisession, workers = 2)  # Use 5 parallel workers

print(paste("Parallel processing enabled with", future::nbrOfWorkers(), "workers"))

# ============================================================================
# STEP 1: DEFINE POLICY REGIMES AND SELECT CONFERENCES
# ============================================================================

# Load your existing data to get available dates
name_prompt_request <- "prompt_naive"

# Load the original July results
llm_july_results <- read_xlsx(paste0("../intermediate_data/aggregate_gemini_result/", 
                                     name_prompt_request, "/",
                                     "2.5flash_",
                                     "2025-07-21",
                                     ".xlsx"))

# Get unique conference dates
available_dates <- unique(as.Date(llm_july_results$date))

print(paste("Total available conferences:", length(available_dates)))

# Define ECB policy regimes
policy_regimes <- list(
  "Duisenberg_early" = list(
    start = as.Date("1998-06-01"),
    end = as.Date("2003-10-31"),
    description = "Early ECB, establishing credibility",
    n_select = 5
  ),
  "Trichet_normal" = list(
    start = as.Date("2003-11-01"),
    end = as.Date("2007-07-31"),
    description = "Great Moderation period",
    n_select = 5
  ),
  "Financial_crisis" = list(
    start = as.Date("2007-08-01"),
    end = as.Date("2009-12-31"),
    description = "Global Financial Crisis",
    n_select = 8
  ),
  "Sovereign_debt_crisis" = list(
    start = as.Date("2010-01-01"),
    end = as.Date("2012-07-31"),
    description = "European Sovereign Debt Crisis",
    n_select = 7
  ),
  "Draghi_whatever_it_takes" = list(
    start = as.Date("2012-08-01"),
    end = as.Date("2014-05-31"),
    description = "Post 'Whatever it takes'",
    n_select = 5
  ),
  "QE_era" = list(
    start = as.Date("2014-06-01"),
    end = as.Date("2018-12-31"),
    description = "Quantitative Easing period",
    n_select = 6
  ),
  "Normalization_attempt" = list(
    start = as.Date("2019-01-01"),
    end = as.Date("2020-02-29"),
    description = "Attempted policy normalization",
    n_select = 3
  ),
  "Pandemic" = list(
    start = as.Date("2020-03-01"),
    end = as.Date("2021-06-30"),
    description = "COVID-19 pandemic",
    n_select = 5
  ),
  "Inflation_surge" = list(
    start = as.Date("2021-07-01"),
    end = as.Date("2022-06-30"),
    description = "Initial inflation surge",
    n_select = 4
  ),
  "Aggressive_tightening" = list(
    start = as.Date("2022-07-01"),
    end = as.Date("2024-12-31"),
    description = "Aggressive rate hiking cycle",
    n_select = 7
  )
)

# Select conferences from each regime
selected_conferences <- list()

for (regime_name in names(policy_regimes)) {
  regime <- policy_regimes[[regime_name]]
  
  # Get dates in this regime
  regime_dates <- available_dates[available_dates >= regime$start & 
                                  available_dates <= regime$end]
  
  # Sample the required number (or all if fewer available)
  n_to_select <- min(regime$n_select, length(regime_dates))
  
  if (n_to_select > 0) {
    # Select evenly spaced dates
    if (n_to_select == length(regime_dates)) {
      selected <- regime_dates
    } else {
      indices <- round(seq(1, length(regime_dates), length.out = n_to_select))
      selected <- regime_dates[indices]
    }
    
    selected_conferences[[regime_name]] <- selected
    
    # remove NAs from each list element
    selected_conferences<- selected_conferences |> 
      map(~ .x[!is.na(.x)])
    
    cat(sprintf("\n%s: Selected %d conferences\n", regime_name, length(selected)))
  }
}

# Combine all selected conferences
final_conference_dates <- sort(unique(unlist(selected_conferences))) |> as.Date()

print(paste("\nTOTAL CONFERENCES SELECTED:", length(final_conference_dates)))

# Save the selection for documentation
regime_df <- map2_df(selected_conferences, names(selected_conferences), 
                     ~data.frame(date = .x, regime = .y))
write_xlsx(regime_df, paste0("../intermediate_data/controlled_experiment_dates_", Sys.Date(), ".xlsx"))

# ============================================================================
# STEP 2: LOAD ECB TRANSCRIPTS IN PARALLEL
# ============================================================================

print("\nLoading ECB transcripts...")

# Function to get transcript for a specific date
get_ecb_transcript <- function(conference_date) {
  date_str <- as.character(conference_date)
  
  # Find the matching transcript file
  transcript_files <- list.files("../intermediate_data/texts/", 
                                pattern = paste0("^", date_str), 
                                full.names = TRUE)
  
  if (length(transcript_files) == 0) {
    return(NULL)
  }
  
  # Read the transcript
  text <- readtext(transcript_files[1])$text
  return(text)
}

# Load all transcripts in parallel
transcripts <- future_map(final_conference_dates, get_ecb_transcript, 
                         .options = furrr_options(seed = TRUE))
names(transcripts) <- as.character(final_conference_dates)

# Check how many we successfully loaded
valid_transcripts <- transcripts[!sapply(transcripts, is.null)]
print(paste("Successfully loaded", length(valid_transcripts), "transcripts"))

# Update final conference dates to only include those with transcripts
final_conference_dates <- as.Date(names(valid_transcripts))

# ============================================================================
# STEP 3: DEFINE GEMINI FUNCTION (CONSISTENT VERSION)
# ============================================================================

# Use EXACTLY the same function as your July run
new_gemini <- function(prompt, model = "2.5-flash", temperature = 1, maxOutputTokens = 1000000,
                       topK = 40, topP = 0.95, seed= 45) {
  
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
    contents = list(
      parts = list(
        list(text = prompt)
      )
    ),
    generationConfig = generation_config
  )
  
  req <- request(url) |>
    req_url_query(key = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_json(request_body) |>
    req_timeout(120)
  
  resp <- tryCatch(
    req_perform(req),
    error = function(e) {
      return(NULL)
    }
  )
  
  if (is.null(resp) || resp$status_code != 200) {
    return(NULL)
  }
  
  candidates <- resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}

# ============================================================================
# STEP 4: DEFINE PROMPTS
# ============================================================================

# Load your original prompt
source("create_prompts.R")

# Prompt WITH text (your original naive prompt)
prompt_with_text_template <- prompt_naive

# Prompt WITHOUT text (text-free version)
prompt_without_text_template <- "
Context:
You are simulating the Euro area interest rate swap market, composed of 30 individual traders.
These traders are making predictions after an ECB Governing Council meeting on [date].
Each trader makes a trading decision based on their characteristics and general knowledge.

Trader Characteristics:
Each trader has the following attributes:
- Risk Aversion: High / Medium / Low — determines sensitivity to uncertainty and preference for stability.
- Behavioral Biases (1–2 per trader): e.g., Confirmation Bias, Overconfidence, Anchoring, Herding, Loss Aversion, Recency Bias.
- Interpretation Style (1 per trader): e.g., Fundamentalist, Sentiment Reader, Quantitative, Skeptic, Narrative-Driven.

Task:
For the ECB meeting on [date], simulate trading decisions for 30 traders across three tenors.
You do NOT have access to the actual press conference transcript.
Use only your general economic knowledge right before this date.

For each tenor, the trader must:
- Provide an expected rate direction: Up / Down / Unchanged
- Provide a new expected swap rate (in percent, to two decimal places)
- Provide a confidence score (0-100%)

Output:
Provide a table with the following structure:

| Date       | Trader ID | Tenor   | Expected Direction | New Expected Rate (%) | Confidence (%) |
|------------|-----------|---------|--------------------|-----------------------|----------------|
| YYYY-MM-DD | T001      | 3M      | Up                 | 3.15                  | 65             |
| YYYY-MM-DD | T001      | 2Y      | Down               | 2.85                  | 80             |
| ...        | ...       | ...     | ...                | ...                   | ...            |

Guidelines:
- Use only information available as of [date].
- Output only a markdown table with the specified columns, no additional text.
"

# ============================================================================
# STEP 5: DEFINE PARALLEL PROCESSING FUNCTION
# ============================================================================

# Create directory for results
dir.create("../intermediate_data/controlled_experiment/", showWarnings = FALSE, recursive = TRUE)

# Function to run BOTH conditions for a single conference
run_both_conditions <- function(conference_date, transcript_text) {
  
  date_str <- as.character(conference_date)
  
  # Find regime
  regime_name <- NA
  for (rname in names(selected_conferences)) {
    if (conference_date %in% selected_conferences[[rname]]) {
      regime_name <- rname
      break
    }
  }
  
  # Generate unique session ID
  session_id <- paste0(date_str, "_", format(Sys.time(), "%H%M%S"), "_", sample(1000:9999, 1))
  
  # Initialize results
  results <- list(
    date = date_str,
    regime = regime_name,
    with_text = NULL,
    without_text = NULL
  )
  
  # Prepare prompts
  prompt_with <- gsub("\\[date\\]", date_str, prompt_with_text_template)
  full_prompt_with <- paste0(
    prompt_with,
    "\nPress Conference on ", date_str, "\n",
    "Text:", transcript_text, "\n\n"
  )
  
  prompt_without <- gsub("\\[date\\]", date_str, prompt_without_text_template)
  full_prompt_without <- paste0(
    prompt_without
  )

  # Run WITH text (with retries)
  for (attempt in 1:3) {
    results$with_text <- new_gemini(full_prompt_with, 
                                    model = "2.5-flash",
                                    temperature = 1)
    if (!is.null(results$with_text)) break
    Sys.sleep(2 * attempt)
  }
  
  # Small delay between requests
  Sys.sleep(2)
  
  # Run WITHOUT text (with retries)
  for (attempt in 1:3) {
    results$without_text <- new_gemini(full_prompt_without,
                                       model = "2.5-flash",
                                       temperature = 1)
    if (!is.null(results$without_text)) break
    Sys.sleep(2 * attempt)
  }
  
  # Save individual result
  saveRDS(results, paste0("../intermediate_data/controlled_experiment/", date_str, ".rds"))
  
  # Return status
  status <- paste0(
    date_str, " - ",
    ifelse(!is.null(results$with_text), "✅", "❌"), " WITH | ",
    ifelse(!is.null(results$without_text), "✅", "❌"), " WITHOUT"
  )
  
  cat(status, "\n")
  
  return(results)
}

# ============================================================================
# STEP 6: RUN CONTROLLED EXPERIMENT IN PARALLEL (NO BATCHING)
# ============================================================================

print("\n========== STARTING PARALLEL CONTROLLED EXPERIMENT ==========")
print(paste("Testing", length(final_conference_dates), "conferences"))
print(paste("Using", future::nbrOfWorkers(), "parallel workers"))

start_time <- Sys.time()

# Create a list of dates and their transcripts
conference_list <- map2(final_conference_dates, 
                        valid_transcripts[as.character(final_conference_dates)],
                        ~list(date = .x, transcript = .y))

# Run all conferences in parallel at once
all_results <- future_map(
  sample_conference_list,
  ~run_both_conditions(.x$date, .x$transcript),
  seed = 140,
  .options = furrr_options(seed = TRUE) # Ensure reproducibility of random seeds
)

end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")

print("\n========== PARALLEL EXPERIMENT COMPLETED ==========")
print(paste("Total time:", round(total_time, 2), "minutes"))

# Count successes
successful_both <- sum(sapply(all_results, function(x) !is.null(x$with_text) && !is.null(x$without_text)))
successful_with_only <- sum(sapply(all_results, function(x) !is.null(x$with_text) && is.null(x$without_text)))
successful_without_only <- sum(sapply(all_results, function(x) is.null(x$with_text) && !is.null(x$without_text)))
failed_both <- sum(sapply(all_results, function(x) is.null(x$with_text) && is.null(x$without_text)))

cat("\nResults summary:\n")
cat(paste("  Both conditions successful:", successful_both, "\n"))
cat(paste("  Only WITH text successful:", successful_with_only, "\n"))
cat(paste("  Only WITHOUT text successful:", successful_without_only, "\n"))
cat(paste("  Both failed:", failed_both, "\n"))

# Save final results
controlled_results <- all_results
names(controlled_results) <- sapply(all_results, function(x) x$date)

saveRDS(controlled_results, 
        paste0("../intermediate_data/controlled_results_final_", Sys.Date(), ".rds"))


# ============================================================================
# STEP 7: PARSE RESULTS IN PARALLEL
# ============================================================================

print("\n========== PARSING RESULTS ==========")

# Function to parse markdown table from LLM output
parse_llm_output <- function(output_text, date_str, regime) {
  if (is.null(output_text) || length(output_text) == 0) return(NULL)
  
  tryCatch({
    # Read as markdown table
    df <- read_delim(textConnection(output_text), 
                    delim = "|", 
                    trim_ws = TRUE, 
                    skip = 1,
                    show_col_types = FALSE) %>%
      select(-1, -ncol(.)) %>%
      slice(-1) %>%
      setNames(c("date", "id", "tenor", "direction", "rate", "confidence")) %>%
      mutate(
        date = date_str,
        regime = regime,
        rate = as.numeric(rate),
        confidence = as.numeric(confidence)
      ) %>%
      filter(tenor %in% c("3M", "2Y", "10Y"))
    
    return(df)
  }, error = function(e) {
    return(NULL)
  })
}

# Parse all results in parallel
parse_results <- future_map(controlled_results, function(result) {
  list(
    with_text = parse_llm_output(result$with_text, result$date, result$regime),
    without_text = parse_llm_output(result$without_text, result$date, result$regime)
  )
}, .options = furrr_options(seed = TRUE))

# Combine parsed results
df_with_text <- bind_rows(lapply(parse_results, function(x) x$with_text))
df_without_text <- bind_rows(lapply(parse_results, function(x) x$without_text))

print(paste("Parsed WITH text:", nrow(df_with_text), "predictions"))
print(paste("Parsed WITHOUT text:", nrow(df_without_text), "predictions"))

# ============================================================================
# STEP 8: COMPUTE DISAGREEMENT AND ANALYZE
# ============================================================================

print("\n========== COMPUTING DISAGREEMENT ==========")

# Compute disagreement for WITH text
disagreement_with <- df_with_text %>%
  group_by(date, tenor, regime) %>%
  summarise(
    std_with_text = sd(rate, na.rm = TRUE),
    mean_with_text = mean(rate, na.rm = TRUE),
    n_agents = n(),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(date))

# Compute disagreement for WITHOUT text  
disagreement_without <- df_without_text %>%
  group_by(date, tenor, regime) %>%
  summarise(
    std_without_text = sd(rate, na.rm = TRUE),
    mean_without_text = mean(rate, na.rm = TRUE),
    n_agents = n(),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(date))

# Load market volatility
market_vol <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(
    tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor),
    date = as.Date(date)
  ) %>%
  select(date, tenor, market_volatility = correct_post_mean)

# Combine all measures
final_comparison <- disagreement_with %>%
  inner_join(disagreement_without, by = c("date", "tenor", "regime")) %>%
  left_join(market_vol, by = c("date", "tenor")) %>%
  filter(!is.na(market_volatility))

print(paste("Final dataset:", nrow(final_comparison), "observations"))

# ============================================================================
# STEP 9: ANALYZE RESULTS
# ============================================================================

print("\n========== ANALYSIS ==========")

# Overall correlations
overall_results <- final_comparison %>%
  group_by(tenor) %>%
  summarise(
    n = n(),
    corr_with_text = cor(std_with_text, market_volatility, 
                         method = "spearman", use = "complete.obs"),
    corr_without_text = cor(std_without_text, market_volatility,
                           method = "spearman", use = "complete.obs"),
    information_value = corr_with_text - corr_without_text,
    percent_change = (corr_with_text - corr_without_text) / abs(corr_without_text) * 100,
    .groups = "drop"
  )

print("\nOVERALL RESULTS:")
print(overall_results)

# Results by regime
regime_results <- final_comparison %>%
  group_by(regime, tenor) %>%
  filter(n() >= 3) %>%  # Only calculate correlation if at least 3 observations
  summarise(
    n = n(),
    corr_with_text = cor(std_with_text, market_volatility,
                        method = "spearman", use = "complete.obs"),
    corr_without_text = cor(std_without_text, market_volatility,
                           method = "spearman", use = "complete.obs"),
    information_value = corr_with_text - corr_without_text,
    .groups = "drop"
  )

print("\nRESULTS BY REGIME:")
print(regime_results)

# Crisis vs normal periods
crisis_periods <- c("Financial_crisis", "Sovereign_debt_crisis", "Pandemic", "Inflation_surge", "Aggressive_tightening")

crisis_results <- final_comparison %>%
  mutate(is_crisis = regime %in% crisis_periods) %>%
  group_by(is_crisis, tenor) %>%
  summarise(
    n = n(),
    corr_with_text = cor(std_with_text, market_volatility,
                        method = "spearman", use = "complete.obs"),
    corr_without_text = cor(std_without_text, market_volatility,
                           method = "spearman", use = "complete.obs"),
    information_value = corr_with_text - corr_without_text,
    .groups = "drop"
  )

print("\nCRISIS VS NORMAL:")
print(crisis_results)

# ============================================================================
# STEP 10: SAVE RESULTS AND CLOSE PARALLEL PROCESSING
# ============================================================================

# Save all results
write_xlsx(
  list(
    "overall" = overall_results,
    "by_regime" = regime_results,
    "crisis_analysis" = crisis_results,
    "raw_comparison" = final_comparison,
    "with_text_raw" = df_with_text,
    "without_text_raw" = df_without_text
  ),
  paste0("../output/controlled_experiment_results_", Sys.Date(), ".xlsx")
)

# Close parallel processing
plan(sequential)

print("\n========== CONTROLLED EXPERIMENT COMPLETE ==========")
print(paste("Results saved to: ../output/controlled_experiment_results_", Sys.Date(), ".xlsx"))
print(paste("Total execution time:", round(total_time, 2), "minutes"))

# Final interpretation
cat("\n=== INTERPRETATION ===\n")
for (i in 1:nrow(overall_results)) {
  tenor <- overall_results$tenor[i]
  info_val <- overall_results$information_value[i]
  
  cat(sprintf("\n%s Tenor:\n", tenor))
  cat(sprintf("  With text: %.3f\n", overall_results$corr_with_text[i]))
  cat(sprintf("  Without text: %.3f\n", overall_results$corr_without_text[i]))
  cat(sprintf("  Information value: %.3f (%.1f%% change)\n", 
              info_val, overall_results$percent_change[i]))
  
  if (info_val > 0.1) {
    cat("  ✅ Text provides substantial information\n")
  } else if (info_val > 0) {
    cat("  ⚠️ Text provides modest information\n")
  } else {
    cat("  ❌ Text does not improve predictions\n")
  }
}