# Cross-LLM Robustness Testing - Modular Approach
# ================================================
# Each step is completely separate for easier debugging

# Load libraries ----
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  openai, httr2, crayon, stringr, purrr, readr, writexl, readxl, 
  tidyverse, future, furrr, psych, glue, RColorBrewer, showtext
)

# Configuration ----
setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# API keys from .Renviron
Sys.setenv(OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY"))

TEMPERATURE <- 1.0
TARGET_TENORS <- c("3M", "2Y", "10Y")
MAX_WORKERS <- 2

# Directories
OUTPUT_DIR <- "../intermediate_data/cross_llm_analysis"
RESULTS_DIR <- "../output/cross_llm_results"
RAW_RESPONSES_DIR <- file.path(OUTPUT_DIR, "raw_responses")

walk(c(OUTPUT_DIR, RESULTS_DIR, RAW_RESPONSES_DIR), ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# =============================================================================
# STEP 1: API FUNCTIONS (completely separate)
# =============================================================================

call_chatgpt <- function(prompt, user_message, temperature = 1.0) {
  tryCatch({
    result <- create_chat_completion(
      model = "gpt-5-mini",
      messages = list(
        list(role = "system", content = prompt),
        list(role = "user", content = user_message)
      ),
      temperature = temperature
    )
    return(result$choices$message.content)
  }, error = function(e) {
    cat(crayon::red(glue("ChatGPT API Error: {e$message}\n")))
    return(NULL)
  })
}

call_deepseek <- function(prompt, user_message, temperature = 1.0) {
  api_key <- Sys.getenv("DEEPSEEK_API_KEY")
  if (api_key == "") {
    cat(crayon::red("DEEPSEEK_API_KEY not found in environment\n"))
    return(NULL)
  }
  
  tryCatch({
    request_body <- list(
      model = "deepseek-v3",
      messages = list(
        list(role = "system", content = prompt),
        list(role = "user", content = user_message)
      ),
      temperature = temperature,
      max_tokens = 1000000
    )
    
    response <- request("https://api.deepseek.com/chat/completions") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(request_body) |>
      req_timeout(120) |>
      req_perform() |>
      resp_body_json()
    
    return(response$choices[[1]]$message$content)
  }, error = function(e) {
    cat(crayon::red(glue("DeepSeek API Error: {e$message}\n")))
    return(NULL)
  })
}

call_llm <- function(model_name, prompt, user_message, temperature = 1.0) {
  switch(model_name,
    "chatgpt" = call_chatgpt(prompt, user_message, temperature),
    "deepseek" = call_deepseek(prompt, user_message, temperature),
    {
      cat(crayon::red(glue("Unknown model: {model_name}\n")))
      return(NULL)
    }
  )
}

# =============================================================================
# STEP 2: DATA LOADING (completely separate)
# =============================================================================

load_transcripts <- function() {
  cat(crayon::blue("📖 Loading ECB transcripts...\n"))
  
  transcript_files <- list.files("../intermediate_data/texts/", pattern = "\\.txt$", full.names = TRUE)
  
  if (length(transcript_files) == 0) {
    cat(crayon::red("❌ No transcript files found\n"))
    return(tibble())
  }
  
  transcript_data <- transcript_files %>%
    map_dfr(~{
      filename <- basename(.x)
      date_match <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
      
      if (is.na(date_match)) {
        cat(crayon::yellow(glue("⚠️ Skipping file with invalid date format: {filename}\n")))
        return(tibble())
      }
      
      tryCatch({
        text <- read_lines(.x) %>% paste(collapse = " ")
        tibble(date = as.Date(date_match), filename = filename, text = text)
      }, error = function(e) {
        cat(crayon::red(glue("❌ Error reading {filename}: {e$message}\n")))
        return(tibble())
      })
    }) %>%
    arrange(date)
  
  cat(crayon::green(glue("✅ Loaded {nrow(transcript_data)} transcripts\n")))
  return(transcript_data)
}

load_gemini_results <- function() {
  cat(crayon::blue("📊 Loading existing Gemini results...\n"))
  
  tryCatch({
    gemini_file <- "../intermediate_data/aggregate_gemini_result/prompt_naive/2.5flash_2025-07-21.xlsx"
    
    if (!file.exists(gemini_file)) {
      cat(crayon::yellow("⚠️ Gemini file not found\n"))
      return(tibble())
    }
    
    gemini_data <- read_xlsx(gemini_file) %>%
      mutate(
        model_name = "gemini",
        conference_date = as.character(date),
        tenor = case_when(
          tenor == "3M" ~ "3M",
          tenor == "2Y" ~ "2Y", 
          tenor == "10Y" ~ "10Y",
          TRUE ~ tenor
        )
      ) %>%
      filter(tenor %in% TARGET_TENORS) %>%
      select(conference_date, model_name, tenor, rate, trader_id = id, direction, confidence)
    
    cat(crayon::green(glue("✅ Loaded {nrow(gemini_data)} Gemini predictions\n")))
    return(gemini_data)
    
  }, error = function(e) {
    cat(crayon::red(glue("❌ Error loading Gemini data: {e$message}\n")))
    return(tibble())
  })
}

load_market_data <- function() {
  cat(crayon::blue("📈 Loading market volatility data...\n"))
  
  tryCatch({
    market_data <- readRDS("../intermediate_data/range_difference_df.rds") %>%
      mutate(
        conference_date = as.character(as.Date(date)),
        tenor = case_when(
          tenor == "3mnt" ~ "3M",
          tenor %in% c("2Y", "10Y") ~ tenor,
          TRUE ~ tenor
        )
      ) %>%
      filter(tenor %in% TARGET_TENORS) %>%
      select(conference_date, tenor, market_volatility = correct_post_mean) %>%
      filter(!is.na(market_volatility))
    
    cat(crayon::green(glue("✅ Loaded market data: {nrow(market_data)} observations\n")))
    return(market_data)
    
  }, error = function(e) {
    cat(crayon::red(glue("❌ Error loading market data: {e$message}\n")))
    return(NULL)
  })
}

# =============================================================================
# STEP 3: RAW LLM CALLING & SAVING (completely separate from parsing)
# =============================================================================

call_and_save_llm_response <- function(model_name, conference_date, conference_text, analyst_prompt) {
  cat(crayon::cyan(glue("🔄 Calling {model_name} for {conference_date}\n")))
  
  # Check if raw response already exists
  raw_file <- file.path(RAW_RESPONSES_DIR, glue("{model_name}_{conference_date}_raw.rds"))
  
  if (file.exists(raw_file)) {
    cat(crayon::blue(glue("📁 Raw response exists for {model_name} {conference_date}\n")))
    return(TRUE)
  }
  
  # Prepare prompt and message
  formatted_prompt <- str_replace_all(analyst_prompt, "\\[date\\]", conference_date)
  user_message <- glue("ECB press conference for {conference_date}:\n\n{conference_text}\n\n")
  
  # Try API call with retries
  response <- NULL
  for (attempt in 1:3) {
    if (attempt > 1) {
      Sys.sleep(2^(attempt-1))
      cat(crayon::yellow(glue("⏳ Retry {attempt} for {model_name} {conference_date}\n")))
    }
    
    response <- call_llm(model_name, formatted_prompt, user_message, TEMPERATURE)
    
    if (!is.null(response) && nzchar(response)) {
      # Save raw response immediately
      saveRDS(response, raw_file)
      cat(crayon::green(glue("💾 Raw response saved: {model_name} {conference_date}\n")))
      return(TRUE)
    }
    
    cat(crayon::red(glue("❌ Attempt {attempt} failed for {model_name} {conference_date}\n")))
  }
  
  cat(crayon::red(glue("❌ All attempts failed for {model_name} {conference_date}\n")))
  return(FALSE)
}

# Batch function to call all models for all conferences
call_all_llm_responses <- function(transcript_data, models = c("chatgpt", "deepseek"), limit_conferences = NULL) {
  # Load prompt
  source("create_prompts.R")
  analyst_prompt <- prompt_naive
  
  # Optionally limit conferences
  if (!is.null(limit_conferences)) {
    transcript_data <- head(transcript_data, limit_conferences)
    cat(crayon::yellow(glue("⚠️ Limited to {limit_conferences} conferences\n")))
  }
  
  cat(crayon::blue(glue("🚀 Calling LLMs for {nrow(transcript_data)} conferences\n")))
  
  # Create all combinations
  call_grid <- expand_grid(
    model_name = models,
    conference_idx = 1:nrow(transcript_data)
  )
  
  results <- call_grid %>%
    pmap_lgl(function(model_name, conference_idx) {
      conf_info <- transcript_data[conference_idx, ]
      
      # Small delay to avoid rate limits
      Sys.sleep(1)
      
      call_and_save_llm_response(
        model_name = model_name,
        conference_date = as.character(conf_info$date),
        conference_text = conf_info$text,
        analyst_prompt = analyst_prompt
      )
    })
  
  success_count <- sum(results)
  total_count <- length(results)
  
  cat(crayon::green(glue("✅ Completed LLM calls: {success_count}/{total_count} successful\n")))
  
  return(tibble(
    model_name = call_grid$model_name,
    conference_idx = call_grid$conference_idx,
    success = results
  ))
}

# =============================================================================
# STEP 4: PARSING (completely separate from API calling)
# =============================================================================

parse_markdown_table <- function(markdown_string) {
  if (is.null(markdown_string) || is.na(markdown_string) || nzchar(markdown_string) == FALSE) {
    return(NULL)
  }
  
  # Split into lines and clean
  lines <- str_split_1(markdown_string, "\n") %>% str_trim() %>% keep(~nzchar(.x))
  
  if (length(lines) < 3) {
    return(NULL)
  }
  
  # Find table separator (line with dashes and pipes)
  separator_indices <- which(str_detect(lines, "^\\|[\\s\\-\\|]+\\|$"))
  
  if (length(separator_indices) == 0) {
    return(NULL)
  }
  
  separator_idx <- separator_indices[1]
  header_idx <- separator_idx - 1
  
  if (header_idx < 1) {
    return(NULL)
  }
  
  # Extract headers
  headers <- lines[header_idx] %>%
    str_remove_all("^\\||\\|$") %>%
    str_split_1("\\|") %>%
    str_trim() %>%
    keep(~nzchar(.x))
  
  if (length(headers) == 0) {
    return(NULL)
  }
  
  # Extract data rows
  data_lines <- lines[(separator_idx + 1):length(lines)] %>%
    keep(~str_detect(.x, "^\\|.*\\|$"))
  
  if (length(data_lines) == 0) {
    return(NULL)
  }
  
  tryCatch({
    # Parse each row
    parsed_data <- data_lines %>%
      map(~{
        cells <- .x %>%
          str_remove_all("^\\||\\|$") %>%
          str_split_1("\\|") %>%
          str_trim() %>%
          keep(~nzchar(.x))
        
        if (length(cells) == length(headers)) {
          return(cells)
        } else {
          return(NULL)
        }
      }) %>%
      discard(is.null)
    
    if (length(parsed_data) == 0) {
      return(NULL)
    }
    
    # Create dataframe
    df <- map_dfr(parsed_data, ~set_names(.x, headers))
    
    # Find rate column and convert
    rate_col <- names(df) %>% str_subset("(?i)rate.*%|expected.*rate") %>% first()
    
    if (!is.null(rate_col)) {
      df <- df %>%
        mutate(rate = as.numeric(.data[[rate_col]])) %>%
        select(-all_of(rate_col))
    }
    
    # Standardize column names
    df <- df %>%
      rename_with(~case_when(
        str_detect(.x, "(?i)tenor") ~ "tenor",
        str_detect(.x, "(?i)trader.*id|id") ~ "trader_id",
        str_detect(.x, "(?i)direction") ~ "direction", 
        str_detect(.x, "(?i)confidence") ~ "confidence",
        TRUE ~ .x
      )) %>%
      filter(if("tenor" %in% names(.)) tenor %in% TARGET_TENORS else TRUE)
    
    return(df)
    
  }, error = function(e) {
    cat(crayon::red(glue("Parse error: {e$message}\n")))
    return(NULL)
  })
}

parse_single_response <- function(model_name, conference_date) {
  raw_file <- file.path(RAW_RESPONSES_DIR, glue("{model_name}_{conference_date}_raw.rds"))
  
  if (!file.exists(raw_file)) {
    cat(crayon::red(glue("❌ Raw file not found: {model_name} {conference_date}\n")))
    return(NULL)
  }
  
  # Load raw response
  raw_response <- tryCatch({
    readRDS(raw_file)
  }, error = function(e) {
    cat(crayon::red(glue("❌ Error reading raw file {model_name} {conference_date}: {e$message}\n")))
    return(NULL)
  })
  
  if (is.null(raw_response)) {
    return(NULL)
  }
  
  # Parse the response
  parsed_df <- parse_markdown_table(raw_response)
  
  if (is.null(parsed_df) || nrow(parsed_df) == 0) {
    cat(crayon::yellow(glue("⚠️ Failed to parse {model_name} {conference_date}\n")))
    return(NULL)
  }
  
  # Add metadata
  result <- parsed_df %>%
    mutate(
      conference_date = conference_date,
      model_name = model_name
    )
  
  cat(crayon::green(glue("✅ Parsed {model_name} {conference_date}: {nrow(result)} rows\n")))
  
  return(result)
}

parse_all_responses <- function(transcript_data, models = c("chatgpt", "deepseek")) {
  cat(crayon::blue("🔍 Parsing all raw responses...\n"))
  
  # Get all combinations that should exist
  parse_grid <- expand_grid(
    model_name = models,
    conference_date = as.character(transcript_data$date)
  )
  
  # Parse each response
  parsed_results <- parse_grid %>%
    pmap(function(model_name, conference_date) {
      parse_single_response(model_name, conference_date)
    }) %>%
    discard(is.null)
  
  if (length(parsed_results) == 0) {
    cat(crayon::red("❌ No responses could be parsed\n"))
    return(tibble())
  }
  
  # Combine all results
  combined_results <- bind_rows(parsed_results)
  
  cat(crayon::green(glue("✅ Parsing complete: {nrow(combined_results)} total rows from {length(parsed_results)} responses\n")))
  
  return(combined_results)
}

# =============================================================================
# STEP 5: ANALYSIS FUNCTIONS (separate from data generation)
# =============================================================================

compute_disagreement_measures <- function(results_df) {
  cat(crayon::blue("📊 Computing disagreement measures...\n"))
  
  disagreement_df <- results_df %>%
    filter(!is.na(rate)) %>%
    group_by(model_name, conference_date, tenor) %>%
    summarise(
      predicted_sd = sd(rate, na.rm = TRUE),
      predicted_mean = mean(rate, na.rm = TRUE),
      n_traders = n(),
      .groups = "drop"
    ) %>%
    filter(n_traders >= 10, !is.na(predicted_sd))
  
  cat(crayon::green(glue("✅ Disagreement computed: {nrow(disagreement_df)} observations\n")))
  
  return(disagreement_df)
}

compute_correlations <- function(disagreement_df, market_data_df) {
  cat(crayon::blue("📈 Computing correlations with market data...\n"))
  
  merged_df <- disagreement_df %>%
    inner_join(market_data_df, by = c("conference_date", "tenor"))
  
  if (nrow(merged_df) == 0) {
    cat(crayon::red("❌ No matching data between disagreement and market measures\n"))
    return(NULL)
  }
  
  cat(crayon::green(glue("✅ Merged data: {nrow(merged_df)} observations\n")))
  
  # Compute correlations by model and tenor
  correlation_results <- merged_df %>%
    group_by(model_name, tenor) %>%
    filter(n() >= 5) %>%
    do({
      spear_test <- cor.test(.$predicted_sd, .$market_volatility, method = "spearman")
      tibble(
        n_observations = nrow(.),
        spearman_correlation = spear_test$estimate,
        spearman_p_value = spear_test$p.value
      )
    }) %>%
    ungroup()
  
  # Model averages
  model_averages <- correlation_results %>%
    group_by(model_name) %>%
    summarise(
      avg_spearman_correlation = mean(spearman_correlation, na.rm = TRUE),
      total_observations = sum(n_observations),
      n_tenors = n(),
      .groups = "drop"
    )
  
  cat(crayon::green("✅ Correlations computed\n"))
  
  return(list(
    detailed = correlation_results,
    averages = model_averages,
    merged_data = merged_df
  ))
}

# =============================================================================
# STEP 6: MAIN EXECUTION FUNCTIONS
# =============================================================================

# Step-by-step execution
step1_load_data <- function() {
  cat(crayon::magenta("=== STEP 1: Loading Data ===\n"))
  
  transcript_data <- load_transcripts()
  gemini_data <- load_gemini_results()
  market_data <- load_market_data()
  
  return(list(
    transcripts = transcript_data,
    gemini = gemini_data,
    market = market_data
  ))
}

step2_call_llms <- function(transcript_data, models = c("chatgpt"), limit_conferences = 3) {
  cat(crayon::magenta("=== STEP 2: Calling LLMs ===\n"))
  
  call_results <- call_all_llm_responses(
    transcript_data = transcript_data,
    models = models,
    limit_conferences = limit_conferences
  )
  
  return(call_results)
}

step3_parse_responses <- function(transcript_data, models = c("chatgpt")) {
  cat(crayon::magenta("=== STEP 3: Parsing Responses ===\n"))
  
  parsed_results <- parse_all_responses(
    transcript_data = transcript_data,
    models = models
  )
  
  return(parsed_results)
}

step4_analyze_results <- function(parsed_results, gemini_data, market_data) {
  cat(crayon::magenta("=== STEP 4: Analysis ===\n"))
  
  # Combine all results
  all_results <- bind_rows(parsed_results, gemini_data)
  
  # Compute disagreement
  disagreement_df <- compute_disagreement_measures(all_results)
  
  # Compute correlations
  correlation_results <- compute_correlations(disagreement_df, market_data)
  
  return(list(
    all_results = all_results,
    disagreement = disagreement_df,
    correlations = correlation_results
  ))
}

# =============================================================================
# EXECUTION EXAMPLE
# =============================================================================

if (interactive()) {
  cat(crayon::blue("🚀 Cross-LLM Analysis - Modular Approach\n"))
  cat(crayon::blue("Run each step separately:\n\n"))
  
  cat("# Step 1: Load data\n")
data <- step1_load_data()
  
  cat("# Step 2: Call LLMs (limited to 3 conferences for testing)\n") 
call_results <- step2_call_llms(data$transcripts, models = c('chatgpt'), limit_conferences = 3)
  
  cat("# Step 3: Parse responses\n"))\n\n")
  cat("parsed <- step3_parse_responses(data$transcripts, models = c('chatgpt'))\n\n")
  
  cat("# Step 4: Analyze\n")
  cat("analysis <- step4_analyze_results(parsed, data$gemini, data$market)\n\n")
  
  cat("# Check results\n")
  cat("print(analysis$correlations$detailed)\n")
}

