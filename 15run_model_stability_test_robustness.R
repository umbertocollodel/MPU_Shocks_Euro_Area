# Cross-LLM Robustness Testing - Updated with Claude Integration
# ================================================================
# Each step is completely separate for easier debugging
#
# IMPORTANT: Run this script from the 'code/' directory
# ================================================================

# Load libraries ----
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  openai, httr2, crayon, stringr, purrr, readr, writexl, readxl,
  tidyverse, future, furrr, psych, glue, RColorBrewer, showtext
)

# Configuration ----
# Verify we're in the correct directory
if (!file.exists("../intermediate_data")) {
  stop("Please run this script from the 'code/' directory.\n",
       "Current directory: ", getwd())
}

# API keys from .Renviron - Make sure to add ANTHROPIC_API_KEY to your .Renviron file
Sys.setenv(OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY"))
Sys.setenv(CLAUDE_API_KEY = Sys.getenv("CLAUDE_API_KEY"))

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
      model = "gpt-5-mini",  # Updated to use gpt-4o-mini instead of non-existent gpt-5-mini
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

call_claude <- function(prompt, user_message, temperature = 1.0) {
  api_key <- Sys.getenv("CLAUDE_API_KEY")
  if (api_key == "") {
    cat(crayon::red("ANTHROPIC_API_KEY not found in environment\n"))
    return(NULL)
  }
  
  tryCatch({
    # Combine system prompt and user message for Claude
    combined_message <- paste0(
      "System: ", prompt, "\n\n",
      "Human: ", user_message, "\n\nAssistant: "
    )
    
    request_body <- list(
      model = "claude-sonnet-4-20250514",  # Latest Claude model
      max_tokens = 8192,  # Claude requires max_tokens parameter
      temperature = temperature,
      messages = list(
        list(
          role = "user",
          content = combined_message
        )
      )
    )
    
    response <- request("https://api.anthropic.com/v1/messages") |>
      req_headers(
        "x-api-key" = api_key,
        "Content-Type" = "application/json",
        "anthropic-version" = "2023-06-01"  # Required header for Claude API
      ) |>
      req_body_json(request_body) |>
      req_timeout(120) |>
      req_perform() |>
      resp_body_json()
    
    # Extract text content from Claude's response format
    if (!is.null(response$content) && length(response$content) > 0) {
      return(response$content[[1]]$text)
    } else {
      cat(crayon::red("Claude API returned empty content\n"))
      return(NULL)
    }
    
  }, error = function(e) {
    cat(crayon::red(glue("Claude API Error: {e$message}\n")))
    return(NULL)
  })
}

call_llm <- function(model_name, prompt, user_message, temperature = 1.0) {
  switch(model_name,
    "chatgpt" = call_chatgpt(prompt, user_message, temperature),
    "claude" = call_claude(prompt, user_message, temperature),
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
call_all_llm_responses <- function(transcript_data, models = c("chatgpt", "claude"), limit_conferences = NULL) {
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
      
   # Model-specific delays to avoid rate limits
      delay_seconds <- switch(model_name,
        "claude" = 15,    # Claude needs more time between requests
        "chatgpt" = 1,   # ChatGPT is more permissive
        2                # Default fallback
      )

     Sys.sleep(delay_seconds)

      
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
# Fixed Cross-LLM Parsing Functions with Better Error Handling
# ==============================================================

# Fixed Cross-LLM Parsing Functions with Better Error Handling
# ==============================================================

# Fixed Cross-LLM Parsing Functions with Better Error Handling
# ==============================================================

parse_markdown_table <- function(markdown_string) {
  if (is.null(markdown_string) || is.na(markdown_string) || nzchar(markdown_string) == FALSE) {
    cat(crayon::yellow("⚠️ Empty or null markdown string\n"))
    return(NULL)
  }
  
  # Handle case where markdown_string is a vector
  if (is.vector(markdown_string) && length(markdown_string) == 1) {
    text_content <- markdown_string[1]
  } else {
    text_content <- paste(markdown_string, collapse = "\n")
  }
  
  # Debug: Show first few lines of content
  lines_preview <- str_split_1(text_content, "\n")[1:min(10, length(str_split_1(text_content, "\n")))]
  cat(crayon::cyan("📄 First few lines of content:\n"))
  walk(lines_preview, ~cat(crayon::cyan(paste("  ", .x, "\n"))))
  
  # Split into lines and clean
  lines <- str_split_1(text_content, "\n") %>% str_trim()
  
  # Keep only non-empty lines
  lines <- lines[nzchar(lines)]
  
  if (length(lines) < 3) {
    cat(crayon::yellow(glue("⚠️ Too few lines ({length(lines)})\n")))
    return(NULL)
  }
  
  cat(crayon::cyan(glue("📋 Total lines after cleaning: {length(lines)}\n")))
  
  # More flexible separator detection - look for lines with multiple dashes and pipes
  separator_patterns <- c(
    "^\\|[\\s\\-\\|]+\\|$",           # Standard markdown separator
    "^[\\|\\s]*[-]+[\\|\\s\\-]*$",   # Looser separator pattern
    "^\\|.*[-]{2,}.*\\|$"            # At least 2 dashes with pipes
  )
  
  separator_indices <- c()
  for (pattern in separator_patterns) {
    separator_indices <- which(str_detect(lines, pattern))
    if (length(separator_indices) > 0) {
      cat(crayon::green(glue("✅ Found separator with pattern: {pattern}\n")))
      break
    }
  }
  
  if (length(separator_indices) == 0) {
    cat(crayon::yellow("⚠️ No separator found, showing all lines:\n"))
    iwalk(lines, ~cat(crayon::yellow(glue("  Line {.y}: {.x}\n"))))
    return(NULL)
  }
  
  separator_idx <- separator_indices[1]
  header_idx <- separator_idx - 1
  
  if (header_idx < 1) {
    cat(crayon::red("❌ Invalid header index\n"))
    return(NULL)
  }
  
  cat(crayon::cyan(glue("📍 Header line {header_idx}: {lines[header_idx]}\n")))
  cat(crayon::cyan(glue("📍 Separator line {separator_idx}: {lines[separator_idx]}\n")))
  
  # Extract headers with more flexible parsing
  header_line <- lines[header_idx]
  headers <- header_line %>%
    str_remove_all("^\\|+|\\|+$") %>%  # Remove leading/trailing pipes
    str_split_1("\\|") %>%
    str_trim() %>%
    keep(~nzchar(.x))
  
  if (length(headers) == 0) {
    cat(crayon::red("❌ No headers found\n"))
    return(NULL)
  }
  
  cat(crayon::green(glue("✅ Found {length(headers)} headers: {paste(headers, collapse = ', ')}\n")))
  
  # Extract data rows - be more flexible about what constitutes a data row
  data_lines <- lines[(separator_idx + 1):length(lines)] %>%
    keep(~str_detect(.x, "\\|")) %>%  # Any line with at least one pipe
    keep(~!str_detect(.x, "^\\|[\\s\\-\\|]+\\|$"))  # But not separator lines
  
  if (length(data_lines) == 0) {
    cat(crayon::yellow("⚠️ No data lines found after separator\n"))
    return(NULL)
  }
  
  cat(crayon::green(glue("✅ Found {length(data_lines)} data lines\n")))
  
  # Show first few data lines for debugging
  cat(crayon::cyan("📄 First few data lines:\n"))
  walk(head(data_lines, 3), ~cat(crayon::cyan(paste("  ", .x, "\n"))))
  
  tryCatch({
    # Parse each row with more robust error handling
    parsed_data <- data_lines %>%
      map(~{
        cells <- .x %>%
          str_remove_all("^\\|+|\\|+$") %>%  # Remove leading/trailing pipes
          str_split_1("\\|") %>%
          str_trim()
        
        # Filter out completely empty cells
        cells <- cells[nzchar(cells)]
        
        # Handle cases where there are missing cells by padding with NA
        if (length(cells) < length(headers)) {
          cells <- c(cells, rep(NA_character_, length(headers) - length(cells)))
        } else if (length(cells) > length(headers)) {
          # If too many cells, take only the first n
          cells <- cells[1:length(headers)]
        }
        
        return(cells)
      }) %>%
      # Remove any NULL results
      discard(is.null) %>%
      # Filter out rows that don't have enough data
      keep(~length(.x) >= length(headers))
    
    if (length(parsed_data) == 0) {
      cat(crayon::yellow("⚠️ No valid data rows after parsing\n"))
      return(NULL)
    }
    
    cat(crayon::green(glue("✅ Successfully parsed {length(parsed_data)} data rows\n")))
    
    # Create dataframe
    df <- map_dfr(parsed_data, ~set_names(.x, headers))
    
    cat(crayon::green(glue("✅ Created dataframe with {nrow(df)} rows and {ncol(df)} columns\n")))
    
    # Find rate column and convert - be more flexible with column names
    rate_cols <- names(df) %>% 
      str_subset("(?i)rate.*%?|expected.*rate|new.*expected.*rate")
    
    cat(crayon::cyan(glue("🔍 Potential rate columns: {paste(rate_cols, collapse = ', ')}\n")))
    
    rate_col <- rate_cols[1]  # Take first match
    
    if (!is.null(rate_col) && !is.na(rate_col)) {
      cat(crayon::green(glue("✅ Using rate column: {rate_col}\n")))
      df <- df %>%
        mutate(
          rate = as.numeric(str_remove_all(.data[[rate_col]], "[^0-9.-]"))
        ) %>%
        select(-all_of(rate_col))
    } else {
      cat(crayon::yellow("⚠️ No rate column found\n"))
    }
    
    # Find confidence column if it exists
    confidence_cols <- names(df) %>% 
      str_subset("(?i)confidence")
    
    confidence_col <- confidence_cols[1]
    
    if (!is.null(confidence_col) && !is.na(confidence_col)) {
      cat(crayon::green(glue("✅ Using confidence column: {confidence_col}\n")))
      df <- df %>%
        mutate(
          confidence_numeric = as.numeric(str_remove_all(.data[[confidence_col]], "[^0-9.-]"))
        ) %>%
        select(-all_of(confidence_col)) %>%
        rename(confidence = confidence_numeric)
    }
    
    # Standardize column names more robustly
    df <- df %>%
      rename_with(~case_when(
        str_detect(.x, "(?i)tenor") ~ "tenor",
        str_detect(.x, "(?i)trader.*id|^id$") ~ "trader_id",
        str_detect(.x, "(?i)direction") ~ "direction", 
        str_detect(.x, "(?i)date") ~ "date",
        TRUE ~ .x
      )) %>%
      # Standardize tenor values
      mutate(
        tenor = case_when(
          str_detect(toupper(tenor), "3M") ~ "3M",
          str_detect(toupper(tenor), "2Y") ~ "2Y", 
          str_detect(toupper(tenor), "10Y") ~ "10Y",
          TRUE ~ toupper(tenor)
        )
      ) %>%
      # Filter for valid tenors if TARGET_TENORS is defined
      {if(exists("TARGET_TENORS")) filter(., tenor %in% TARGET_TENORS) else .}
    
    cat(crayon::green(glue("✅ Final dataframe: {nrow(df)} rows, columns: {paste(names(df), collapse = ', ')}\n")))
    
    return(df)
    
  }, error = function(e) {
    cat(crayon::red(glue("❌ Parse error: {e$message}\n")))
    cat(crayon::red("Stack trace:\n"))
    traceback()
    return(NULL)
  })
}

parse_single_response <- function(model_name, conference_date) {
  raw_file <- file.path(RAW_RESPONSES_DIR, glue("{model_name}_{conference_date}_raw.rds"))
  
  cat(crayon::cyan(glue("🔍 Parsing {model_name} {conference_date}\n")))
  
  if (!file.exists(raw_file)) {
    cat(crayon::red(glue("❌ Raw file not found: {raw_file}\n")))
    return(NULL)
  }
  
  # Load raw response
  raw_response <- tryCatch({
    response <- readRDS(raw_file)
    cat(crayon::green(glue("✅ Loaded raw response, type: {class(response)}, length: {length(response)}\n")))
    response
  }, error = function(e) {
    cat(crayon::red(glue("❌ Error reading raw file {model_name} {conference_date}: {e$message}\n")))
    return(NULL)
  })
  
  if (is.null(raw_response)) {
    cat(crayon::red("❌ Raw response is NULL\n"))
    return(NULL)
  }
  
  # Debug: show first few characters of raw response
  response_preview <- substr(paste(raw_response, collapse = " "), 1, 200)
  cat(crayon::cyan(glue("📄 Response preview: {response_preview}...\n")))
  
  # Parse the response
  parsed_df <- parse_markdown_table(raw_response)
  
  if (is.null(parsed_df) || nrow(parsed_df) == 0) {
    cat(crayon::yellow(glue("⚠️ Failed to parse {model_name} {conference_date}\n")))
    return(NULL)
  }
  
  # Add metadata and ensure consistent column structure
  result <- parsed_df %>%
    mutate(
      conference_date = as.character(conference_date),
      model_name = model_name,
      # Add date column if missing (use conference_date)
      date = if("date" %in% names(.)) as.character(date) else as.character(conference_date),
      # Ensure trader_id is consistent - handle after column renaming
      id = if("trader_id" %in% names(.)) trader_id else paste0("T", str_pad(row_number(), 3, pad = "0"))
    ) %>%
    # Ensure required columns exist
    {
      df_temp <- .
      required_cols <- c("date", "id", "tenor", "direction", "rate")
      missing_cols <- setdiff(required_cols, names(df_temp))
      
      if (length(missing_cols) > 0) {
        cat(crayon::yellow(glue("⚠️ Missing required columns: {paste(missing_cols, collapse = ', ')}\n")))
        for (col in missing_cols) {
          df_temp[[col]] <- NA
        }
      }
      df_temp
    } %>%
    # Standardize final column names to match your existing workflow
    select(
      date,
      id,
      tenor,
      direction,
      rate,
      any_of("confidence"),  # Use any_of() for optional columns
      conference_date,
      model_name
    ) %>%
    # Remove confidence column if it's all NA
    {if("confidence" %in% names(.) && all(is.na(.$confidence))) select(., -confidence) else .} %>%
    # Remove any rows with completely missing essential data
    filter(!is.na(tenor), !is.na(rate), !is.na(direction))
  
  cat(crayon::green(glue("✅ Parsed {model_name} {conference_date}: {nrow(result)} rows\n")))
  
  return(result)
}

parse_all_responses <- function(transcript_data, models = c("chatgpt", "claude")) {
  cat(crayon::blue("🔍 Parsing all raw responses...\n"))
  
  # Ensure TARGET_TENORS is defined for filtering
  if (!exists("TARGET_TENORS")) {
    TARGET_TENORS <<- c("3M", "2Y", "10Y")
    cat(crayon::yellow("⚠️ TARGET_TENORS not defined, using default: 3M, 2Y, 10Y\n"))
  }
  
  # Get all combinations that should exist
  parse_grid <- expand_grid(
    model_name = models,
    conference_date = as.character(transcript_data$date)
  )
  
  cat(crayon::blue(glue("📋 Attempting to parse {nrow(parse_grid)} model-date combinations\n")))
  
  # Parse each response with detailed progress
  parsed_results <- parse_grid %>%
    pmap(function(model_name, conference_date) {
      cat(crayon::magenta(glue("\n--- Parsing {model_name} {conference_date} ---\n")))
      result <- parse_single_response(model_name, conference_date)
      cat(crayon::magenta("--- End parsing ---\n\n"))
      return(result)
    }) %>%
    discard(is.null)
  
  if (length(parsed_results) == 0) {
    cat(crayon::red("❌ No responses could be parsed\n"))
    return(tibble())
  }
  
  # Combine all results
  combined_results <- bind_rows(parsed_results)
  
  # Data quality summary
  cat(crayon::blue("\n📊 Parsing Summary:\n"))
  cat(crayon::green(glue("✅ Successfully parsed: {length(parsed_results)}/{nrow(parse_grid)} responses\n")))
  cat(crayon::green(glue("📈 Total observations: {nrow(combined_results)} rows\n")))
  
  # Summary by model
  model_summary <- combined_results %>%
    count(model_name, name = "observations") %>%
    arrange(desc(observations))
  
  cat(crayon::blue("📊 Observations by model:\n"))
  walk2(model_summary$model_name, model_summary$observations, 
        ~cat(crayon::green(glue("  {.x}: {.y} observations\n"))))
  
  # Summary by tenor
  if ("tenor" %in% names(combined_results)) {
    tenor_summary <- combined_results %>%
      count(tenor, name = "observations") %>%
      arrange(desc(observations))
    
    cat(crayon::blue("📊 Observations by tenor:\n"))
    walk2(tenor_summary$tenor, tenor_summary$observations,
          ~cat(crayon::green(glue("  {.x}: {.y} observations\n"))))
  }
  
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

step2_call_llms <- function(transcript_data, models = c("chatgpt", "claude"), limit_conferences = 3) {
  cat(crayon::magenta("=== STEP 2: Calling LLMs ===\n"))
  
  call_results <- call_all_llm_responses(
    transcript_data = transcript_data,
    models = models,
    limit_conferences = limit_conferences
  )
  
  return(call_results)
}

step3_parse_responses <- function(transcript_data, models = c("chatgpt", "claude")) {
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
  cat(crayon::blue("🚀 Cross-LLM Analysis - Now with Claude!\n"))
  cat(crayon::blue("Run each step separately:\n\n"))
  
  cat("# Step 1: Load data\n")
data <- step1_load_data()
  
  cat("# Step 2: Call LLMs (you can limit for testing)\n") 
call_results2 <- step2_call_llms(data$transcripts, models = c('claude'), limit_conferences = NULL)
  
  cat("# Step 3: Parse responses\n")
parsed <- step3_parse_responses(data$transcripts, models = c('chatgpt', 'claude'))
  
  cat("# Step 4: Analyze\n")
analysis <- step4_analyze_results(parsed, data$gemini, data$market)
  
  cat("# Check results\n")
print(analysis$correlations$detailed)
  
}
# ============================================================================
# CROSS-LLM MODEL STABILITY ICC ANALYSIS
# ============================================================================
# Measures reliability/consistency between different LLM models
# Higher ICC indicates models agree more on disagreement patterns

if (!exists("analysis") || is.null(analysis$disagreement)) {
  stop("No disagreement results available. Please run step4_analyze_results first.")
}
# --- Prepare data for ICC analysis with per-model standardization ---

model_stability_data <- analysis$disagreement %>%
  # Ensure we have at least 2 models for comparison
  group_by(conference_date, tenor) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  # Keep raw predicted_sd
  select(conference_date, tenor, model_name, predicted_sd) %>%
  filter(!is.na(predicted_sd), predicted_sd > 0) %>%
  # Standardize within each model (mean 0, sd 1)
  group_by(model_name) %>%
  mutate(std_rate = as.numeric(scale(predicted_sd))) %>%
  ungroup()

cat("=== MODEL STABILITY DATA SUMMARY ===\n")
cat("Total observations:", nrow(model_stability_data), "\n")
cat("Models available:", paste(unique(model_stability_data$model_name), collapse = ", "), "\n")
cat("Date range:", min(model_stability_data$conference_date), "to", max(model_stability_data$conference_date), "\n")

# --- Data coverage and ICC analysis ---

# Check data coverage
coverage_summary <- model_stability_data %>%
  count(model_name, tenor, name = "n_conferences") %>%
  arrange(model_name, tenor)

cat("\nData coverage by model and tenor:\n")
print(coverage_summary)

# Simple ICC calculation function (adapted for model comparison)
calculate_model_icc <- function(data) {
  # Between-conference variance (signal)
  conference_means <- data %>%
    group_by(conference_date) %>%
    summarise(conf_mean = mean(std_rate, na.rm = TRUE), .groups = "drop")
  
  # Within-conference variance across models (noise)
  within_variances <- data %>%
    group_by(conference_date) %>%
    summarise(within_var = var(std_rate, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(within_var), within_var > 0)
  
  if (nrow(within_variances) == 0) return(NA)
  
  var_between <- var(conference_means$conf_mean, na.rm = TRUE)
  var_within  <- mean(within_variances$within_var, na.rm = TRUE)
  
  icc <- var_between / (var_between + var_within)
  return(round(icc, 3))
}

# Calculate ICC by tenor
icc_by_tenor <- model_stability_data %>%
  group_by(tenor) %>%
  group_split() %>%
  map_dfr(function(data) {
    if (nrow(data) < 15) return(NULL)  # Skip if too few observations
    
    n_models <- length(unique(data$model_name))
    if (n_models < 2) return(NULL)
    
    tibble(
      tenor          = data$tenor[1],
      icc            = calculate_model_icc(data),
      n_obs          = nrow(data),
      n_models       = n_models,
      n_conferences  = length(unique(data$conference_date))
    )
  }) %>%
  filter(!is.na(icc))

# Calculate overall ICC (across all tenors)
overall_icc <- calculate_model_icc(model_stability_data)

# Display results
cat("\n=== MODEL STABILITY ICC RESULTS ===\n")
cat("Overall ICC (all tenors):", overall_icc, "\n\n")

print(icc_by_tenor %>% arrange(desc(icc)))


# Calculate pairwise correlations between models for additional insight
pairwise_correlations <- model_stability_data %>%
  select(conference_date, tenor, model_name, std_rate) %>%
  pivot_wider(names_from = model_name, values_from = std_rate) %>%
  group_by(tenor) %>%
  do({
    models <- names(.)[!names(.) %in% c("conference_date", "tenor")]
    if(length(models) < 2) return(tibble())
    
    # Calculate all pairwise correlations
    corr_results <- tibble()
    for(i in 1:(length(models)-1)) {
      for(j in (i+1):length(models)) {
        model1 <- models[i]
        model2 <- models[j]
        
        valid_pairs <- !is.na(.[[model1]]) & !is.na(.[[model2]])
        if(sum(valid_pairs) >= 10) {
          corr <- cor(.[[model1]][valid_pairs], .[[model2]][valid_pairs], 
                     method = "spearman", use = "complete.obs")
          
          corr_results <- bind_rows(corr_results, tibble(
            model1 = model1,
            model2 = model2,
            correlation = round(corr, 3),
            n_pairs = sum(valid_pairs)
          ))
        }
      }
    }
    corr_results
  }) %>%
  ungroup()

cat("\n=== PAIRWISE MODEL CORRELATIONS ===\n")
print(pairwise_correlations %>% arrange(desc(correlation)))

# Color palette for visualizations
color_palette <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")
model_colors <- c("chatgpt" = "#10a37f", "claude" = "#d97706", "gemini" = "#4285f4")

# Create simple, clear visualizations that tell a story

# Define standard colors
color_palette <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")
model_colors <- c("chatgpt" = "#91bfdb", "claude" = "#4575b4", "gemini" = "#d73027")

# Plot 1: Model disagreement scatter with 45-degree line and dates as labels
biggest_differences <- model_stability_data %>%
  select(conference_date, tenor, model_name, std_rate) %>%
  pivot_wider(names_from = model_name, values_from = std_rate) %>%
  filter(complete.cases(.)) %>%
  rowwise() %>%
  mutate(
    max_val = max(c_across(where(is.numeric)), na.rm = TRUE),
    min_val = min(c_across(where(is.numeric)), na.rm = TRUE),
    difference = max_val - min_val,
    avg_val = mean(c_across(where(is.numeric)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    conference_date = as.Date(conference_date),
    date_label = format(conference_date, "%Y-%m")  # Format as YYYY-MM for readability
  )

# --- Plot 1: Model disagreement vs average uncertainty ---
p1 <- ggplot(biggest_differences, aes(x = avg_val, y = difference)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50", size = 0.8) +
  geom_text(aes(label = date_label, color = factor(tenor, levels = c("3M", "2Y", "10Y"))),
            size = 3, alpha = 0.7, check_overlap = TRUE) +
  scale_color_manual(values = c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027"), name = "Tenor") +
  facet_wrap(~tenor) +
  labs(
    title = "Model Disagreement vs Average Uncertainty",
    x = "Average Uncertainty",
    y = "Disagreement Between Models",
    caption = "Conference dates shown (YYYY-MM)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.caption = element_text(size = 10, hjust = 0, color = "grey50")
  )

# --- Plot 2: Individual model correlations with market ---
p2 <- ggplot(market_corr_data, aes(x = model_name, y = spearman_correlation, fill = model_name)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", spearman_correlation)), vjust = -0.3, size = 6) +
  facet_wrap(~tenor, nrow = 1) +
  scale_fill_manual(values = c("chatgpt" = "#91bfdb", "claude" = "#4575b4", "gemini" = "#d73027"), guide = "none") +
  labs(
    title = "Individual Model Correlations with Market Volatility",
    x = "Model",
    y = "Spearman Correlation"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    legend.position = "bottom"
  )

# =============================================================================
# ICC + Ensemble Side-by-Side Plot
# =============================================================================

# Prepare combined data
plot_data <- bind_rows(
  icc_by_tenor %>% select(tenor, value = icc) %>% mutate(metric_plot = "ICC"),
  ensemble_corr %>% select(tenor, value = spearman_corr) %>% mutate(metric_plot = "Ensemble")
) %>%
  mutate(
    tenor = factor(tenor, levels = c("3M", "2Y", "10Y")),
    metric_plot = factor(metric_plot, levels = c("ICC", "Ensemble"))
  )

# Define colors
fill_colors <- c(
  "ICC" = "#4575b4",        # blue
  "Ensemble" = "#d73027"    # red
)

# Create side-by-side bar plot
p_combined <- ggplot(plot_data, aes(x = tenor, y = value, fill = metric_plot)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.8) +
  geom_text(aes(label = sprintf("%.3f", value)),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 5) +
  scale_fill_manual(values = fill_colors) +
  labs(
    title = "",
    x = "Tenor",
    y = "ICC / Spearman Correlation",
    fill = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "bottom"
  )




# --- Print plots ---
print(p1)
print(p2)
print(p_combined)

# --- Save plots ---
ggsave("../output/cross_llm_results/model_stability_icc.pdf", p1, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("../output/cross_llm_results/model_individual_correlation.pdf", p2, width = 12, height = 6, dpi = 300, bg = "white")
ggsave("../output/cross_llm_results/model_agreement_ensemble_combined.pdf", p_combined, width = 10, height = 6, dpi = 300, bg = "white")
