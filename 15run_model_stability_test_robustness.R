# Cross-LLM Robustness Testing for ECB Conference Analysis
# =======================================================
# This script tests robustness across ChatGPT and DeepSeek (Gemini results already exist)
# and computes ICC analysis for model sensitivity assessment.

# Load necessary libraries and set parameters ----

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  openai,      # For ChatGPT API
  httr2,       # For DeepSeek API calls
  readtext,
  crayon,
  stringr,
  purrr,
  readr,
  writexl,
  readxl,
  tidyverse,
  future,
  furrr,
  jsonlite,
  psych,       # for ICC analysis
  irr,         # alternative ICC implementation
  broom,       # for tidy statistical output
  corrplot,    # for correlation visualization
  RColorBrewer,
  showtext,
  glue
)

# Set working directory
setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# Set API keys
Sys.setenv(OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY"))

# Configuration ----
TEMPERATURE <- 1.0
TARGET_TENORS <- c("3M", "2Y", "10Y")
MAX_WORKERS <- 4
N_CONFERENCES <- 100 # Adjust based on needs

# Directories
OUTPUT_DIR <- "../intermediate_data/cross_llm_analysis"
RESULTS_DIR <- "../output/cross_llm_results"
INTERMEDIATE_DIR <- file.path(OUTPUT_DIR, "intermediate")

walk(c(OUTPUT_DIR, RESULTS_DIR, INTERMEDIATE_DIR), ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# LLM API Functions ----

call_chatgpt <- function(prompt, user_message, temperature = 1.0) {
  tryCatch({
    create_chat_completion(
      model = "gpt-4o",
      messages = list(
        list(role = "system", content = prompt),
        list(role = "user", content = user_message)
      ),
      temperature = temperature,
      max_tokens = 4000
    ) %>%
      pluck("choices", 1, "message", "content")
  }, error = function(e) {
    stop(glue("ChatGPT API error: {e$message}"))
  })
}

call_deepseek <- function(prompt, user_message, temperature = 1.0) {
  api_key <- Sys.getenv("DEEPSEEK_API_KEY")
  if (api_key == "") stop("DEEPSEEK_API_KEY environment variable not set")
  
  request_body <- list(
    model = "deepseek-chat",
    messages = list(
      list(role = "system", content = prompt),
      list(role = "user", content = user_message)
    ),
    temperature = temperature,
    max_tokens = 4000
  )
  
  tryCatch({
    request("https://api.deepseek.com/chat/completions") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(request_body) |>
      req_timeout(120) |>
      req_perform() |>
      resp_body_json() |>
      pluck("choices", 1, "message", "content")
  }, error = function(e) {
    stop(glue("DeepSeek API error: {e$message}"))
  })
}

# LLM dispatcher
call_llm <- function(model_name, prompt, user_message, temperature = 1.0) {
  switch(model_name,
    "chatgpt" = call_chatgpt(prompt, user_message, temperature),
    "deepseek" = call_deepseek(prompt, user_message, temperature),
    stop(glue("Unknown model: {model_name}"))
  )
}

# Data Processing Functions ----

parse_markdown_table <- function(markdown_string) {
  if (is.null(markdown_string) || is.na(markdown_string) || markdown_string == "") {
    return(NULL)
  }
  
  lines <- str_split_1(markdown_string, "\n") %>%
    str_trim() %>%
    keep(~nzchar(.x))
  
  if (length(lines) < 3) return(NULL)
  
  # Find header and separator
  separator_indices <- lines %>%
    str_detect("^\\|[\\s\\-\\|]+\\|$") %>%
    which()
  
  if (length(separator_indices) == 0) return(NULL)
  
  separator_idx <- separator_indices[1]
  header_idx <- separator_idx - 1
  
  if (header_idx < 1) return(NULL)
  
  # Extract headers
  headers <- lines[header_idx] %>%
    str_remove_all("^\\||\\|$") %>%
    str_split_1("\\|") %>%
    str_trim() %>%
    keep(~nzchar(.x))
  
  # Extract data rows
  data_lines <- lines[(separator_idx + 1):length(lines)] %>%
    keep(~str_detect(.x, "^\\|.*\\|$"))
  
  if (length(data_lines) == 0) return(NULL)
  
  tryCatch({
    parsed_data <- data_lines %>%
      map(~{
        cells <- .x %>%
          str_remove_all("^\\||\\|$") %>%
          str_split_1("\\|") %>%
          str_trim() %>%
          keep(~nzchar(.x))
        
        if (length(cells) == length(headers)) cells else NULL
      }) %>%
      discard(is.null)
    
    if (length(parsed_data) == 0) return(NULL)
    
    # Create tibble
    df <- parsed_data %>%
      map_dfr(~set_names(.x, headers))
    
    # Process rate column
    rate_col <- names(df) %>%
      str_subset("(?i)rate.*%|expected.*rate") %>%
      first()
    
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
    cat(crayon::red(glue("Error parsing markdown table: {e$message}\n")))
    return(NULL)
  })
}

load_transcripts <- function() {
  transcript_files <- list.files("../intermediate_data/texts/", pattern = "\\.txt$", full.names = TRUE)
  
  transcripts <- transcript_files %>%
    set_names(basename) %>%
    map_dfr(~{
      filename <- basename(.x)
      date_match <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
      
      if (is.na(date_match)) return(tibble())
      
      tryCatch({
        text <- read_lines(.x) %>% paste(collapse = " ")
        tibble(
          date = as.Date(date_match),
          filename = filename,
          text = text
        )
      }, error = function(e) {
        cat(crayon::yellow(glue("Warning: Could not read {filename}: {e$message}\n")))
        tibble()
      })
    }, .id = "file_path") %>%
    filter(nzchar(as.character(date))) %>%
    arrange(date) %>%
    slice_tail(n = N_CONFERENCES)
  
  cat(glue("📊 Loaded {nrow(transcripts)} transcripts (most recent {N_CONFERENCES})\n"))
  return(transcripts)
}

# Load existing Gemini results
load_gemini_results <- function() {
  tryCatch({
    # Specify the exact file you want to use
    gemini_file <- "../intermediate_data/aggregate_gemini_result/prompt_naive/2.5flash_2025-07-21.xlsx"
    
    if (!file.exists(gemini_file)) {
      cat(crayon::yellow("⚠️ Gemini results file not found at specified path\n"))
      cat(crayon::yellow(glue("   Looking for: {gemini_file}\n")))
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
    
    cat(glue("✅ Loaded Gemini results: {nrow(gemini_data)} rows from {basename(gemini_file)}\n"))
    return(gemini_data)
    
  }, error = function(e) {
    cat(crayon::red(glue("❌ Error loading Gemini results: {e$message}\n")))
    return(tibble())
  })
}

# Single conference simulation for one model
simulate_single_conference <- function(model_name, conference_info, analyst_prompt) {
  conference_date <- as.character(conference_info$date)
  conference_text <- conference_info$text
  
  # Check if intermediate result already exists
  intermediate_file <- file.path(INTERMEDIATE_DIR, glue("{model_name}_{conference_date}.rds"))
  
  if (file.exists(intermediate_file)) {
    cat(crayon::blue(glue("📂 Loading cached result for {model_name} on {conference_date}\n")))
    return(readRDS(intermediate_file))
  }
  
  # Format prompt
  formatted_prompt <- str_replace_all(analyst_prompt, "\\[date\\]", conference_date)
  
  user_message <- glue(
    "Here is the ECB press conference for {conference_date}:\n\n",
    "{conference_text}\n\n",
    "Please simulate the trading actions for the 30 traders as per the instructions. ",
    "Ensure the output is strictly a markdown table with the specified columns."
  )
  
  # Retry logic with exponential backoff
  max_attempts <- 3
  
  for (attempt in 1:max_attempts) {
    if (attempt > 1) {
      wait_time <- 2^(attempt-1)
      cat(crayon::yellow(glue("⏳ Waiting {wait_time} seconds before retry...\n")))
      Sys.sleep(wait_time)
    }
    
    tryCatch({
      cat(crayon::blue(glue("🔄 {model_name} processing {conference_date} (attempt {attempt})\n")))
      
      response <- call_llm(model_name, formatted_prompt, user_message, TEMPERATURE)
      parsed_df <- parse_markdown_table(response)
      
      if (!is.null(parsed_df) && nrow(parsed_df) > 0) {
        result <- parsed_df %>%
          mutate(
            conference_date = conference_date,
            model_name = model_name
          )
        
        # Save intermediate result
        saveRDS(result, intermediate_file)
        
        expected_rows <- 30 * length(TARGET_TENORS)
        actual_rows <- nrow(result)
        
        if (actual_rows >= expected_rows * 0.7) {
          cat(crayon::green(glue("✅ {model_name} {conference_date}: {actual_rows} rows\n")))
        } else {
          cat(crayon::yellow(glue("⚠️ {model_name} {conference_date}: {actual_rows}/{expected_rows} rows\n")))
        }
        
        return(result)
      } else {
        cat(crayon::yellow(glue("⚠️ Attempt {attempt} failed to parse response\n")))
      }
      
    }, error = function(e) {
      cat(crayon::red(glue("❌ Attempt {attempt} failed: {e$message}\n")))
    })
  }
  
  cat(crayon::red(glue("❌ All {max_attempts} attempts failed for {model_name} on {conference_date}\n")))
  return(NULL)
}

# Main Analysis Function ----

run_cross_llm_analysis <- function() {
  cat(crayon::blue("🚀 Starting Cross-LLM Robustness Analysis\n"))
  cat(crayon::blue(str_dup("=", 60)), "\n")
  
  # Load data
  transcript_data <- load_transcripts()
  gemini_data <- load_gemini_results()
  
  # Load the best-performing naive prompt from create_prompts.R
  source("create_prompts.R")
  analyst_prompt <- prompt_naive

  start_time <- Sys.time()
  
  # Set up parallel processing
  plan(multisession, workers = MAX_WORKERS)
  
  # Models to run (excluding Gemini since we have those results)
  new_models <- c("chatgpt", "deepseek")
  
  # Run analysis for new models in parallel
  cat(crayon::magenta("\n🤖 Running analysis for new models...\n"))
  
  new_results <- new_models %>%
    set_names() %>%
    map(~{
      cat(crayon::magenta(glue("📍 Processing {toupper(.x)} model\n")))
      
      # Process all conferences for this model in parallel
      model_results <- transcript_data %>%
        future_pmap(function(date, filename, text, file_path) {
          conference_info <- list(date = date, text = text, filename = filename)
          simulate_single_conference(.x, conference_info, analyst_prompt)
        }, .options = furrr_options(seed = TRUE)) %>%
        discard(is.null)
      
      if (length(model_results) > 0) {
        combined_results <- bind_rows(model_results)
        cat(crayon::green(glue("✅ {toupper(.x)}: {nrow(combined_results)} total rows\n")))
        
        # Save model results
        model_file <- file.path(OUTPUT_DIR, glue("{.x}_complete_results.csv"))
        write_csv(combined_results, model_file)
        cat(crayon::blue(glue("💾 Saved {.x} results to {model_file}\n")))
        
        combined_results
      } else {
        cat(crayon::red(glue("❌ {toupper(.x)} failed completely\n")))
        NULL
      }
    }) %>%
    discard(is.null)
  
  # Reset parallel plan
  plan(sequential)
  
  # Combine all results (Gemini + new models)
  all_results <- list()
  
  # Add Gemini results if available
  if (nrow(gemini_data) > 0) {
    all_results[["gemini"]] <- gemini_data
    cat(crayon::green(glue("✅ GEMINI: {nrow(gemini_data)} rows (pre-existing)\n")))
  }
  
  # Add new model results
  all_results <- c(all_results, new_results)
  
  if (length(all_results) == 0) {
    stop("❌ No valid results from any model")
  }
  
  # Combine final results
  final_results <- bind_rows(all_results)
  
  # Save combined results with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  final_file <- file.path(OUTPUT_DIR, glue("cross_llm_complete_{timestamp}.csv"))
  write_csv(final_results, final_file)
  
  elapsed_time <- difftime(Sys.time(), start_time, units = "mins")
  
  cat(crayon::green("\n✅ Cross-LLM Analysis Completed\n"))
  cat(glue("⏱️ Time elapsed: {round(elapsed_time, 1)} minutes\n"))
  cat(glue("📊 Total rows: {nrow(final_results)}\n"))
  cat(glue("📊 Models: {paste(unique(final_results$model_name), collapse = ', ')}\n"))
  cat(glue("📊 Conferences: {n_distinct(final_results$conference_date)}\n"))
  cat(glue("💾 Final results: {final_file}\n"))
  
  return(final_results)
}

# ICC Analysis Functions ----

compute_disagreement_measures <- function(df) {
  cat("📊 Computing disagreement measures...\n")
  
  disagreement <- df %>%
    filter(!is.na(rate)) %>%
    group_by(model_name, conference_date, tenor) %>%
    summarise(
      predicted_sd = sd(rate, na.rm = TRUE),
      predicted_mean = mean(rate, na.rm = TRUE),
      n_traders = n(),
      .groups = "drop"
    ) %>%
    filter(n_traders >= 10, !is.na(predicted_sd)) %>%
    arrange(conference_date, tenor, model_name)
  
  cat(glue("✅ Computed disagreement for {nrow(disagreement)} model-conference-tenor combinations\n"))
  return(disagreement)
}

load_market_data <- function() {
  tryCatch({
    cat("📊 Loading market volatility data...\n")
    
    market_data <- readRDS("../intermediate_data/range_difference_df.rds") %>%
      mutate(
        conference_date = as.character(as.Date(date)),
        tenor = case_when(
          tenor == "3mnt" ~ "3M",
          tenor == "6mnt" ~ "6M",
          tenor %in% c("1Y", "2Y", "5Y", "10Y") ~ tenor,
          TRUE ~ tenor
        )
      ) %>%
      filter(tenor %in% TARGET_TENORS) %>%
      select(conference_date, tenor, market_volatility = correct_post_mean) %>%
      filter(!is.na(market_volatility))
    
    cat(glue("✅ Market data loaded: {nrow(market_data)} observations\n"))
    cat(glue("📊 Date range: {min(market_data$conference_date)} to {max(market_data$conference_date)}\n"))
    
    return(market_data)
    
  }, error = function(e) {
    cat(crayon::red(glue("❌ Error loading market data: {e$message}\n")))
    return(NULL)
  })
}

compute_model_correlations <- function(disagreement_df, market_data_df) {
  cat("📊 Computing model correlations...\n")
  
  merged_df <- disagreement_df %>%
    inner_join(market_data_df, by = c("conference_date", "tenor"))
  
  if (nrow(merged_df) == 0) {
    cat(crayon::red("❌ No matching data between LLM predictions and market measures\n"))
    return(NULL)
  }
  
  cat(glue("📊 Merged dataset: {nrow(merged_df)} observations\n"))
  cat(glue("📊 Models: {paste(sort(unique(merged_df$model_name)), collapse = ', ')}\n"))
  
  # Compute correlations by model and tenor
  correlation_results <- merged_df %>%
    group_by(model_name, tenor) %>%
    filter(n() >= 5) %>%
    group_split() %>%
    map_dfr(~{
      if (nrow(.x) < 5) return(tibble())
      
      spear_test <- cor.test(.x$predicted_sd, .x$market_volatility, method = "spearman")
      pears_test <- cor.test(.x$predicted_sd, .x$market_volatility, method = "pearson")
      
      tibble(
        model_name = .x$model_name[1],
        tenor = .x$tenor[1],
        n_observations = nrow(.x),
        spearman_correlation = spear_test$estimate,
        spearman_p_value = spear_test$p.value,
        pearson_correlation = pears_test$estimate,
        pearson_p_value = pears_test$p.value
      )
    })
  
  # Model averages across tenors
  model_averages <- correlation_results %>%
    group_by(model_name) %>%
    summarise(
      avg_spearman_correlation = mean(spearman_correlation, na.rm = TRUE),
      avg_pearson_correlation = mean(pearson_correlation, na.rm = TRUE),
      total_observations = sum(n_observations),
      n_tenors = n(),
      .groups = "drop"
    )
  
  cat("✅ Computed correlations for all model-tenor combinations\n")
  
  return(list(
    detailed = correlation_results, 
    averages = model_averages, 
    merged_data = merged_df
  ))
}

compute_icc_analysis <- function(disagreement_df) {
  cat("\n📊 Computing ICC Analysis...\n")
  
  # Prepare data for ICC - pivot to wide format
  icc_data <- disagreement_df %>%
    select(model_name, conference_date, tenor, predicted_sd) %>%
    pivot_wider(
      names_from = model_name, 
      values_from = predicted_sd, 
      values_fill = NA
    )
  
  model_cols <- setdiff(names(icc_data), c("conference_date", "tenor"))
  
  if (length(model_cols) < 2) {
    cat(crayon::yellow("⚠️ Need at least 2 models for ICC analysis\n"))
    return(list())
  }
  
  # Compute ICC for each tenor
  icc_results <- TARGET_TENORS %>%
    set_names() %>%
    map(~{
      tenor_data <- icc_data %>%
        filter(tenor == .x) %>%
        select(all_of(model_cols))
      
      complete_data <- tenor_data[complete.cases(tenor_data), ]
      
      if (nrow(complete_data) >= 3 && ncol(complete_data) >= 2) {
        tryCatch({
          icc_result <- psych::ICC(complete_data, missing = FALSE)
          
          icc_values <- list(
            tenor = .x,
            n_observations = nrow(complete_data),
            n_models = ncol(complete_data),
            models_included = model_cols,
            icc_1_1 = icc_result$results["ICC1", "ICC"],
            icc_2_1 = icc_result$results["ICC2", "ICC"],
            icc_3_1 = icc_result$results["ICC3", "ICC"],
            icc_2_k = icc_result$results["ICC2k", "ICC"],
            icc_3_k = icc_result$results["ICC3k", "ICC"],
            icc_2_1_lower = icc_result$results["ICC2", "lower bound"],
            icc_2_1_upper = icc_result$results["ICC2", "upper bound"],
            p_value = icc_result$results["ICC2", "p"]
          )
          
          cat(glue("  ✅ {.x}: ICC(2,1) = {round(icc_values$icc_2_1, 3)} ",
                  "[{round(icc_values$icc_2_1_lower, 3)}, ",
                  "{round(icc_values$icc_2_1_upper, 3)}], ",
                  "p = {round(icc_values$p_value, 3)} (n={icc_values$n_observations})\n"))
          
          icc_values
        }, error = function(e) {
          cat(glue("  ❌ {.x}: Error computing ICC - {e$message}\n"))
          NULL
        })
      } else {
        cat(glue("  ❌ {.x}: Insufficient data (n={nrow(complete_data)}, models={ncol(complete_data)})\n"))
        NULL
      }
    }) %>%
    discard(is.null)
  
  # Overall ICC across all tenors
  if (length(icc_results) > 0) {
    tryCatch({
      overall_data <- icc_data %>%
        select(all_of(model_cols))
      
      complete_overall <- overall_data[complete.cases(overall_data), ]
      
      if (nrow(complete_overall) >= 3) {
        overall_icc <- psych::ICC(complete_overall, missing = FALSE)
        
        icc_results[["overall"]] <- list(
          tenor = "Overall",
          n_observations = nrow(complete_overall),
          n_models = ncol(complete_overall),
          models_included = model_cols,
          icc_2_1 = overall_icc$results["ICC2", "ICC"],
          icc_2_k = overall_icc$results["ICC2k", "ICC"],
          icc_2_1_lower = overall_icc$results["ICC2", "lower bound"],
          icc_2_1_upper = overall_icc$results["ICC2", "upper bound"],
          p_value = overall_icc$results["ICC2", "p"]
        )
        
        cat(glue("\n  🎯 Overall ICC: ICC(2,1) = {round(icc_results$overall$icc_2_1, 3)} ",
                "p = {round(icc_results$overall$p_value, 3)}\n"))
      }
    }, error = function(e) {
      cat(glue("  ⚠️ Could not compute overall ICC: {e$message}\n"))
    })
  }
  
  return(icc_results)
}

# Visualization Functions ----

create_robustness_plots <- function(correlation_results, icc_results) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # 1. Correlation comparison plot
  p1 <- correlation_results$detailed %>%
    mutate(
      significance = case_when(
        spearman_p_value < 0.001 ~ "***",
        spearman_p_value < 0.01 ~ "**", 
        spearman_p_value < 0.05 ~ "*",
        TRUE ~ ""
      ),
      tenor = factor(tenor, levels = TARGET_TENORS)
    ) %>%
    ggplot(aes(x = tenor, y = spearman_correlation, fill = model_name)) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.8) +
    geom_text(aes(label = paste0(round(spearman_correlation, 3), significance)), 
              position = position_dodge(0.7), vjust = -0.5, size = 3.5) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Cross-LLM Performance: Correlations with Market Volatility",
      subtitle = "Spearman correlations between LLM disagreement and market-based measures",
      x = "OIS Tenor", 
      y = "Spearman Correlation",
      fill = "Model",
      caption = "*** p<0.001, ** p<0.01, * p<0.05"
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "grey50"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  # 2. ICC comparison plot
  if (length(icc_results) > 0) {
    icc_df <- icc_results %>%
      map_dfr(~{
        tibble(
          tenor = .x$tenor,
          icc_2_1 = .x$icc_2_1,
          icc_lower = .x$icc_2_1_lower,
          icc_upper = .x$icc_2_1_upper,
          p_value = .x$p_value,
          n_obs = .x$n_observations
        )
      }) %>%
      filter(tenor != "Overall") %>%
      mutate(
        tenor = factor(tenor, levels = TARGET_TENORS),
        significance = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01 ~ "**",
          p_value < 0.05 ~ "*", 
          TRUE ~ ""
        )
      )
    
    p2 <- icc_df %>%
      ggplot(aes(x = tenor, y = icc_2_1)) +
      geom_col(fill = "steelblue", alpha = 0.7, width = 0.5) +
      geom_errorbar(aes(ymin = icc_lower, ymax = icc_upper), 
                    width = 0.2, color = "darkred", linewidth = 0.8) +
      geom_text(aes(label = paste0(round(icc_2_1, 3), significance)), 
                vjust = -0.5, size = 4) +
      labs(
        title = "Cross-LLM Consistency: Intraclass Correlation Coefficients",
        subtitle = "ICC(2,1) measures consistency across models (higher = more consistent)",
        x = "OIS Tenor",
        y = "ICC(2,1)",
        caption = "Error bars: 95% confidence intervals; *** p<0.001, ** p<0.01, * p<0.05"
      ) +
      theme_minimal(base_family = "Segoe UI") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "grey50"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  } else {
    p2 <- ggplot() + 
      geom_text(aes(x = 0.5, y = 0.5, label = "Insufficient data for ICC analysis"), 
                size = 6, color = "grey50") +
      labs(title = "ICC Analysis: Insufficient Data") +
      theme_void()
  }
  
  # 3. Model comparison heatmap
  p3 <- correlation_results$detailed %>%
    ggplot(aes(x = tenor, y = model_name, fill = spearman_correlation)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = round(spearman_correlation, 3)), 
              color = "white", fontface = "bold", size = 4) +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue", 
      midpoint = 0, name = "Correlation"
    ) +
    labs(
      title = "Cross-LLM Performance Heatmap",
      subtitle = "Spearman correlations by model and tenor",
      x = "OIS Tenor",
      y = "Model"
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "grey50"),
      axis.text.x = element_text(angle = 0),
      panel.grid = element_blank()
    )
  
  # Save plots
  plot_files <- c(
    glue("correlation_comparison_{timestamp}.png"),
    glue("icc_analysis_{timestamp}.png"), 
    glue("performance_heatmap_{timestamp}.png")
  )
  
  plots <- list(p1, p2, p3)
  
  walk2(plots, plot_files, ~{
    ggsave(
      file.path(RESULTS_DIR, .y), .x, 
      width = 10, height = 6, dpi = 320, bg = "white"
    )
  })
  
  cat(glue("📊 Plots saved: {paste(plot_files, collapse = ', ')}\n"))
  
  return(list(correlation_plot = p1, icc_plot = p2, heatmap_plot = p3))
}

create_robustness_report <- function(correlation_results, icc_results, filename = NULL) {
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- file.path(RESULTS_DIR, glue("robustness_report_{timestamp}.md"))
  }
  
  # Build report content
  report_lines <- c(
    "# Cross-LLM Robustness Analysis Report",
    glue("**Generated:** {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"),
    "",
    "## Executive Summary",
    "",
    glue("This report presents robustness testing across **{n_distinct(correlation_results$detailed$model_name)} LLM models**:"),
    paste("*", sort(unique(correlation_results$detailed$model_name)), collapse = ", "),
    "",
    glue("Analysis covers **{n_distinct(correlation_results$detailed$tenor)} interest rate tenors**: {paste(TARGET_TENORS, collapse = ', ')}"),
    "",
    "## Model Performance Comparison",
    ""
  )
  
  # Add model averages table
  model_performance <- correlation_results$averages %>%
    arrange(desc(avg_spearman_correlation)) %>%
    mutate(
      rank = row_number(),
      avg_spearman = round(avg_spearman_correlation, 3),
      avg_pearson = round(avg_pearson_correlation, 3)
    )
  
  report_lines <- c(
    report_lines,
    "### Overall Model Rankings (by Average Spearman Correlation):",
    "",
    "| Rank | Model | Avg Spearman | Avg Pearson | Total Obs | Tenors |",
    "|------|-------|--------------|-------------|-----------|--------|"
  )
  
  model_table_lines <- model_performance %>%
    pmap_chr(~glue("| {..6} | {..1} | {..7} | {..8} | {..4} | {..5} |"))
  
  report_lines <- c(report_lines, model_table_lines, "")
  
  # Add detailed results by tenor
  report_lines <- c(
    report_lines,
    "### Detailed Results by Tenor:",
    ""
  )
  
  for (t in TARGET_TENORS) {
    tenor_results <- correlation_results$detailed %>%
      filter(tenor == t) %>%
      arrange(desc(spearman_correlation))
    
    report_lines <- c(
      report_lines,
      glue("#### {t} Tenor:"),
      ""
    )
    
    tenor_lines <- tenor_results %>%
      pmap_chr(~glue("- **{..1}**: ρ = {round(..4, 3)} (p = {round(..5, 3)}, n = {..3})"))
    
    report_lines <- c(report_lines, tenor_lines, "")
  }
  
  # Add ICC results
  if (length(icc_results) > 0) {
    report_lines <- c(
      report_lines,
      "## Intraclass Correlation Analysis",
      "",
      "The ICC measures **consistency across models**. Values closer to 1 indicate higher consistency.",
      ""
    )
    
    icc_lines <- icc_results %>%
      imap_chr(~{
        if (.y == "overall") {
          glue("**Overall**: ICC(2,1) = {round(.x$icc_2_1, 3)} [{round(.x$icc_2_1_lower, 3)}, {round(.x$icc_2_1_upper, 3)}], p = {round(.x$p_value, 3)} (n = {.x$n_observations})")
        } else {
          glue("**{.y}**: ICC(2,1) = {round(.x$icc_2_1, 3)} [{round(.x$icc_2_1_lower, 3)}, {round(.x$icc_2_1_upper, 3)}], p = {round(.x$p_value, 3)} (n = {.x$n_observations})")
        }
      })
    
    report_lines <- c(report_lines, icc_lines, "")
  }
  
  # Add interpretation section
  report_lines <- c(
    report_lines,
    "## Key Findings & Interpretation",
    "",
    "### Model Performance:",
    glue("- **Best performing model**: {model_performance$model_name[1]} (ρ = {model_performance$avg_spearman[1]})"),
    glue("- **Correlation range**: {min(correlation_results$detailed$spearman_correlation, na.rm=TRUE) |> round(3)} to {max(correlation_results$detailed$spearman_correlation, na.rm=TRUE) |> round(3)}"),
    ""
  )
  
  if (length(icc_results) > 0) {
    overall_icc <- icc_results$overall$icc_2_1 %||% NA
    consistency_level <- case_when(
      is.na(overall_icc) ~ "Unknown",
      overall_icc >= 0.75 ~ "Excellent",
      overall_icc >= 0.60 ~ "Good", 
      overall_icc >= 0.40 ~ "Fair",
      TRUE ~ "Poor"
    )
    
    report_lines <- c(
      report_lines,
      "### Model Consistency:",
      if (!is.na(overall_icc)) glue("- **Overall ICC**: {round(overall_icc, 3)} ({consistency_level} consistency)") else "- **Overall ICC**: Not available",
      ""
    )
  }
  
  report_lines <- c(
    report_lines,
    "---",
    "",
    "*This analysis demonstrates the robustness of LLM-based market simulation across different model architectures.*"
  )
  
  # Write report
  writeLines(report_lines, filename)
  cat(glue("📄 Report saved to: {filename}\n"))
  
  return(filename)
}

# Main execution function ----

main_robustness_analysis <- function() {
  cat(crayon::blue("🎯 Starting Complete Cross-LLM Robustness Analysis\n\n"))
  
  # Step 1: Run cross-LLM analysis
  results_df <- run_cross_llm_analysis()
  
  # Step 2: Compute disagreement measures
  disagreement_df <- compute_disagreement_measures(results_df)
  
  # Step 3: Load market data
  market_data_df <- load_market_data()
  
  if (is.null(market_data_df) || nrow(market_data_df) == 0) {
    stop("❌ Could not load market data - aborting analysis")
  }
  
  # Step 4: Compute correlations
  correlation_results <- compute_model_correlations(disagreement_df, market_data_df)
  
  if (is.null(correlation_results)) {
    stop("❌ Could not compute correlations - aborting analysis")
  }
  
  # Step 5: Compute ICC analysis
  icc_results <- compute_icc_analysis(disagreement_df)
  
  # Step 6: Create visualizations
  plots <- create_robustness_plots(correlation_results, icc_results)
  
  # Step 7: Create comprehensive report
  report_file <- create_robustness_report(correlation_results, icc_results)
  
  # Step 8: Save detailed results
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  results_list <- list(
    correlations_detailed = correlation_results$detailed,
    correlations_summary = correlation_results$averages,
    disagreement_measures = disagreement_df,
    raw_results = results_df
  )
  
  # Add ICC results if available
  if (length(icc_results) > 0) {
    results_list$icc_results <- map_dfr(icc_results, ~as_tibble(.x), .id = "tenor")
  }
  
  results_file <- file.path(RESULTS_DIR, glue("robustness_analysis_{timestamp}.xlsx"))
  write_xlsx(results_list, results_file)
  
  cat(crayon::green("\n🎉 Complete Cross-LLM Robustness Analysis Finished!\n"))
  cat(glue("📊 Summary: {nrow(results_df)} total observations across {n_distinct(results_df$model_name)} models\n"))
  cat(glue("📄 Report: {report_file}\n"))
  cat(glue("📊 Data: {results_file}\n"))
  cat(glue("🎨 Plots: {RESULTS_DIR}/\n"))
  
  return(list(
    results = results_df,
    correlations = correlation_results,
    icc = icc_results,
    plots = plots,
    report_file = report_file
  ))
}

# Execute the analysis ----
if (interactive()) {
  cat("🚀 Ready to run cross-LLM robustness analysis!\n")
  cat("Execute: robustness_output <- main_robustness_analysis()\n")
} else {
  # Run automatically if sourced non-interactively
  main_robustness_analysis()
}