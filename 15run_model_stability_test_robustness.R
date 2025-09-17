# Cross-LLM Robustness Testing for ECB Conference Analysis
# =======================================================
# Complete script testing Gemini 2.5 Flash vs GPT-4o vs DeepSeek-V3

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
MAX_WORKERS <- 4

# Directories
OUTPUT_DIR <- "../intermediate_data/cross_llm_analysis"
RESULTS_DIR <- "../output/cross_llm_results"
INTERMEDIATE_DIR <- file.path(OUTPUT_DIR, "intermediate")

walk(c(OUTPUT_DIR, RESULTS_DIR, INTERMEDIATE_DIR), ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# API Functions ----
call_chatgpt <- function(prompt, user_message, temperature = 1.0) {
  create_chat_completion(
    model = "gpt-4o",
    messages = list(
      list(role = "system", content = prompt),
      list(role = "user", content = user_message)
    ),
    temperature = temperature,
    max_tokens = 4000
  ) %>% pluck("choices", 1, "message", "content")
}

call_deepseek <- function(prompt, user_message, temperature = 1.0) {
  api_key <- Sys.getenv("DEEPSEEK_API_KEY")
  if (api_key == "") stop("DEEPSEEK_API_KEY not found in environment")
  
  request_body <- list(
    model = "deepseek-v3",
    messages = list(
      list(role = "system", content = prompt),
      list(role = "user", content = user_message)
    ),
    temperature = temperature,
    max_tokens = 4000
  )
  
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
}

call_llm <- function(model_name, prompt, user_message, temperature = 1.0) {
  switch(model_name,
    "chatgpt" = call_chatgpt(prompt, user_message, temperature),
    "deepseek" = call_deepseek(prompt, user_message, temperature),
    stop(glue("Unknown model: {model_name}"))
  )
}

# Data Processing ----
parse_markdown_table <- function(markdown_string) {
  if (is.null(markdown_string) || is.na(markdown_string) || markdown_string == "") {
    return(NULL)
  }
  
  lines <- str_split_1(markdown_string, "\n") %>% str_trim() %>% keep(~nzchar(.x))
  if (length(lines) < 3) return(NULL)
  
  # Find table separator
  separator_indices <- which(str_detect(lines, "^\\|[\\s\\-\\|]+\\|$"))
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
  
  # Extract data
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
    
    df <- map_dfr(parsed_data, ~set_names(.x, headers))
    
    # Find and convert rate column
    rate_col <- names(df) %>% str_subset("(?i)rate.*%|expected.*rate") %>% first()
    if (!is.null(rate_col)) {
      df <- df %>%
        mutate(rate = as.numeric(.data[[rate_col]])) %>%
        select(-all_of(rate_col))
    }
    
    # Standardize columns
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

load_transcripts <- function() {
  list.files("../intermediate_data/texts/", pattern = "\\.txt$", full.names = TRUE) %>%
    map_dfr(~{
      filename <- basename(.x)
      date_match <- str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
      if (is.na(date_match)) return(tibble())
      
      tryCatch({
        text <- read_lines(.x) %>% paste(collapse = " ")
        tibble(date = as.Date(date_match), filename = filename, text = text)
      }, error = function(e) tibble())
    }) %>%
    arrange(date) 
}

load_gemini_results <- function() {
  tryCatch({
    gemini_file <- "../intermediate_data/aggregate_gemini_result/prompt_naive/2.5flash_2025-07-21.xlsx"
    
    if (!file.exists(gemini_file)) {
      cat(crayon::yellow("⚠️ Gemini file not found\n"))
      return(tibble())
    }
    
    read_xlsx(gemini_file) %>%
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
  }, error = function(e) {
    cat(crayon::red(glue("Gemini load error: {e$message}\n")))
    tibble()
  })
}

# Single Conference Processing ----
simulate_single_conference <- function(model_name, conference_info, analyst_prompt) {
  conference_date <- as.character(conference_info$date)
  cat(crayon::cyan(glue("→ {model_name} {conference_date}\n")))   # add this line

  
  # Check cache
  cache_file <- file.path(INTERMEDIATE_DIR, glue("{model_name}_{conference_date}.rds"))
  if (file.exists(cache_file)) {
    cat(crayon::blue(glue("📂 {model_name} {conference_date} (cached)\n")))
    return(readRDS(cache_file))
  }
  
  # Prepare prompt
  formatted_prompt <- str_replace_all(analyst_prompt, "\\[date\\]", conference_date)
  user_message <- glue(
    "ECB press conference for {conference_date}:\n\n{conference_info$text}\n\n",
    "Please simulate the 30 traders as instructed. Output only a markdown table."
  )
  
  # Try with retries
  for (attempt in 1:3) {
    if (attempt > 1) Sys.sleep(2^(attempt-1))
    
    tryCatch({
      cat(crayon::blue(glue("🔄 {model_name} {conference_date} (attempt {attempt})\n")))
      
      response <- call_llm(model_name, formatted_prompt, user_message, TEMPERATURE)
      parsed_df <- parse_markdown_table(response)
      
      if (!is.null(parsed_df) && nrow(parsed_df) > 0) {
        result <- parsed_df %>%
          mutate(conference_date = conference_date, model_name = model_name)
        
        saveRDS(result, cache_file)  # Save to cache
        
        cat(crayon::green(glue("✅ {model_name} {conference_date}: {nrow(result)} rows\n")))
        return(result)
      }
    }, error = function(e) {
      cat(crayon::red(glue("❌ Attempt {attempt}: {e$message}\n")))
    })
  }
  
  cat(crayon::red(glue("❌ {model_name} {conference_date} failed completely\n")))
  return(NULL)
}

# Main Analysis ----
run_cross_llm_analysis <- function(limit_conferences = NULL) {
  cat(crayon::blue("🚀 Cross-LLM Robustness Analysis\n"))
  cat(crayon::blue("Models: Gemini 2.5 Flash vs GPT-4o vs DeepSeek-V3\n\n"))
  
  # Load data
  transcript_data <- load_transcripts()
  gemini_data <- load_gemini_results()

    # Optionally limit number of conferences
  if (!is.null(limit_conferences)) {
    transcript_data <- head(transcript_data, limit_conferences)
    cat(crayon::yellow(glue("⚠️ Limiting to first {limit_conferences} conferences for testing\n")))
  }
  
  cat(glue("📊 Analyzing {nrow(transcript_data)} conferences\n"))
  
  # Load prompt
  source("create_prompts.R")
  analyst_prompt <- prompt_naive
  
  # Setup parallel processing
  plan(multisession, workers = MAX_WORKERS)
  
  # Process new models
  new_models <- c("chatgpt")
  
  new_results <- new_models %>%
    set_names() %>%
    map(~{
      cat(crayon::magenta(glue("🤖 Processing {toupper(.x)}\n")))
      
      model_results <- transcript_data %>%
        future_pmap(function(date, filename, text) {
          simulate_single_conference(.x, list(date = date, text = text), analyst_prompt)
        }, .options = furrr_options(seed = TRUE)) %>%
        discard(is.null)
      
      if (length(model_results) > 0) {
        combined <- bind_rows(model_results)
        write_csv(combined, file.path(OUTPUT_DIR, glue("{.x}_results.csv")))
        cat(crayon::green(glue("✅ {toupper(.x)}: {nrow(combined)} rows\n")))
        combined
      } else {
        NULL
      }
    }) %>%
    discard(is.null)
  
  plan(sequential)
  
  # Combine all results
  all_results <- list()
  if (nrow(gemini_data) > 0) all_results[["gemini"]] <- gemini_data
  all_results <- c(all_results, new_results)
  
  final_results <- bind_rows(all_results)
  
  # Save final results
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  write_csv(final_results, file.path(OUTPUT_DIR, glue("cross_llm_complete_{timestamp}.csv")))
  
  cat(crayon::green(glue("\n✅ Analysis complete: {nrow(final_results)} total rows\n")))
  return(final_results)
}

# Analysis Functions ----
compute_disagreement_measures <- function(df) {
  df %>%
    filter(!is.na(rate)) %>%
    group_by(model_name, conference_date, tenor) %>%
    summarise(
      predicted_sd = sd(rate, na.rm = TRUE),
      predicted_mean = mean(rate, na.rm = TRUE),
      n_traders = n(),
      .groups = "drop"
    ) %>%
    filter(n_traders >= 10, !is.na(predicted_sd))
}

load_market_data <- function() {
  tryCatch({
    readRDS("../intermediate_data/range_difference_df.rds") %>%
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
  }, error = function(e) {
    cat(crayon::red(glue("Market data error: {e$message}\n")))
    NULL
  })
}

compute_correlations <- function(disagreement_df, market_data_df) {
  merged_df <- disagreement_df %>%
    inner_join(market_data_df, by = c("conference_date", "tenor"))
  
  if (nrow(merged_df) == 0) return(NULL)
  
  # Compute correlations
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
  
  return(list(detailed = correlation_results, averages = model_averages, merged_data = merged_df))
}

compute_icc_analysis <- function(disagreement_df) {
  # Prepare data for ICC
  icc_data <- disagreement_df %>%
    select(model_name, conference_date, tenor, predicted_sd) %>%
    pivot_wider(names_from = model_name, values_from = predicted_sd, values_fill = NA)
  
  model_cols <- setdiff(names(icc_data), c("conference_date", "tenor"))
  if (length(model_cols) < 2) return(list())
  
  # ICC for each tenor
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
          
          list(
            tenor = .x,
            n_observations = nrow(complete_data),
            icc_2_1 = icc_result$results["ICC2", "ICC"],
            icc_2_1_lower = icc_result$results["ICC2", "lower bound"],
            icc_2_1_upper = icc_result$results["ICC2", "upper bound"],
            p_value = icc_result$results["ICC2", "p"]
          )
        }, error = function(e) NULL)
      } else {
        NULL
      }
    }) %>%
    discard(is.null)
  
  return(icc_results)
}

# Visualization ----
create_plots <- function(correlation_results, icc_results) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Correlation plot
  p1 <- correlation_results$detailed %>%
    mutate(
      significance = ifelse(spearman_p_value < 0.05, "*", ""),
      tenor = factor(tenor, levels = TARGET_TENORS)
    ) %>%
    ggplot(aes(x = tenor, y = spearman_correlation, fill = model_name)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_text(aes(label = paste0(round(spearman_correlation, 3), significance)), 
              position = position_dodge(0.9), vjust = -0.5) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Cross-LLM Performance: Correlations with Market Volatility",
      x = "OIS Tenor", y = "Spearman Correlation", fill = "Model"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # ICC plot
  if (length(icc_results) > 0) {
    icc_df <- map_dfr(icc_results, ~as_tibble(.x)) %>%
      mutate(tenor = factor(tenor, levels = TARGET_TENORS))
    
    p2 <- icc_df %>%
      ggplot(aes(x = tenor, y = icc_2_1)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      geom_errorbar(aes(ymin = icc_2_1_lower, ymax = icc_2_1_upper), width = 0.2) +
      geom_text(aes(label = round(icc_2_1, 3)), vjust = -0.5) +
      labs(
        title = "Cross-LLM Consistency: ICC Analysis",
        x = "OIS Tenor", y = "ICC(2,1)"
      ) +
      theme_minimal()
  } else {
    p2 <- ggplot() + labs(title = "ICC: Insufficient Data") + theme_void()
  }
  
  # Save plots
  ggsave(file.path(RESULTS_DIR, glue("correlations_{timestamp}.png")), p1, 
         width = 10, height = 6, dpi = 320, bg = "white")
  ggsave(file.path(RESULTS_DIR, glue("icc_{timestamp}.png")), p2, 
         width = 8, height = 6, dpi = 320, bg = "white")
  
  return(list(correlation_plot = p1, icc_plot = p2))
}

# Reporting ----
create_report <- function(correlation_results, icc_results) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(RESULTS_DIR, glue("report_{timestamp}.md"))
  
  # Build report
  avg_corr <- mean(correlation_results$detailed$spearman_correlation, na.rm = TRUE)
  sig_corrs <- sum(correlation_results$detailed$spearman_p_value < 0.05, na.rm = TRUE)
  total_corrs <- nrow(correlation_results$detailed)
  
  report_lines <- c(
    "# Cross-LLM Robustness Analysis Report",
    glue("**Generated:** {Sys.time()}"),
    "",
    "## Summary",
    glue("- **Models tested:** {paste(unique(correlation_results$detailed$model_name), collapse = ', ')}"),
    glue("- **Average correlation:** {round(avg_corr, 3)}"),
    glue("- **Significant correlations:** {sig_corrs}/{total_corrs}"),
    "",
    "## Model Performance",
    ""
  )
  
  # Add detailed results
  for (model in unique(correlation_results$detailed$model_name)) {
    model_data <- filter(correlation_results$detailed, model_name == model)
    report_lines <- c(
      report_lines,
      glue("### {toupper(model)}:"),
      map_chr(1:nrow(model_data), ~{
        row <- model_data[.x, ]
        glue("- **{row$tenor}:** ρ = {round(row$spearman_correlation, 3)} (p = {round(row$spearman_p_value, 3)})")
      }),
      ""
    )
  }
  
  # ICC results
  if (length(icc_results) > 0) {
    report_lines <- c(
      report_lines,
      "## ICC Analysis",
      map_chr(icc_results, ~{
        glue("- **{.x$tenor}:** ICC(2,1) = {round(.x$icc_2_1, 3)} (p = {round(.x$p_value, 3)})")
      }),
      ""
    )
  }
  
  writeLines(report_lines, filename)
  cat(glue("📄 Report: {filename}\n"))
  return(filename)
}


# Main_analysis ----

main_analysis <- function(limit_conferences = NULL) {
  cat(crayon::blue("🎯 Starting Cross-LLM Robustness Analysis\n\n"))
  
  # Run analysis
  results_df <- run_cross_llm_analysis(limit_conferences = limit_conferences)
  disagreement_df <- compute_disagreement_measures(results_df)
  market_data_df <- load_market_data()
  
  if (is.null(market_data_df)) stop("❌ No market data")
  
  correlation_results <- compute_correlations(disagreement_df, market_data_df)
  if (is.null(correlation_results)) stop("❌ No correlations computed")
  
  icc_results <- compute_icc_analysis(disagreement_df)
  
  # Create outputs
  plots <- create_plots(correlation_results, icc_results)
  report_file <- create_report(correlation_results, icc_results)
  
  # Save data
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  write_xlsx(
    list(
      correlations = correlation_results$detailed,
      model_averages = correlation_results$averages,
      disagreement = disagreement_df,
      raw_results = results_df,
      icc_results = if(length(icc_results) > 0) map_dfr(icc_results, ~as_tibble(.x)) else tibble()
    ),
    file.path(RESULTS_DIR, glue("analysis_{timestamp}.xlsx"))
  )
  
  # Summary
  cat(crayon::green("\n🎉 Analysis Complete!\n"))
  cat(glue("📊 Models: {n_distinct(results_df$model_name)}\n"))
  cat(glue("📊 Avg correlation: {round(mean(correlation_results$detailed$spearman_correlation), 3)}\n"))
  cat(glue("📁 Results in: {RESULTS_DIR}\n"))
  
  return(list(
    results = results_df,
    correlations = correlation_results,
    icc = icc_results,
    plots = plots
  ))
}

# Run if interactive
if (interactive()) {
  cat("🚀 Ready! Run: results <- main_analysis()\n")
} else {
  main_analysis()
}


results <- main_analysis(limit_conferences = 5)  # For testing, limit to first 5 conferences
