#===============================================================================
# GEMINI API INTERFACE
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# This module provides functions for interacting with Google's Gemini API.
# It includes:
#   - new_gemini(): Main API call function with extended timeout
#   - process_single_conference(): Wrapper for processing ECB press conferences
#
# Dependencies: httr2, crayon
#
# Usage:
#   source("src/llm_api/gemini_api.R")
#   response <- new_gemini("Your prompt here", model = "2.5-flash")
#===============================================================================


#' Send Request to Gemini API with Extended Timeout
#'
#' @description
#' Custom wrapper around Gemini API with increased timeout (120s) and robust
#' error handling. Supports various generation parameters.
#'
#' @param prompt Character string containing the full prompt
#' @param model Character. Model version (default: "2.5-flash")
#' @param temperature Numeric. Controls randomness (0-2, default: 1)
#' @param maxOutputTokens Numeric. Maximum response length (default: 1000000)
#' @param topK Numeric. Top-K sampling parameter (default: 40)
#' @param topP Numeric. Nucleus sampling parameter (default: 0.95)
#' @param seed Numeric. Random seed for reproducibility (default: 1234)
#'
#' @return Character vector containing the model's response
#'
#' @details
#' Temperature = 1 provides balanced randomness suitable for simulating
#' diverse trader behavior. Higher values increase creativity/variability.
#'
#' @examples
#' \dontrun{
#' response <- new_gemini("What is monetary policy?", temperature = 0.5)
#' }
#'
#' @export
new_gemini <- function(prompt,
                       model = "2.5-flash",
                       temperature = 1,
                       maxOutputTokens = 1000000,
                       topK = 40,
                       topP = 0.95,
                       seed = 1234) {

  # Construct API endpoint
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")

  # Verify API key is set
  if (api_key == "") {
    stop("GEMINI_API_KEY environment variable not set. Please set it before calling this function.")
  }

  # Create generation configuration
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )

  # Add responseModalities for image generation model (future-proofing)
  if (model == "2.0-flash-exp-image-generation") {
    generation_config$responseModalities <- list("Text", "Image")
  }

  # Construct request body
  request_body <- list(
    contents = list(
      parts = list(
        list(text = prompt)
      )
    ),
    generationConfig = generation_config
  )

  # Build and execute HTTP request
  req <- httr2::request(url) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(request_body) |>
    httr2::req_timeout(120)  # Extended timeout for long documents

  resp <- httr2::req_perform(req)

  # Check response status
  if (resp$status_code != 200) {
    stop(paste0("API Error: Status code ", resp$status_code,
                " - ", httr2::resp_body_string(resp)))
  }

  # Extract and return generated text
  candidates <- httr2::resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) {
    candidate$content$parts
  }))

  return(outputs)
}


#' Process Single Press Conference
#'
#' @description
#' Wrapper function to process one ECB press conference through the LLM.
#' Designed for use with furrr::future_map for parallel processing.
#'
#' @param conf_date Character. Date of conference (YYYY-MM-DD format)
#' @param conf_text Character. Full transcript text
#' @param prompt_template Character. Prompt template with [date] placeholder
#' @param output_dir Character. Directory to save results
#' @param log_file_path Character. Path to error log file
#' @param model Character. Gemini model version (default: "2.5-flash")
#' @param seed Numeric. Random seed for reproducibility (default: 120)
#' @param temperature Numeric. Temperature parameter (default: 1)
#' @param max_attempts Numeric. Maximum retry attempts (default: 5)
#'
#' @return Logical. TRUE if successful, FALSE otherwise
#'
#' @details
#' - Implements exponential backoff: waits 5s, 10s, 15s, ... between retries
#' - Saves results as RDS files for memory efficiency
#' - Logs all errors to specified log file
#'
#' @examples
#' \dontrun{
#' success <- process_single_conference(
#'   conf_date = "2024-01-15",
#'   conf_text = "Full transcript...",
#'   prompt_template = prompt_naive,
#'   output_dir = "../intermediate_data/gemini_result/prompt_naive",
#'   log_file_path = "errors.log"
#' )
#' }
#'
#' @export
process_single_conference <- function(conf_date,
                                      conf_text,
                                      prompt_template,
                                      output_dir,
                                      log_file_path,
                                      model = "2.5-flash",
                                      seed = 120,
                                      temperature = 1,
                                      max_attempts = 5) {

  cat(crayon::yellow(paste0("🔄 Starting processing for ", conf_date, "\n")))

  # Construct conference-specific prompt
  full_prompt_for_conf <- gsub("\\[date\\]", conf_date, prompt_template)
  full_prompt_for_conf <- paste0(
    full_prompt_for_conf,
    "Press Conference on ", conf_date, "\n",
    "Text:", conf_text, "\n\n"
  )

  # Retry loop with exponential backoff
  result <- FALSE
  for (attempt in 1:max_attempts) {
    Sys.sleep(5 * attempt)  # Exponential backoff: 5s, 10s, 15s, ...

    result <- tryCatch({
      # Call LLM API
      res <- new_gemini(
        full_prompt_for_conf,
        model = model,
        seed = seed,
        temperature = temperature
      )

      # Save result
      output_path <- file.path(output_dir, paste0(conf_date, ".rds"))
      saveRDS(res, file = output_path)

      cat(crayon::green(paste0("✅ Press conference on ", conf_date,
                              " processed and saved.\n")))
      return(TRUE)

    }, error = function(e) {
      # Log error
      cat(crayon::red(paste0("❌ Error processing press conference on ",
                            conf_date, ": ", e$message, "\n")))
      write(paste0(conf_date, ": ", e$message),
            file = log_file_path,
            append = TRUE)
      return(FALSE)
    })

    if (result) break  # Exit loop if successful
  }

  # Report final failure if all attempts exhausted
  if (!result) {
    cat(crayon::red(paste0("❌ All ", max_attempts,
                          " attempts failed for ", conf_date, "\n")))
  }

  return(result)
}


#' Process Single Conference with Historical Context
#'
#' @description
#' Extended version that adds historical volatility context to the prompt.
#' Used by the historical_surprise model.
#'
#' @param conf_date Character. Date of conference (YYYY-MM-DD format)
#' @param conf_text Character. Full transcript text
#' @param prompt_template Character. Prompt template with [date] placeholder
#' @param range_diff_df DataFrame. Historical standard deviation data
#' @param history_window Integer. Number of previous conferences (default: 3)
#' @param output_dir Character. Directory to save results
#' @param log_file_path Character. Path to error log file
#' @param model Character. Gemini model version (default: "2.5-flash")
#' @param seed Numeric. Random seed for reproducibility (default: 120)
#' @param temperature Numeric. Temperature parameter (default: 1)
#' @param max_attempts Numeric. Maximum retry attempts (default: 5)
#'
#' @return Logical. TRUE if successful, FALSE otherwise
#'
#' @export
process_conference_with_history <- function(conf_date,
                                           conf_text,
                                           prompt_template,
                                           range_diff_df,
                                           history_window = 3,
                                           output_dir,
                                           log_file_path,
                                           model = "2.5-flash",
                                           seed = 120,
                                           temperature = 1,
                                           max_attempts = 5) {

  cat(crayon::yellow(paste0("🔄 Starting processing for ", conf_date,
                           " with historical context\n")))

  # Get previous N std devs for each tenor
  std_info <- range_diff_df %>%
    dplyr::filter(date < as.Date(conf_date)) %>%
    dplyr::group_by(tenor) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::slice_head(n = history_window) %>%
    dplyr::summarise(
      historical_std = paste(
        paste0("Date: ", date, ", Before: ",
               round(range_before, 4), ", After: ", round(range_after, 4)),
        collapse = "; "
      ),
      .groups = "drop"
    )

  # Build historical context string
  history_context <- paste0(
    "\n\nHistorical Context (Previous ", history_window, " Conferences):\n",
    paste(paste0("- ", std_info$tenor, ": ", std_info$historical_std),
          collapse = "\n"),
    "\n\n"
  )

  # Construct conference-specific prompt
  full_prompt_for_conf <- gsub("\\[date\\]", conf_date, prompt_template)
  full_prompt_for_conf <- paste0(
    full_prompt_for_conf,
    history_context,
    "Press Conference on ", conf_date, "\n",
    "Text:", conf_text, "\n\n"
  )

  # Retry loop with exponential backoff
  result <- FALSE
  for (attempt in 1:max_attempts) {
    Sys.sleep(5 * attempt)

    result <- tryCatch({
      # Call LLM API
      res <- new_gemini(
        full_prompt_for_conf,
        model = model,
        seed = seed,
        temperature = temperature
      )

      # Save result
      output_path <- file.path(output_dir, paste0(conf_date, ".rds"))
      saveRDS(res, file = output_path)

      cat(crayon::green(paste0("✅ Press conference on ", conf_date,
                              " processed and saved.\n")))
      return(TRUE)

    }, error = function(e) {
      cat(crayon::red(paste0("❌ Error processing press conference on ",
                            conf_date, ": ", e$message, "\n")))
      write(paste0(conf_date, ": ", e$message),
            file = log_file_path,
            append = TRUE)
      return(FALSE)
    })

    if (result) break
  }

  if (!result) {
    cat(crayon::red(paste0("❌ All ", max_attempts,
                          " attempts failed for ", conf_date, "\n")))
  }

  return(result)
}

#===============================================================================
# END OF MODULE
#===============================================================================
