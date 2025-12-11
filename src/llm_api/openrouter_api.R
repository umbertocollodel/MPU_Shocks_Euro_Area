#===============================================================================
# OPENROUTER API INTERFACE
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# This module provides functions for interacting with OpenRouter API.
# It includes:
#   - new_openrouter(): Main API call function with extended timeout
#   - process_single_conference_openrouter(): Wrapper for processing ECB press conferences
#
# Dependencies: httr2, crayon
#
# Usage:
#   source("src/llm_api/openrouter_api.R")
#   response <- new_openrouter("Your prompt here", model = "qwen/qwen3-235b-a22b-instruct")
#===============================================================================


#' Send Request to OpenRouter API with Extended Timeout
#'
#' @description
#' Custom wrapper around OpenRouter API with increased timeout (120s) and robust
#' error handling. Supports various generation parameters.
#'
#' @param prompt Character string containing the full prompt
#' @param model Character. Model identifier (default: "qwen/qwen3-235b-a22b-instruct")
#' @param temperature Numeric. Controls randomness (0-2, default: 1)
#' @param max_tokens Numeric. Maximum response length (default: 100000)
#' @param top_p Numeric. Nucleus sampling parameter (default: 0.95)
#' @param seed Numeric. Random seed for reproducibility (default: 120)
#'
#' @return Character vector containing the model's response
#'
#' @details
#' Temperature = 1 provides balanced randomness suitable for simulating
#' diverse trader behavior. Higher values increase creativity/variability.
#'
#' @examples
#' \dontrun{
#' response <- new_openrouter("What is monetary policy?", temperature = 0.5)
#' }
#'
#' @export
new_openrouter <- function(prompt,
                           model = "qwen/qwen3-235b-a22b-instruct",
                           temperature = 1,
                           max_tokens = 100000,
                           top_p = 0.95,
                           seed = 120) {

  # API endpoint
  url <- "https://openrouter.ai/api/v1/chat/completions"
  api_key <- "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"


  # Verify API key is set
  if (api_key == "") {
    stop("OPENROUTER_API_KEY environment variable not set. Please set it before calling this function.")
  }

  # Construct request body
  request_body <- list(
    model = model,
    messages = list(
      list(role = "user", content = prompt)
    ),
    temperature = temperature,
    max_tokens = max_tokens,
    top_p = top_p,
    seed = seed
  )

  # Build and execute HTTP request
  req <- httr2::request(url) |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(request_body) |>
    httr2::req_timeout(120)  # Extended timeout for long documents

  resp <- httr2::req_perform(req)

  # Check response status
  if (resp$status_code != 200) {
    stop(paste0("API Error: Status code ", resp$status_code,
                " - ", httr2::resp_body_string(resp)))
  }

  # Extract and return generated text
  response_json <- httr2::resp_body_json(resp)
  output <- response_json$choices[[1]]$message$content

  return(output)
}


#' Process Single Press Conference via OpenRouter
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
#' @param model Character. OpenRouter model identifier (default: "qwen/qwen3-235b-a22b-instruct")
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
#' success <- process_single_conference_openrouter(
#'   conf_date = "2024-01-15",
#'   conf_text = "Full transcript...",
#'   prompt_template = prompt_naive,
#'   output_dir = "../intermediate_data/openrouter_result/qwen3_naive",
#'   log_file_path = "errors.log"
#' )
#' }
#'
#' @export
process_single_conference_openrouter <- function(conf_date,
                                                  conf_text,
                                                  prompt_template,
                                                  output_dir,
                                                  log_file_path,
                                                  model = "qwen/qwen3-235b-a22b-instruct",
                                                  seed = 120,
                                                  temperature = 1,
                                                  max_attempts = 5) {

  cat(crayon::yellow(paste0("Processing ", conf_date, "\n")))

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
      res <- new_openrouter(
        full_prompt_for_conf,
        model = model,
        seed = seed,
        temperature = temperature
      )

      # Save result
      output_path <- file.path(output_dir, paste0(conf_date, ".rds"))
      saveRDS(res, file = output_path)

      cat(crayon::green(paste0("Press conference on ", conf_date,
                               " processed and saved.\n")))
      return(TRUE)

    }, error = function(e) {
      # Log error
      cat(crayon::red(paste0("Error processing press conference on ",
                             conf_date, ": ", e$message, "\n")))
      write(paste0(Sys.time(), " - ", conf_date, ": ", e$message),
            file = log_file_path,
            append = TRUE)
      return(FALSE)
    })

    if (result) break  # Exit loop if successful
  }

  # Report final failure if all attempts exhausted
  if (!result) {
    cat(crayon::red(paste0("All ", max_attempts,
                           " attempts failed for ", conf_date, "\n")))
  }

  return(result)
}

#===============================================================================
# END OF MODULE
#===============================================================================
