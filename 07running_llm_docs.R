# Load necessary libraries and set parameters: -----

setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# Load and install all required packages
pacman::p_load(
  gemini.R,
  cli,
  httr2,
  readtext,
  crayon,
  stringr,
  purrr,
  readr,
  writexl
)

setAPI("AIzaSyA1O-J8XK-Y5Ymr341izyQvsDlb2UkETp4")

# Create custom function to send request to Gemini API with higher timeout time:

new_gemini <- function(prompt, model = "2.0-flash", temperature = 1, maxOutputTokens = 8192,
                       topK = 40, topP = 0.95, seed = 1234) {
  
  model_query <- paste0("gemini-", model, ":generateContent")
  url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query)
  api_key <- Sys.getenv("GEMINI_API_KEY")
  
  sb <- cli_status("Gemini is answering...")
  
  # Create generation config
  generation_config <- list(
    temperature = temperature,
    maxOutputTokens = maxOutputTokens,
    topP = topP,
    topK = topK,
    seed = seed
  )
  
  # Add responseModalities only for image generation model
  if (model == "2.0-flash-exp-image-generation") {
    generation_config$responseModalities <- list("Text", "Image")
  }
  
  # Create request body as a separate list
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
    req_timeout(120)  # Increase the timeout here (in seconds)
  
  resp <- req_perform(req)
  
  # Check the status code of the response
  if (resp$status_code != 200) {
    cli_status_clear(id = sb)
    cli_alert_danger(paste0("Error in generate request: Status code ", resp$status_code))
    return(NULL)
  }
  
  cli_status_clear(id = sb)
  
  candidates <- resp_body_json(resp)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  return(outputs)
}



# Write a prompt for LLM request: -----

prompt = c("
Context:
You are a bank following the press conferences of the ECB Governing Council. 
These conferences communicate decisions about the monetary policy stance, provide an assessment 
of the state of the economy, and include a Q&A session with journalists. 
We are in [date].

Task:
Manage your exposure to interest rate fluctuations by buying/selling
Overnight Index Swap (OIS) rates based on the information from these conferences.
Using only the words from the conference and the economic information available as of [date],
assess your confusion about the expected interest rate developments.

Confusion Definition:
Confusion is defined as the lack of clarity or certainty about the expected 
developments in interest rates.

Scoring Scale (0–10):
- 0: No confusion – clear, comprehensive, no doubt.
- 1–2: Minimal confusion – mostly clear, minor ambiguities.
- 3–4: Low confusion – generally clear, some uncertainty.
- 5–6: Moderate confusion – mixed signals, significant ambiguities.
- 7–8: High confusion – unclear, many ambiguities.
- 9–10: Maximum confusion – very unclear, no guidance.

Evaluation Criteria:
- Clarity of Communication
- Consistency of Information
- Detail and Specificity
- Context and Background

Output:
For each interest rate horizon (short-term: 3 months–2 years, long-term: 5–10 years), provide the following in a table:

| Date         | Horizon       | Confusion Score (0–10) | Reason for Score (max 3 sentences) | Rephrased Version (as ECB Governor)  | Rephrased Version Score  | Key Differences (2–3 bullet points or 2 sentences) |
|--------------|----------------|------------------------|------------------------------------|--------------------------------------|--------------------------|-----------------------------------------------------|
| YYYY-MM-DD   | Short-term     |                        |                                    |                                      |                          |                                                     |
| YYYY-MM-DD   | Long-term      |                        |                                    |                                      |                          |                                                     |

Column Instructions:
- Column 1: The date of the press conference (format: YYYY-MM-DD).You can find it in`Press Conference on`.
- Column 2: Horizon (short-term or long-term).
- Column 3: A confusion score from 0 to 10 (0 = no confusion, 10 = maximum confusion - float).
- Column 4: The reason for your chosen value in a short paragraph making reference to the evaluation criteria.
- Column 5: Rewrite the ECB’s message as if you are the ECB President or Chief Economist delivering the same policy decision, but with maximum clarity, confidence, and strategic intent. 
  Your goal is to reduce confusion, eliminate ambiguity, and provide a clear signal to markets about the rationale and future direction of policy. 
  You may restructure, reframe, or enhance the message to improve its effectiveness — not just reword it.
- Column 6: Provide a new confusion score (0–10) for the rephrased version.
- Column 7: Summarize the key differences between the original and rephrased versions. Focus on removed ambiguities, clarified language, and strategic improvements. Limit to 2–3 bullet points or 2 sentences.

Important:
- Do not use any data not available as of [date].
- Keep all responses concise and structured.
- Output only the table, no additional text.
- The prompt will include multiple press conferences. Be careful.
")





# Create a list of press conferences with dates and names: ----

dates_ecb_presconf=list.files("../intermediate_data/texts/") %>% 
  str_subset("\\d") %>% 
  str_remove("\\.txt") %>% 
  str_extract("\\d{4}-\\d{2}-\\d{2}")

names_ecb_presconf=list.files("../intermediate_data/texts/") %>% 
  str_subset("\\d") %>% 
  str_remove("\\.txt")


ecb_pressconf=list.files("../intermediate_data/texts/") %>% 
  str_subset("\\d") %>% 
  paste0("../intermediate_data/texts/",.) %>% 
  map(~ readtext(.x)) %>% 
  map(~ .$text) %>% 
  set_names(names_ecb_presconf)

# Run LLM remotely: ----

# Initialize time and log

log_file <- "failed_requests.log"
start_time <- Sys.time()

# Clear previous log

if (file.exists(log_file)) file.remove(log_file)

# Custom function to apply gemini prompt and save resulting rds file

make_request <- function(text, date, seed = 120) {
  
  Sys.sleep(5)
  
  result <- tryCatch({
    res <- new_gemini(text, seed = seed, temperature = 0.5)
    saveRDS(res, file = paste0("../intermediate_data/gemini_result/", date, ".rds"))
    cat(green(paste0("✅ Press conference on ", date, " processed and saved.\n")))
    TRUE
  }, error = function(e) {
    cat(red(paste0("❌ Error processing press conference on ", date, "\n")))
    write(paste0(date, ": ", e$message), file = log_file, append = TRUE)
    FALSE
  })
}

# Run the requests

# Define batch size
batch_size <- 6

# Split into batches
batches <- split(seq_along(ecb_pressconf), ceiling(seq_along(ecb_pressconf) / batch_size))

# Loop over batches
for (i in seq_along(batches)) {
  batch_indices <- batches[[i]]
  batch_dates <- dates_ecb_presconf[batch_indices]
  batch_names <- names_ecb_presconf[batch_indices]
  batch_texts <- ecb_pressconf[batch_indices]
  
  # Combine all press conferences in the batch
  batch_input <- map2_chr(batch_texts, batch_dates, ~ {
    paste0("Press Conference on ", .y, ":\n", .x, "\n\n")
  }) %>% paste(collapse = "\n---\n")
  
  # Inject into prompt
  full_prompt <- gsub("\\[date\\]", paste(batch_dates, collapse = ", "), prompt)
  full_prompt <- paste0(full_prompt, batch_input)
  
  # Save with batch ID
  batch_id <- paste0("batch_", i)
  
  make_request(text = full_prompt, date = batch_id)
}


# Print metrics: 

end_time <- Sys.time()
total_time <- end_time - start_time

cat("Total time taken:", total_time, "seconds\n")