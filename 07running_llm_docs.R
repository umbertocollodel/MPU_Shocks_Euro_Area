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
  writexl,
  scales,
  showtext,
  readxl
)

setAPI(Sys.getenv("MY_API_KEY"))

# Create custom function to send request to Gemini API with higher timeout time:

new_gemini <- function(prompt, model = "2.0-flash", temperature = 1, maxOutputTokens = 1000000,
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

prompt <- c("
Context:
You are simulating the Euro area interest rate swap market, composed of 20 individual traders.
These traders interpret the ECB Governing Council press conference, which communicates monetary policy decisions, economic assessments, and includes a Q&A session with journalists.
Each trader then makes a trading decision to maximize profit based on their interpretation of the conference and their unique characteristics.

Trader Characteristics:
Each trader has the following attributes:
- Risk Aversion: High / Medium / Low — determines sensitivity to uncertainty and preference for stability.
- Behavioral Biases (1–2 per trader): e.g., Confirmation Bias, Overconfidence, Anchoring, Herding, Loss Aversion, Recency Bias.
- Interpretation Style (1 per trader): e.g., Fundamentalist, Sentiment Reader, Quantitative, Skeptic, Narrative-Driven.

Task:
You are given a certain number of distinct ECB press conferences.
For each of the 20 traders, simulate their individual trading action in the interest rate swap market across three tenors (3 months, 2 years, 10 years).
For each tenor, the trader must:
   - Provide an expected rate direction: Up / Down / Unchanged
   - Provide a new expected swap rate (in percent, to two decimal places)
   - Provide a one-sentence rationale for the trading decision

Output:
Provide a table with the following structure for each press conference, trader, and interest rate tenor:

| Date       | Trader ID | Tenor   | Expected Direction | New Expected Rate (%) | Rationale (1 sentence)          |
|------------|-----------|---------|--------------------|------------------------|--------------------------------|
| YYYY-MM-DD | T001      | 3M      | Up                 | 3.15                   | [Trader's rationale]           |
| YYYY-MM-DD | T001      | 2Y      | Down               | 2.85                   | [Trader's rationale]           |
| ...        | ...       | ...     | ...                | ...                    | ...                            |

Guidelines:
- Use only the information available as of [date].
- Do not aggregate or summarize responses.
- Reflect diversity in interpretation, risk tolerance, and horizon. Rationale must be unique for each trader and can vary across tenors.
- Output only a markdown table with the specified columns, no additional text. Do not use JSON or any other data serialization format.
- If multiple press conferences are included, clearly distinguish between them using the 'Date' field.
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

# Retrieve OIS rates pre-conference: ----

ois_daily_df <- read_xlsx("../raw_data/ois_daily_data.xlsx",skip = 1) %>% 
  select(1,2,4,6) %>% 
  setNames(c("date","3M","2Y","10Y"))



# Run LLM remotely: ----

# Initialize time and log

log_file <- "failed_requests.log"
start_time <- Sys.time()

# Clear previous log

if (file.exists(log_file)) file.remove(log_file)

# Custom function to apply gemini prompt and save resulting rds file


make_request <- function(text, date, seed = 120, max_attempts = 5) {
  
  for (attempt in 1:max_attempts) {
    
    Sys.sleep(5 * attempt) # Exponential backoff
    
    result <- tryCatch({
      res <- new_gemini(text, seed = seed, temperature = 1)
      saveRDS(res, file = paste0("../intermediate_data/gemini_result/", date, ".rds"))
      cat(crayon::green(paste0("✅ Press conference on ", date, " processed and saved.\n")))
      return(TRUE)
    }, error = function(e) {
      cat(crayon::red(paste0("❌ Error processing press conference on ", date, "\n")))
      write(paste0(date, ": ", e$message), file = log_file, append = TRUE)
      return(FALSE)
    })
    
    if (result) break# Exit loop if successful
  }
  
  if (!result) {
    cat(crayon::red(paste0("❌ All attempts failed for ", date, "\n")))
  }
}


# Run the requests

# Define batch size
batch_size <- 3

# Split into batches
batches <- split(seq_along(ecb_pressconf), ceiling(seq_along(ecb_pressconf) / batch_size))

# Loop over batches
for (i in seq_along(batches)) {
  batch_indices <- batches[[i]]
  batch_dates <- dates_ecb_presconf[batch_indices]
  batch_ois_values <- ois_daily_df[batch_dates,-1] %>% 
    split(seq_len(nrow(.))) %>% 
    map(function(tbl_row) {
      vec <- as.character(tbl_row[1, ])
      names(vec) <- names(tbl_row)
      paste(paste0(names(vec), ": ", vec), collapse = ", ")
    })
  
  batch_texts <- ecb_pressconf[batch_indices]
  
  
  # Combine all press conferences in the batch with OIS values
  batch_input <- pmap_chr(
    list(batch_texts, batch_dates, batch_ois_values),
    function(text, date, ois_values) {
      paste0("Press Conference on ", date, "\n",
             "OIS rates pre-conference: ", ois_values, "\n", 
             "Text:",text, "\n\n")
    }
  ) %>% paste(collapse = "\n---\n")
  
  
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