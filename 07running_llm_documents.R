# Load necessary libraries and set parameters: -----

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


# Libraries and API key:

library(gemini.R)


setAPI("AIzaSyA1O-J8XK-Y5Ymr341izyQvsDlb2UkETp4")



# Write a prompt for LLM request: -----

prompt=c("
Context: You are a bank following the press conferences of the ECB Governing Council. 
These conferences communicate decisions about the monetary policy stance, provide an assessment of the state of the economy, and include a Q&A session with journalists.
We are in [date].

Task: manage your exposure to interest rate fluctuations by buying/selling Overnight Index Swap (OIS) rates based on the information from these conferences.
Using only the words from the conference and the information available as of [date], assess your confusion about the expected interest rate developments.

**Confusion Definition**: Confusion is defined as the lack of clarity or certainty about the expected developments in interest rates.

**Score Levels**:
- **0**: No confusion. The information is clear, comprehensive, and leaves no room for doubt.
- **1-2**: Minimal confusion. The information is mostly clear, with only minor ambiguities.
- **3-4**: Low confusion. The information is generally clear, but there are some areas of uncertainty.
- **5-6**: Moderate confusion. The information is mixed, with significant ambiguities and uncertainties.
- **7-8**: High confusion. The information is unclear, with many ambiguities and conflicting signals.
- **9-10**: Maximum confusion. The information is very unclear, with pervasive ambiguities and no clear guidance.

**Criteria for Evaluation**:
- **Clarity of Communication**: How clearly are the interest rate developments communicated?
- **Consistency of Information**: Are there any conflicting statements or signals?
- **Detail and Specificity**: How detailed and specific is the information provided?
- **Context and Background**: Is there sufficient context and background information to understand the developments?

**Examples**:
- **Score 0**: The introductory statement and Q&A provide clear, detailed, and consistent information about the expected interest rate developments, with no conflicting signals.
- **Score 5**: The introductory statement is somewhat clear, but the Q&A introduces conflicting signals and ambiguities, leading to moderate confusion.
- **Score 10**: Both the introductory statement and Q&A are vague, with many conflicting signals and no clear guidance, leading to maximum confusion.

**Consistency Checks**:
- **Review Process**: Implement a review process where multiple analysts independently score the same conference and then compare scores to ensure consistency.
- **Feedback Loop**: Establish a feedback loop where analysts can discuss and resolve discrepancies in their scores.
- **Historical Context**: Review previous press conferences to identify trends.


Output: for each horizon (short-term: 3 months-1 year, medium-term: 2-5 years, long-term: 10 years), provide the following:

1. A confusion score from 0 to 10 (0 = no confusion, 10 = maximum confusion - float).
2. The reason for your chosen value in a short paragraph making reference to the evaluation criteria.
3. The main source of confusion in the confusion score (introductory statement, Q&A, or both).
4. A rephrased version of the introductory statement and Q&A answers to reduce confusion. Keep the format of the original.
5. A confusion score for the rephrased version (point 4).

Output the results in a table with three columns per task (one per horizon). 
The table should have dimensions 1x12 (number of conferences; 4 tasks * 3 horizons).

Do not incorporate any data that was not available as of [date] in your assessment.

Provide only the table as output, not any text.
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

# Initialize counters

request_count <- 0
start_time <- Sys.time()

# Function to make requests with delay and tracking
make_request <- function(text, prompt, seed = 120) {
  
  Sys.sleep(5)  # Wait for 5 seconds between requests
  
  
  request_count <<- request_count + 1
  new_gemini(text, seed = seed, temperature = 0.5)
  cat(crayon::green(paste0("Press conference number",request_count," processed!\n")))
}

# Run Gemini LLM:

result <- ecb_pressconf %>%
  map(~ paste0(prompt, "Press Conference:", .x)) %>%
  map2(dates_ecb_presconf, ~ gsub("\\[date\\]", .y, .x)) %>% 
  map(~ make_request(.x, 
                     seed = 120)) %>% 
  set_names(names_ecb_presconf)

# Print metrics: 

end_time <- Sys.time()
total_time <- end_time - start_time


cat("Total requests made:", request_count, "\n")
cat("Total time taken:", total_time, "seconds\n")



# Convert output in proper format: ----


df <- result %>% 
  map(~ .x %>% readr::read_delim(delim = "|", trim_ws = TRUE, skip = 2)) %>%
  keep(~ nrow(.x) == 2) %>% 
  bind_rows(.id = "date") %>% 
  select(-`...1`,-`...14`) %>% 
  filter(!str_detect(confusion_s,"--")) %>% 
  mutate(across(starts_with("confusion"), as.numeric))



# Export: 

writexl::write_xlsx(df,"../intermediate_data/llm_assessment.xlsx")


# Plot: ----

# Assuming df is your data frame
df_long <- df %>%
  mutate(across(starts_with("confusion"), as.numeric)) %>% 
  pivot_longer(cols = starts_with("confusion"), names_to = "confusion_type", values_to = "value")

ggplot(df_long, aes(x = confusion_type, y = date, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Confusion Levels Over Time",
       x = "Confusion Type",
       y = "Date",
       fill = "Value")






