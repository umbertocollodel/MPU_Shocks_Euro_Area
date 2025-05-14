# Load necessary libraries and set parameters: -----


library(gemini.R)


setAPI("AIzaSyA1O-J8XK-Y5Ymr341izyQvsDlb2UkETp4")



# Write a prompt for LLM: -----

prompt=c("
You are a bank following the press conferences of the ECB Governing Council. These conferences communicate decisions about the monetary policy stance, provide an assessment of the state of the economy, and include a Q&A session with journalists.

Your task is to manage your exposure to interest rate fluctuations by buying/selling Overnight Index Swap (OIS) rates based on the information from these conferences.

We are in [date] (format: YYYY-MM-DD).

Using only the words from the conference and the information available as of [date], assess your confusion about the expected interest rate developments.

For each horizon (short-term: 3 months-1 year, medium-term: 2-5 years, long-term: 10 years), provide the following:

1. A confusion score from 0 to 100.
2. The reason for your chosen value.
3. The main source of confusion (introductory statement, Q&A, or both).
4. A rephrased version of the introductory statement and Q&A answers to reduce confusion.

Output the results in a table with three columns per task (one per horizon). The table should have dimensions 1x12 (number of conferences; 4 tasks * 3 horizons).

Do not incorporate any data that was not available as of [date] in your assessment.

Provide only the table as output, not any text.
")



# Run LLM -----

# Create list with all press conferences:

names_ecb_presconf=list.files("../intermediate_data/texts/") %>% 
  str_subset("\\d") %>% 
  str_remove("\\.txt")


ecb_pressconf=list.files("../intermediate_data/texts/") %>% 
  str_subset("\\d") %>% 
  paste0("../intermediate_data/texts/",.) %>% 
  map(~ readtext(.x)) %>% 
  map(~ .$text) %>% 
  set_names(names_ecb_presconf)
  

# Initialize counters
request_count <- 0
start_time <- Sys.time()

# Function to make requests with delay and tracking
make_request <- function(text, prompt, seed = 120) {
  Sys.sleep(5)  # Wait for 5 seconds between requests
  request_count <<- request_count + 1
  gemini(text, seed = seed)
  
}

# Run Gemini LLM:

result <- ecb_pressconf %>%
  map(~ paste0(prompt, "Press Conference:", .x)) %>%
  map(~ make_request(.x, seed = 120)) %>% 
  set_names(names_ecb_presconfresult)

# Print metrics
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






