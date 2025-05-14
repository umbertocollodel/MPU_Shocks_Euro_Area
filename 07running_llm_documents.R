# Load necessary libraries and set parameters: -----


library(gemini.R)


setAPI("AIzaSyA1O-J8XK-Y5Ymr341izyQvsDlb2UkETp4")



# Write a prompt for LLM: -----

prompt=c("You are a bank following the press conferences of the ECB
Governing Council, where the decisions about the monetary policy stance is communicated
to the public, together with an assessment of the state of the economy that motivates
the decision and a Q&A with journalists.

You follow the conference to buy/sell Overnight 
Index Swap rates at different prices to manage your exposure to 
interest rate fluctuations.

From the conference you infer certain developments for key interest rates
over the course of the short (3 months-1 year), medium (2-5 years) and long-term (10 years) 
horizon and you buy/sell OIS instruments based on that.

We are in [date].

Using as input only these words and the information available to you as of [date],
how confused are you on the interest rate developments? 
Provide separate numerical scores from 0-100 for confusion regarding
the short horizon, medium horizon and long-term horizon.

Do not incorporate any data that was not available to you as of [] in your assessment.

Please, provide your assessment in a table form.

The first three columns will be a sequence of numerical values idicating your confusion on the three
distinct horizon: confusion_s, confusion_m, confusion_l

Then, three columns with a text of explanation, less than 30 words, for your assessment, on the three
distinct horizons: explanation_s,explanation_m, explanation_l

Then, three columns with a text on whether the confusion stems from the introductory statement, from the Q&A, or both equally, on
the tree distinct horizon: part_s,part_m, part_l

Finally, three columns with a different wording that could have made the message clearer, if you see potential for more
clarity: better_s,better_m,better_l")


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
  select(-1,-ncol(.)) %>% 
  filter(!str_detect(confusion_s,"--"))

writexl::write_xlsx(df,"../intermediate_data/llm_assessment.xlsx")




