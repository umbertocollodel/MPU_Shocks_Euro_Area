# Combine and clean results: -------

# Names columns:

names_col=c("date","id","tenor","direction","rate","confidence")

# Re-create a name vector in case messed up some conferences in API request:

names_correct=list.files(path = "../intermediate_data/gemini_result/prompt_history_surprises/",
           full.names = T) %>% 
  str_subset("\\d") %>% 
  str_remove("\\.txt") %>% 
  str_extract("\\d{4}-\\d{2}-\\d{2}")

# From character string to tibble:

results <- list.files(path = "../intermediate_data/gemini_result/prompt_history_surprises/",
                      pattern = "\\d{4}-\\d{2}-\\d{2}",
                    full.names = T) %>% 
  map(~ readRDS(.x)) %>% 
  map(~ tryCatch(.x %>% 
                   readr::read_delim(delim = "|", trim_ws = TRUE, skip = 1) %>% 
                   select(-1,-ncol(.)) %>% 
                   slice(-nrow(.)),
      error = function(e){
        cat("Error")
      })
) 


# Clean further: 

clean_df=results %>% 
  keep(~ !is.null(.x) && any(!is.na(.x))) %>%
  map(~ .x %>% setNames(names_col)) %>%
  map(~ .x %>% slice(-1)) %>% 
  map(~ .x %>% mutate(date=as.character(date))) %>%
  map(~ .x |> mutate(confidence = as.numeric(confidence))) %>%
  map(~ .x %>% mutate_at(vars(contains("rate")),as.numeric)) %>% 
  bind_rows() %>% 
  filter(tenor %in% c("3M","2Y","10Y"))


# # Combine with governor name:
# 
# file_names=list.files("../intermediate_data/texts/introductory_statements/")
# 
# # Extract date and governor name
# governor_df <- data.frame(
#   date = str_extract(file_names, "\\d{4}-\\d{2}-\\d{2}"),
#   governor = str_extract(file_names, "(?<=_)\\w+\\s\\w+")
# ) %>% 
#   mutate(date = as.Date(date),
#          governor = ifelse(is.na(governor),"Jean-Claude Trichet",governor))
# 
# 
# # Join the governor name into your df
# clean_df <- clean_df %>%
#   left_join(governor_df, by = "date")



# Export: 

writexl::write_xlsx(clean_df,
                    paste0("../intermediate_data/llm_assessment_",
                           name_prompt_request,
                           Sys.Date(),
                           ".xlsx")
)

