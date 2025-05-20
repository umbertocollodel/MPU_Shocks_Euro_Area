# Combine and clean results: -------

# Names columns:

names_col=c("date","horizon","conf_score","reason","ai_version","conf_score_ai","diff")

# Re-create a name vector in case messed up some conferences in API request:

names_correct=list.files(path = "../intermediate_data/gemini_result/",
           full.names = T) %>% 
  str_subset("\\d") %>% 
  str_remove("\\.txt") %>% 
  str_extract("\\d{4}-\\d{2}-\\d{2}")

# From character string to tibble:

results <- list.files(path = "../intermediate_data/gemini_result/",
                      pattern = "batch",
                    full.names = T) %>% 
  map(~ readRDS(.x)) %>% 
  map(~ tryCatch(.x %>% 
                   readr::read_delim(delim = "|", trim_ws = TRUE, skip = 1) %>% 
                   select(-1) %>% 
                   slice(-nrow(.)),
      error = function(e){
        cat("Error")
      })
) 


# Clean further: 

clean_df=results %>% 
  keep(~ !is.null(.x) && any(!is.na(.x))) %>% 
  map(~ .x %>% mutate(Date=as.character(Date))) %>% 
  map(~ .x %>% mutate_at(vars(contains("score")),as.numeric)) %>% 
  bind_rows() %>% 
  select(-c(8:ncol(.))) %>%
  setNames(names_col) %>%
  filter(!is.na(conf_score))


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

writexl::write_xlsx(clean_df,"../intermediate_data/llm_assessment.xlsx")

