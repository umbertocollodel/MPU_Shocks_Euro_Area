# Combine and clean results: -------

# Names columns:

names_col=c("date","conf_sr","conf_sr_ai","conf_mr","conf_mr_ai","conf_lr","conf_lr_ai")

# Re-create a name vector in case messed up some conferences in API request:

names_correct=list.files(path = "../intermediate_data/gemini_result/",
           full.names = T) %>% 
  str_subset("\\d") %>% 
  str_remove("\\.txt") %>% 
  str_extract("\\d{4}-\\d{2}-\\d{2}")

# From character string to tibble:

results <- list.files(path = "../intermediate_data/gemini_result/",
                    full.names = T) %>% 
  map(~ readRDS(.x)) %>% 
  map(~ tryCatch(.x %>% 
                   readr::read_delim(delim = "|", trim_ws = TRUE, skip = 2),
      error = function(e){
        cat("Error")
      })
) %>% 
  set_names(names_correct)


# Clean further: 

clean_df=results %>% 
  map(~ tryCatch(.x %>% 
                   slice(1) %>% 
                   select(-1) %>% 
                   rename_with(~ paste("Confusion Score", seq_along(.))) %>% 
                   select(-1,-ncol(.)) %>% 
                   mutate_all(as.numeric)
                 ,
                 error = function(e){
                   cat("Error")
                 })
  ) %>% 
  keep(~ !is.null(.x) && any(!is.na(.x))) %>% 
  bind_rows(.id = "date") %>% 
  mutate(date = as.Date(date)) %>% 
  setNames(names_col) %>% 
  select(1:7)



# Export: 

writexl::write_xlsx(clean_df,"../intermediate_data/llm_assessment.xlsx")


# Plot: ----

clean_df %>%
  pivot_longer(cols = conf_sr:ncol(.),
               names_to = "var") %>%
  mutate(agent = if_else(str_ends(var, "_ai"), "ai", "actual")) %>% 
  mutate(var = str_remove(var,"_ai")) %>% 
  filter(agent == "actual") %>% 
  ggplot(aes(date,value,col=var)) +
  geom_point(size=4,alpha=0.5) +
  geom_line(size=1) +
  facet_wrap(~ var) +
  theme_minimal() +
  labs(col="",
       x="",
       y="Confusion Score") +
  theme_bw() +
  theme(text=element_text(family="Segoe UI Light")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.text = element_text(size=14),
         # The new stuff
         strip.text = element_text(size = 20)) +
  theme(legend.position = "bottom") +
  theme(plot.caption = element_text(hjust = 0,size=12))


# Export:

ggsave("../output/figures/llm_confusion_scores_actual.png",
       width = 9,
       height = 3.5,
       dpi="retina")


