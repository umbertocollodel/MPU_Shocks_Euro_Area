# Script to compare different prompt specifications
library(RColorBrewer)


# Prepare complete df: ---------


name_prompts=c("anchor","microstructure","naive")


prompts_df <- list.files("../intermediate_data/",full.names = T) %>%
  str_subset("xlsx") %>% 
  str_subset("3batch") %>% 
  map(~ .x %>% read_xlsx()) %>% 
  set_names(name_prompts) %>% 
  bind_rows(.id = "prompt_name") %>% 
  select(-reason)
  



# Figure: compare mistakes for E(x) across prompts and time: -----


mean_llm_df <- prompts_df %>% 
  group_by(date,tenor,prompt_name) %>% 
  mutate(date = as.Date(date)) %>%
  summarise(rate = mean(rate,na.rm=T))
  


actual_ois_df <- read_xlsx("../raw_data/ois_daily_data.xlsx",skip=1) %>%
  select(1,2,4,6) %>% 
  setNames(c("date","3M","2Y","10Y")) %>% 
  mutate(date = as.Date(date)) %>% 
  pivot_longer(`3M`:`10Y`,names_to = "tenor",values_to = "actual_rate")



# Join and compute error
joined_df <- merge(mean_llm_df, actual_ois_df,by=c("date","tenor")) %>%
  as_tibble() %>% 
  mutate(error = actual_rate - rate)

# Plot error by tenor over time
ggplot(joined_df, aes(x = date, y = error, color = prompt_name)) +
  geom_line(size=1.2) +
  geom_point(size=2,alpha=0.6) +
  geom_hline(yintercept = 0) + 
  facet_wrap(~ tenor, nrow=3) +
  labs(title = "Error by Tenor Over Time",
       x = "Date",
       y = "Error (Actual - LLM Mean Rate)",
       color = "") +
  theme_minimal(base_family = "Segoe UI") +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 24),
         axis.title = element_text( size = 20, face = "bold" ),
         legend.text = element_text(size=18),
         # The new stuff
         strip.text = element_text(size = 24)) +
  theme(legend.position = "bottom") +
  theme(plot.caption = element_text(hjust = 0,size=26)) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) 


# Save error over time plot
ggsave(
  filename = "../output/figures/error_over_time_by_prompt.png",
  plot = last_plot(),  # or assign your ggplot to a variable and use it here
  dpi = 320,
  width = 10,
  height = 8,
  bg = "white"
)




# Figure: compare median error by prompt and tenor (overall)


median_error_df <- joined_df %>% 
  group_by(tenor,prompt_name) %>% 
  summarise(median_error = mean(error,na.rm = T)) %>% 
  arrange(median_error) %>%
  mutate(prompt_name = fct_reorder(prompt_name, median_error, mean)) %>% 
  mutate(tenor = factor(tenor,levels=c("3M","2Y","10Y")))
  

# Plot
median_error_df %>% 
ggplot(aes(x = tenor, y = median_error*100, fill = prompt_name)) +
  geom_col(position="dodge",
           width=0.5,
           col="white") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "",
       x = "",
       y = "Median Error (Bps)",
       fill = "") +
  theme_minimal(base_family = "Segoe UI") +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 24),
         axis.title = element_text( size = 20, face = "bold" ),
         legend.text = element_text(size=18),
         # The new stuff
         strip.text = element_text(size = 24)) +
  theme(legend.position = "bottom") +
  theme(plot.caption = element_text(hjust = 0,size=26))




# Save median error plot
ggsave(
  filename = "../output/figures/median_error_by_prompt.png",
  plot = last_plot(), # or use your plot variable
  dpi = 320,
  width = 8,
  height = 6,
  bg = "white"
)


# Best forecaster -----


# Join forecasts with actuals
joined_df_best <- merge(prompts_df %>% mutate(date=as.Date(date)),
                   actual_ois_df,by=c("date","tenor")) %>%
  as_tibble() %>% 
  mutate(abs_error = abs(actual_rate - rate))

# Select best forecast per date and tenor (by id)
best_forecasts_df <- joined_df_best %>%
  group_by(date, tenor) %>%
  slice_min(abs_error, with_ties = FALSE) %>%
  ungroup() %>% 
  mutate(error = actual_rate - rate)

# Plot 1: Error over time by tenor
ggplot(best_forecasts_df, aes(x = date, y = error*100, color = prompt_name)) +
  geom_line(size = 1.2) +
  geom_point(size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ tenor, nrow = 3) +
  labs(title = "Error by Tenor Over Time (Best ID at Each Date)",
       x = "",
       y = "Error (Actual - Forecast Rate)",
       color = "Prompt Name") +
  theme_minimal()


mean_error_df <- best_forecasts_df%>% 
  group_by(tenor,prompt_name) %>% 
  summarise(mean_error = mean(error,na.rm = T)) %>% 
  arrange(mean_error) %>%
  mutate(prompt_name = fct_reorder(prompt_name, mean_error, mean)) %>% 
  mutate(tenor = factor(tenor,levels=c("3M","2Y","10Y")))


# Plot
mean_error_df %>% 
  ggplot(aes(x = tenor, y = mean_error*100, fill = prompt_name)) +
  geom_col(position="dodge",
           width=0.5,
           col="white") +
  scale_fill_brewer(palette = "Set2") +
  ylim(-50,0) +
  labs(title = "",
       x = "",
       y = "Median Error (Bps)",
       fill = "Prompt Name") +
  theme_minimal() +
  theme_minimal(base_family = "Segoe UI") +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 24),
         axis.title = element_text( size = 20, face = "bold" ),
         legend.text = element_text(size=18),
         # The new stuff
         strip.text = element_text(size = 24)) +
  theme(legend.position = "bottom") +
  theme(plot.caption = element_text(hjust = 0,size=26))


# Save median error plot
ggsave(
  filename = "../output/figures/median_error_by_prompt_best.png",
  plot = last_plot(), # or use your plot variable
  dpi = 320,
  width = 8,
  height = 6,
  bg = "white"
)



