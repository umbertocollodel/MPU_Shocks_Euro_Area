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
  bind_rows() %>% 
  setNames(names_col) %>%
  select(-ncol(.)) %>%
  mutate_at(vars(contains("score")),as.numeric) %>% 
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


# Plot: ----

clean_df %>%
  select(date,horizon,conf_score,conf_score_ai) %>% 
  pivot_longer(cols = conf_score:ncol(.),
               names_to = "var") %>%
  mutate(agent = if_else(str_ends(var, "_ai"), "ai", "actual")) %>% 
  mutate(var = str_remove(var,"_ai")) %>% 
  filter(agent == "actual") %>%
  group_by(horizon) %>% 
  mutate(mean = mean(value,na.rm=T),
         sd = sd(value,na.rm = T)) %>% 
  mutate(date=as.Date(date)) %>% 
  ggplot(aes(date,value,col=horizon,group=horizon)) +
  geom_point(size=4,alpha=0.5) +
  geom_line(size=1) +
  geom_hline(aes(yintercept = mean),col="red", 
             linetype="dashed",
             size=1.5) +
  facet_wrap(~ horizon,
             scales = "free",
             nrow = 2) +
  theme_minimal() +
  labs(col="",
       x="",
       y="Confusion Score") +
  theme_bw() +
  theme(text=element_text(family="Segoe UI Light")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 18,angle = 90),
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


# Other plot: ----


# Step 1: Prepare the data
diff_df <- clean_df %>%
  mutate(date = as.Date(date)) %>%
  select(date, horizon, conf_score) %>%
  pivot_wider(names_from = horizon, values_from = conf_score) %>%
  rename(short = `Short-term`, long = `Long-term`) %>%
  mutate(abs_diff = abs(short - long))

# Step 2: Identify high-difference dates (95th percentile and above)
threshold <- quantile(diff_df$abs_diff, 0.95, na.rm = TRUE)
highlight_dates <- diff_df %>%
  filter(abs_diff >= threshold) %>%
  pull(date)

# Step 3: Prepare data for plotting
plot_df <- clean_df %>%
  mutate(date = as.Date(date)) %>%
  filter(horizon %in% c("Short-term", "Long-term")) %>%
  group_by(horizon) %>%
  mutate(mean = mean(conf_score, na.rm = TRUE)) %>%
  ungroup()


# Step 4: Plot
ggplot(plot_df, aes(x = date, y = conf_score, color = horizon)) +
  geom_vline(data = tibble(date = highlight_dates),
             aes(xintercept = date),
             color = "grey60", alpha = 0.6, size = 3) +
  geom_point(size = 3, alpha = 0.6) +
  geom_line(aes(group = horizon), size = 1) +
  geom_hline(aes(yintercept = mean), linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~ horizon, scales = "free_y", nrow = 2) +
  theme_minimal(base_family = "Segoe UI Light") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 16),
        legend.position = "bottom") +
  labs(x = "", y = "Confusion Score", color = "", title = "Confusion Scores with High Difference Highlights")

# Plot: ----

clean_df %>%
  pivot_longer(cols = conf_sr:conf_lr_ai,
               names_to = "var") %>%
  mutate(agent = if_else(str_ends(var, "_ai"), "ai", "actual")) %>% 
  mutate(var = str_remove(var,"_ai")) %>% 
  pivot_wider(names_from = "agent",values_from = "value") %>% 
  mutate(diff = actual - ai) %>% 
  ggplot(aes(date,diff,col=var)) +
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


