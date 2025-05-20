
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
             color = "grey60", alpha = 0.6, size = 1) +
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

# Other plot: ----


# Step 1: Prepare the data
diff_df <- clean_df %>%
  mutate(date = as.Date(date)) %>%
  select(date, horizon, conf_score) %>%
  filter(horizon == "Long-term")

# Step 2: Identify high and low difference dates
high_threshold <- quantile(diff_df$conf_score, 0.98, na.rm = TRUE)
low_threshold <- quantile(diff_df$conf_score, 0.02, na.rm = TRUE)

highlight_high_dates <- diff_df %>%
  filter(conf_score >= high_threshold) %>%
  pull(date)

highlight_low_dates <- diff_df %>%
  filter(conf_score <= low_threshold) %>%
  pull(date)

# Step 4: Plot with both high and low percentile lines
ggplot(diff_df, 
       aes(x = date, y = conf_score, color = horizon)) +
  geom_vline(data = tibble(date = highlight_high_dates),
             aes(xintercept = date),
             color = "grey60", alpha = 0.6, size = 1) +
  geom_vline(data = tibble(date = highlight_low_dates),
             aes(xintercept = date),
             color = "blue", alpha = 0.6, size = 1) +
  geom_point(size = 3, alpha = 0.6) +
  geom_line(aes(group = horizon), size = 1) +
  theme_minimal(base_family = "Segoe UI Light") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 16),
        legend.position = "bottom") +
  labs(x = "", y = "Confusion Score", color = "", 
       title = "Confusion Scores with High and Low Difference Highlights")

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


