######## Analyze the readability of ECB press conferences over time

# Load data: ----

ecb_pressconf_final <- list.files("../intermediate_data/texts/") %>% 
  map_chr(~ paste0("../intermediate_data/texts/",.x)) %>% 
  map(~ readtext(.x))

n_meetings = nrow(ecb_pressconf_final[[length(ecb_pressconf_final)]]) # take number of docs in q&a folder 
#for simplicity
  

# Calculate Flesch-Kincaid score: -----

readability_df <- ecb_pressconf_final %>% 
  map(~ corpus(.x)) %>% 
  map(~ textstat_readability(.x, measure = "Flesch.Kincaid")) %>% 
  bind_rows() %>% 
  select(document,Flesch.Kincaid) %>% 
  mutate(document= str_remove(document,"\\.txt")) %>% 
  separate(document, into = c("date", "governor"), sep = "_") %>% 
  mutate(date= as.Date(date)) %>% 
  mutate(part = c(rep("Whole text",n_meetings),
                  rep("Introductory Statement",n_meetings),
                  rep("Q&A",n_meetings)))

# Number of words: -----

nw_vector <- ecb_pressconf_final %>% 
  map(~ corpus(.x)) %>%
  map(~ ntoken(.x)) %>% 
  unlist() %>%
  unname()


# Plot complete text: ----


readability_df %>% 
  cbind(nw_vector) %>% 
  filter(part == "Whole text") %>% 
  mutate(year_mon = format(date, "%Y-%m")) %>% 
  #left_join(main_events,by=c("year_mon")) %>% 
  mutate(governor = factor(governor, levels=c("Willem F. Duisenberg","Jean-Claude Trichet","Mario Draghi","Christine Lagarde"))) %>% 
  ggplot(aes(date,Flesch.Kincaid,col=governor)) +
  geom_point(aes(size=nw_vector), alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_size(range = c(1, 8)) +
  labs(col="",
       size="Number of words",
       x="",
       y="Flesch Kincaid Complexity Score") +
  theme_bw() +
  theme(
    text = element_text(family = "Segoe UI Light"),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  ) +
  theme(legend.position = "bottom")

ggsave("../output/figures/readability_speeches.png",
  dpi = 320,
  width = 12,
  height = 9,
  bg = "white"
)

# Plot different parts of the press conferences: ----


readability_df %>% 
  cbind(nw_vector) %>% 
  filter(Flesch.Kincaid > 0) %>% # still some problems with some texts 
  mutate(governor = factor(governor, levels=c("Willem F. Duisenberg","Jean-Claude Trichet","Mario Draghi","Christine Lagarde"))) %>% 
  ggplot(aes(date,Flesch.Kincaid, col=governor)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ part) +
  labs(col="",
       x="",
       y="Flesch Kincaid Complexity Score") +
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

ggsave("../output/figures/readability_speeches_parts.png",
       width = 6,
       height = 3.5,
       dpi="retina")

# Divide into buckets: -----


# Calculate the quantiles for the Flesch-Kincaid Grade Level scores

quantiles <- readability_df %>%
  split(.$part) %>% 
  map(~ quantile(.x$Flesch.Kincaid, probs = c(1/3, 2/3)))

# Create the buckets

complexity_df <- readability_df %>%
  split(.$part) %>% 
  map2(quantiles, ~ .x %>% mutate(communication_type = case_when(
      Flesch.Kincaid <= .y[1] ~ "simple",
      Flesch.Kincaid > .y[2] ~ "complex",
      TRUE ~ "ignored")))
  


# Plot: -----

complexity_df$`Whole text` %>% 
  ggplot(aes(date,Flesch.Kincaid,col=communication_type)) +
  geom_point() +
  scale_color_grey() +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.text = element_text(size=14),
         # The new stuff
         strip.text = element_text(size = 20))



  

  
      
  


  
  
  
  