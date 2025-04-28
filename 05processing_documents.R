library(quanteda)
library(quanteda.textstats)
library(data.table)
library(readtext)
library(broom)



# Load data: ----

ecb_pressconf_final <- list.files("intermediate_data/texts/") %>% 
  map_chr(~ paste0("intermediate_data/texts/",.x)) %>% 
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
  left_join(main_events,by=c("year_mon")) %>% 
  mutate(governor = factor(governor, levels=c("Willem F. Duisenberg","Jean-Claude Trichet","Mario Draghi","Christine Lagarde"))) %>% 
  ggplot(aes(date,Flesch.Kincaid,col=governor)) +
  geom_point(aes(size=nw_vector), alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_vline(aes(xintercept=Date)) +
  scale_size(range = c(1, 8)) +
  labs(col="",
       size="Number of words",
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

ggsave("output/figures/readability_speeches.png",
       width = 6,
       height = 3.5,
       dpi="retina")

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

ggsave("output/figures/readability_speeches_parts.png",
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
         strip.text = element_text(size = 20)) +
  theme(plot.caption = element_markdown(hjust = 0,size=12))



# Regress formally ----


formulas=c("diff ~ complex_dummy",
           "diff ~ abs(monetary) + complex_dummy",
  "diff ~ abs(monetary) + complex_dummy + abs(monetary)*complex_dummy")

complexity_reg_df <- read_excel("../PEPP_effect/raw_data/00EA_MPD_update_january2024.xlsx",sheet = 4) %>%
  select(date, OIS_3M,OIS_6M,OIS_1Y,OIS_2Y,OIS_5Y,OIS_10Y) %>% 
  setNames(c("date","3mnt","6mnt","1Y","2Y","5Y","10Y")) %>%
  pivot_longer(`3mnt`:`10Y`,names_to = "tenor",values_to = "monetary") %>% 
  inner_join(differences_df %>% select(date,tenor,diff)) %>% 
  mutate(date = as.Date(date)) %>% 
  merge(complexity_df %>% bind_rows(.id = "part")) %>% 
  as_tibble() %>% 
  split(.$tenor) %>% 
  #map(~ .x %>% mutate(complex_dummy = ifelse(communication_type == "complex", 1, 0)))
  
  

# Regression shit that does not work: ------- 
  
complexity_reg_df %>%
  map(~ .x %>% 
         mutate(diff = scale(diff),
         monetary = scale(monetary))) %>% 
  map(~ .x %>% split(.x$part)) %>% 
  list_flatten() %>%
  map(~ .x %>% mutate(sign = case_when(diff > 0 ~ "positive",
                                       T ~ "negative"))) %>% 
  map(~ .x %>% split(.x$sign)) %>% 
  list_flatten() %>% 
  map( function(x) {
    map(formulas, ~ lm(.x, data = x) %>% summary() %>% tidy())
  }) %>% 
  list_flatten() %>% 
  bind_rows(.id = "name") %>% 
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    p.value < 0.05 ~ "*",
    TRUE ~ ""
  )) %>% 
  mutate(tenor = sapply(strsplit(as.character(name), "_"), `[`, 1)) %>% 
  split(.$tenor) %>% 
  map(~
ggplot(.x,aes(x = estimate, y = name, fill = term)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = significance), position = position_dodge(width = 0.9), hjust = -0.3) +
  labs(title = "Multiple Regression Analysis Results", x = "Estimate", y = "Name") +
  theme_minimal()
)



#Plot: correlate MP and MPU with complexity ------

complexity_reg_df %>% 
  map(~ .x %>% filter(part == "Whole text")) %>% 
  map(~ .x %>% 
        ggplot(aes(x = Flesch.Kincaid, y = abs(monetary))) +
  geom_point(color = 'blue') +
    geom_smooth(method = 'lm', color = 'red', se = FALSE) +
  labs(title = 'Scatter Plot of Flesch-Kincaid vs Monetary', x = 'Flesch-Kincaid', y = 'Monetary') +
  theme_minimal())
  

  
      
  


  
  
  
  