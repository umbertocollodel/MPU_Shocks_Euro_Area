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

# =============================================================================
# NEW: Market-based uncertainty vs Flesch-Kincaid complexity
# =============================================================================

# Load market-based disagreement data
range_df <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date, market_volatility = correct_post_mean_3) |> 
  filter(tenor %in% c("3M", "2Y", "10Y")) |> 

# Merge with Flesch-Kincaid scores (whole text only)
complexity_volatility_by_tenor <- readability_df %>%
  filter(part == "Whole text") %>%
  select(date, Flesch.Kincaid) %>%
  inner_join(range_df, by = "date") %>%
  drop_na() %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

# Calculate correlations by tenor
cor_by_tenor <- complexity_volatility_by_tenor %>%
  group_by(tenor) %>%
  summarise(
    pearson = cor(Flesch.Kincaid, market_volatility, method = "pearson"),
    spearman = cor(Flesch.Kincaid, market_volatility, method = "spearman"),
    n = n(),
    .groups = "drop"
  )

cat("\n=== Correlation: FK Complexity vs Market-Based Disagreement by Tenor ===\n")
print(cor_by_tenor)

# Identify and handle outliers
# Calculate outliers by tenor (values beyond 99th percentile)
outlier_threshold <- complexity_volatility_by_tenor %>%
  group_by(tenor) %>%
  summarise(p99 = quantile(market_volatility, 0.99, na.rm = TRUE), .groups = "drop")

# Flag outliers for potential removal or annotation
complexity_volatility_filtered <- complexity_volatility_by_tenor %>%
  left_join(outlier_threshold, by = "tenor") %>%
  mutate(is_outlier = market_volatility > p99)

# Print outlier information
cat("\n=== Outlier Summary ===\n")
outlier_summary <- complexity_volatility_filtered %>%
  filter(is_outlier) %>%
  select(date, tenor, Flesch.Kincaid, market_volatility) %>%
  arrange(desc(market_volatility))

print(outlier_summary)

# Recalculate correlations without outliers
cor_by_tenor_robust <- complexity_volatility_filtered %>%
  filter(!is_outlier) %>%
  group_by(tenor) %>%
  summarise(
    pearson = cor(Flesch.Kincaid, market_volatility, method = "pearson"),
    spearman = cor(Flesch.Kincaid, market_volatility, method = "spearman"),
    n = n(),
    .groups = "drop"
  )

cat("\n=== Robust Correlations (outliers removed) ===\n")
print(cor_by_tenor_robust)

# Create scatter plot by tenor - with outliers removed
complexity_volatility_filtered %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = Flesch.Kincaid, y = market_volatility * 100)) +
  geom_point(size = 2.5, alpha = 0.5, color = "#4575b4") +
  # Use LOESS smooth to show non-linear/rank-based relationship
  geom_smooth(method = "loess", se = TRUE, color = "#d73027", fill = "#d73027", 
              alpha = 0.15, linewidth = 1, span = 0.75) +
  # Add correlation text in each facet
  geom_text(data = cor_by_tenor_robust %>% 
              mutate(label = glue("ρ = {round(spearman, 2)}")),
            aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.5, size = 6, 
            color = "grey30", family = "Segoe UI Light") +
  facet_wrap(~ tenor, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(10, 16, by = 1)) +
  labs(
    title = NULL,
    x = "Flesch-Kincaid Complexity Score",
    y = "Market-Based Disagreement (bps)",
    caption = ""
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.position = "top", # Consistent legend position
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey50"), # Added back caption if needed
    axis.title = element_text(size=18), # Added margin for y-axis title
    axis.text.x = element_text(vjust = 0.5, size = 16), # Match angle, vjust, size
    axis.text.y = element_text(size = 16), # Match size
    panel.border = element_rect(colour = "grey80", fill = NA), # Added for consistency
    strip.text = element_text(face = "bold", size = 14) # Match size
  )
  

ggsave("../output/figures/complexity_vs_market_disagreement_by_tenor.pdf",
       dpi = 320,
       width = 12,
       height = 5,
       bg = "white") 
  


  
  
  
  