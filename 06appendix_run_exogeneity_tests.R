regression_results <- differences_df %>% 
  split(.$tenor) %>% 
  map(~ .x %>% mutate(`Lag(1)` = dplyr::lag(diff,1),
                      `Lag(2)` = dplyr::lag(diff,2),
                      `Lag(3)` = dplyr::lag(diff,3)
  )) %>% 
  map(~ lm(diff ~ `Lag(1)`+`Lag(2)` + `Lag(3)`,.x)) %>% 
  map(~ tidy(.x, conf.int = TRUE))

# Combine results into a single data frame
results_df <- bind_rows(regression_results, .id = "tenor")

# Plot the coefficients with confidence intervals

results_df %>% 
filter(term != "(Intercept)") %>% 
ggplot(aes(x = term, y = estimate, color = tenor)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.5), width = 0.3,linewidth=1) +
  geom_hline(yintercept = 0) +
  labs(title="",
       col="Tenor",
       y="",
       x="",
       fill="",
       caption = "") +
  theme_bw() +
  theme(text=element_text(family="Segoe UI Light")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.text = element_text(size=14),
         legend.title = element_text(size=16),
         # The new stuff
         strip.text = element_text(size = 20)) +
  theme(legend.position = "bottom") +
  theme(
    plot.title = element_text(
      size = 20,            # Set font size
      face = "bold", # Make the title bold and italic
      color = "black",
      family = "Segoe UI Light")) +
  theme(plot.caption = element_text(hjust = 0,size=12))

# Export:

ggsave("output/figures/autocorrelation_surprises.png",
       width = 4,
       height = 3,
       dpi="retina")
  