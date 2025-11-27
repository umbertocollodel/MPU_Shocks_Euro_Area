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

# Hedging words count: -----

# Define hedging word dictionary (common in economic/financial communication)
hedging_words <- c("may", "might", "could", "would", "should", "possibly",
                   "probably", "likely", "unlikely", "appear", "seem",
                   "suggest", "potential", "uncertainty", "uncertain",
                   "risk", "risks")

hedging_vector <- ecb_pressconf_final %>%
  map(~ corpus(.x)) %>%
  map(~ tokens(.x, remove_punct = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_select(pattern = hedging_words, selection = "keep") %>%
      lengths()) %>%
  unlist() %>%
  unname()


# =====================================================================
# Loughran-McDonald Uncertainty Density: -----
# =====================================================================

# Load Loughran-McDonald lexicon from textdata package
lm_lexicon <- textdata::lexicon_loughran() |> filter(sentiment == "uncertainty")
uncertainty_words <- tolower(lm_lexicon$word)

# Count uncertainty words
uncertainty_vector <- ecb_pressconf_final %>%
  map(~ corpus(.x)) %>%
  map(~ tokens(.x, remove_punct = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_select(pattern = uncertainty_words, selection = "keep") %>%
      lengths()) %>%
  unlist() %>%
  unname()

# Calculate density (normalized by word count)
uncertainty_density_vector <- uncertainty_vector / nw_vector *100


# =====================================================================
# Hawkish-Dovish Score: -----
# =====================================================================

# Define monetary policy word lists
hawkish_words <- c(
  "tightening", "tighten", "tight", "raise", "raised", "raising",
  "hikes", "hike", "hiking", "higher", "increase", "increased",
  "restrict", "restrictive", "inflation", "inflationary", "price",
  "vigilance", "vigilant", "monitor", "concerned", "risk",
  "caution", "cautious", "resolve", "firmly", "determined"
)

dovish_words <- c(
  "easing", "ease", "lower", "lowered", "cut", "cuts", "reduce",
  "accommodative", "accommodation", "flexible", "supportive",
  "growth", "employment", "strengthen", "improve", "recovery",
  "confidence", "benign", "moderate", "gradual", "patient"
)

# Calculate counts
hawkish_vector <- ecb_pressconf_final %>%
  map(~ corpus(.x)) %>%
  map(~ tokens(.x, remove_punct = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_select(pattern = hawkish_words, selection = "keep") %>%
      lengths()) %>%
  unlist() %>%
  unname()

dovish_vector <- ecb_pressconf_final %>%
  map(~ corpus(.x)) %>%
  map(~ tokens(.x, remove_punct = TRUE, remove_symbols = TRUE) %>%
      tokens_tolower() %>%
      tokens_select(pattern = dovish_words, selection = "keep") %>%
      lengths()) %>%
  unlist() %>%
  unname()

# Net score (hawkish - dovish)
hawkish_dovish_score_vector <- (hawkish_vector - dovish_vector)/nw_vector *100


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
  select(tenor, date, market_volatility = correct_post_mean_1) |>
  filter(tenor %in% c("3M", "2Y", "10Y"))

# Merge with Flesch-Kincaid scores, word count, and hedging words (whole text only)
# Extract whole text indices
whole_text_idx <- which(readability_df$part == "Whole text")

complexity_volatility_by_tenor <- readability_df %>%
  filter(part == "Whole text") %>%
  select(date, Flesch.Kincaid) %>%
  mutate(word_count = nw_vector[whole_text_idx],
         hedging_count = hedging_vector[whole_text_idx],
         uncertainty_density = uncertainty_density_vector[whole_text_idx],
         hawkish_dovish_score = hawkish_dovish_score_vector[whole_text_idx]) %>%
  inner_join(range_df, by = "date") %>%
  drop_na() %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

# Calculate correlations by tenor for all three variables
cor_by_tenor <- complexity_volatility_by_tenor %>%
  group_by(tenor) %>%
  summarise(
    # FK Complexity
    fk_spearman = cor(Flesch.Kincaid, market_volatility, method = "spearman"),
    fk_pval = cor.test(Flesch.Kincaid, market_volatility, method = "spearman")$p.value,
    # Word count
    wc_spearman = cor(word_count, market_volatility, method = "spearman"),
    wc_pval = cor.test(word_count, market_volatility, method = "spearman")$p.value,
    # Hedging count
    hedge_spearman = cor(hedging_count, market_volatility, method = "spearman"),
    hedge_pval = cor.test(hedging_count, market_volatility, method = "spearman")$p.value,
    # Uncertainty density
    unc_spearman = cor(uncertainty_density, market_volatility, method = "spearman"),
    unc_pval = cor.test(uncertainty_density, market_volatility, method = "spearman")$p.value,
    # Hawkish-Dovish score
    hd_spearman = cor(hawkish_dovish_score, market_volatility, method = "spearman"),
    hd_pval = cor.test(hawkish_dovish_score, market_volatility, method = "spearman")$p.value,
    n = n(),
    .groups = "drop"
  )

cat("\n=== Spearman Correlations: Text Variables vs Market-Based Disagreement by Tenor ===\n")
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

# Recalculate correlations without outliers for all three variables
cor_by_tenor_robust <- complexity_volatility_filtered %>%
  filter(!is_outlier) %>%
  group_by(tenor) %>%
  summarise(
    # FK Complexity
    fk_spearman = cor(Flesch.Kincaid, market_volatility, method = "spearman"),
    fk_pval = cor.test(Flesch.Kincaid, market_volatility, method = "spearman")$p.value,
    # Word count
    wc_spearman = cor(word_count, market_volatility, method = "spearman"),
    wc_pval = cor.test(word_count, market_volatility, method = "spearman")$p.value,
    # Hedging count
    hedge_spearman = cor(hedging_count, market_volatility, method = "spearman"),
    hedge_pval = cor.test(hedging_count, market_volatility, method = "spearman")$p.value,
    # Uncertainty density
    unc_spearman = cor(uncertainty_density, market_volatility, method = "spearman"),
    unc_pval = cor.test(uncertainty_density, market_volatility, method = "spearman")$p.value,
    # Hawkish-Dovish score
    hd_spearman = cor(hawkish_dovish_score, market_volatility, method = "spearman"),
    hd_pval = cor.test(hawkish_dovish_score, market_volatility, method = "spearman")$p.value,
    n = n(),
    .groups = "drop"
  )

cat("\n=== Robust Spearman Correlations (outliers removed) ===\n")
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

# =============================================================================
# Generate LaTeX correlation table using stargazer
# =============================================================================

# Function to add significance stars
add_stars <- function(pval) {
  case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE ~ ""
  )
}

# Prepare table data using robust correlations (outliers removed)
# Reshape data: rows = variables, columns = tenors
cor_table <- cor_by_tenor_robust %>%
  select(tenor, fk_spearman, fk_pval, wc_spearman, wc_pval,
         hedge_spearman, hedge_pval, unc_spearman, unc_pval, hd_spearman, hd_pval) %>%
  pivot_longer(cols = -tenor,
               names_to = c("variable", ".value"),
               names_pattern = "(.+)_(spearman|pval)") %>%
  mutate(variable = case_when(
    variable == "fk" ~ "FK Complexity",
    variable == "wc" ~ "Word Count",
    variable == "hedge" ~ "Hedging Words",
    variable == "unc" ~ "LM Uncertainty",
    variable == "hd" ~ "Net Hawkish-Dovish Score"
  )) %>%
  mutate(cor_with_sig = paste0(sprintf("%.3f", spearman), add_stars(pval))) %>%
  select(variable, tenor, cor_with_sig) %>%
  pivot_wider(names_from = tenor, values_from = cor_with_sig) %>%
  as.data.frame()

# Set row names for stargazer
rownames(cor_table) <- cor_table$variable
cor_table <- cor_table %>% select(-variable)

# Generate LaTeX table with stargazer
stargazer(cor_table,
          type = "latex",
          summary = FALSE,
          rownames = TRUE,
          title = "Spearman Correlations: Text Variables and Market-Based Disagreement by Tenor",
          label = "tab:complexity_correlations",
          notes = c("*** p$<$0.01, ** p$<$0.05, * p$<$0.10",
                    "Correlations calculated using Spearman rank correlation.",
                    "Outliers (above 99th percentile) removed from analysis."),
          notes.align = "l",
          out = "../output/tables/complexity_correlations.tex")

cat("\n=== LaTeX table saved to ../output/tables/complexity_correlations.tex ===\n")

  