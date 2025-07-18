# SCRIPT TO COMPARE DIFFERENT PROMPTS FOR THE SAME TASK


names=c("prompt_anchor", "prompt_history", "prompt_naive")


df_eval=list.files(path="../intermediate_data",
  pattern = "prompt",
full.name=T) |>
  str_subset(pattern = "batch",negate=T) |> 
  map(~ .x |> read_xlsx()) |> 
  set_names(names) |>
  bind_rows(.id="prompt")


#------------------------------------------------------------------------------
## 2. CALCULATE LLM UNCERTAINTY & JOIN WITH MARKET DATA
#------------------------------------------------------------------------------

# --- Calculate LLM uncertainty (std_rate) for each prompt ---
llm_uncertainty_df <- df_eval %>%
  group_by(prompt, date, tenor) %>%
  summarise(std_rate = sd(rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(date))

# --- Load market surprise data ---
market_surprise_df <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = if_else(tenor == "3mnt", "3M", tenor)) %>%
  select(tenor, date, correct_post_mean)

# --- Combine LLM uncertainty with market surprise ---
combined_df <- llm_uncertainty_df %>%
  inner_join(market_surprise_df, by = c("date", "tenor"))


#------------------------------------------------------------------------------
## 3. COMPUTE ROLLING CORRELATION FOR EACH PROMPT
#------------------------------------------------------------------------------

# --- Group by prompt and tenor, then calculate rolling correlation ---
rolling_corr_df <- combined_df %>%
  arrange(prompt, tenor, date) %>%
  group_by(prompt, tenor) %>%
  mutate(
    rolling_corr = rollapply(
      data = select(., std_rate, correct_post_mean),
      width = 12, # 12-month rolling window
      FUN = function(w) cor(w[, 1], w[, 2], method = "spearman", use = "pairwise.complete.obs"),
      by.column = FALSE,
      align = "right",
      fill = NA
    )
  ) %>%
  ungroup()


#------------------------------------------------------------------------------
## 4. CREATE AND SAVE THE COMPARISON PLOT
#------------------------------------------------------------------------------

# --- Define a distinct color palette for the prompts ---
prompt_colors <- c("prompt_anchor" = "#1b9e77", "prompt_history" = "#d95f02", "prompt_naive" = "#7570b3")

# --- Create the plot ---
plot_prompt_comparison <- ggplot(rolling_corr_df, aes(x = date, y = rolling_corr, color = prompt)) +
  # Add zero-reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  
  # Plot the correlation lines
  geom_line(linewidth = 1, alpha = 0.85) +
  
  # Facet by tenor to create separate panels for 10Y, 2Y, and 3M
  facet_wrap(~ tenor, nrow = 3) +
  
  # Apply the custom color scale
  scale_color_manual(values = prompt_colors) +
  
  # Format axes
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(limits = c(-0.6, 1.0), breaks = seq(-0.5, 1.0, 0.5)) +
  
  # Add informative labels
  labs(
    title = "Comparison of Prompt Performance Over Time",
    subtitle = "12-month rolling correlation between LLM uncertainty and market surprise, by prompt",
    x = NULL,
    y = "Rolling Spearman Correlation",
    color = "Prompt Strategy"
  ) +
  
  # Apply a clean, publication-ready theme
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    strip.text = element_text(face = "bold", size = 12)
  )

# --- Display and save the plot ---
print(plot_prompt_comparison)

ggsave(
  filename = "../output/figures/prompt_comparison_rolling_corr.png",
  plot = plot_prompt_comparison,
  dpi = 320, width = 10, height = 8, bg = "white"
)





