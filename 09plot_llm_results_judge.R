# --- Install and Load Packages -----
# Assuming these are already loaded from your previous scripts
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(data.table)

# --- Load and Prepare Data -----    
# Load the JSON file and clean the data structure
judge_df <- fromJSON("optimization_history_in_sample_training_20250731_124608.json")
judge_df <- as_tibble(judge_df) %>%
  split(.$iteration) %>%
  map(~ .x |> unnest(detailed_results_training)) |>
  bind_rows() %>%
  mutate(
    transcript_date = as.Date(transcript_date),
    iteration = as.factor(iteration),
    tenor = as.factor(tenor)
  )

# --- Data Preparation for All Plots ---
# Reshape the data for the boxplot
judge_df_long <- judge_df %>%
  select(transcript_date, tenor, iteration, predicted_sd, actual_volatility) %>%
  pivot_longer(
    cols = c(predicted_sd, actual_volatility),
    names_to = "series_type_raw",
    values_to = "value"
  ) %>%
  mutate(
    plot_category = case_when(
      series_type_raw == "predicted_sd" ~ paste("Prompt", iteration),
      series_type_raw == "actual_volatility" ~ "Actual Volatility",
      TRUE ~ NA_character_
    ),
    plot_category = factor(plot_category, levels = c(paste("Prompt", 1:5), "Actual Volatility")),
    series_type = factor(series_type_raw, levels = c("predicted_sd", "actual_volatility"),
                         labels = c("LLM Predicted SD", "Actual Volatility"))
  )

# Define custom colors
prompt_colors <- brewer.pal(n = 5, name = "Dark2")
names(prompt_colors) <- paste("Prompt", 1:5)
actual_vol_color <- "black"
all_plot_category_colors <- c(prompt_colors, "Actual Volatility" = actual_vol_color)

# --- Generate and Save Plot 2 (Boxplot) ---

plot2 <- ggplot(judge_df_long, aes(x = plot_category, y = value, fill = plot_category)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ tenor, scales = "free_y", ncol = length(unique(judge_df_long$tenor))) +
  labs(
    title = "Distribution of LLM Predicted vs. Actual Volatility by Tenor",
    y = "Standard Deviation of Rate",
    x = "Series / Prompt"
  ) +
  scale_fill_manual(values = all_plot_category_colors) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )

print(plot2)

# --- Save Plot 2 ---
ggsave(
  filename = "llm_response_boxplot.pdf",
  plot = plot2,
  width = 10,
  height = 6,
  dpi = 300
)

# --- Data Preparation for Plot 4 ---
plot_data_for_highlight <- judge_df %>%
  mutate(difference = abs(predicted_sd - actual_volatility)) %>%
  group_by(tenor, iteration) %>%
  mutate(
    p90_threshold = quantile(difference, 0.85, na.rm = TRUE),
    highlight = difference > p90_threshold
  ) %>%
  ungroup()

highlight_rects <- plot_data_for_highlight %>%
  mutate(group = rleid(highlight)) %>%
  group_by(tenor, iteration, group) %>%
  filter(highlight) %>%
  summarise(
    xmin = min(transcript_date),
    xmax = max(transcript_date),
    .groups = 'drop'
  )

line_colors <- c("LLM Predicted SD" = "#4575b4", "Actual Volatility" = "black")
highlight_color <- "#d73027"

# --- Generate and Save Plot 4 (Refined) ---

plot4_refined <- ggplot(plot_data_for_highlight, aes(x = transcript_date)) +
  geom_rect(
    data = highlight_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = highlight_color,
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(
    aes(y = predicted_sd, color = "LLM Predicted SD"),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = actual_volatility, color = "Actual Volatility"),
    linewidth = 0.8
  ) +
  facet_grid(iteration ~ tenor) +
  labs(
    title = "",
    subtitle = "",
    y = "Standard Deviation of Rate",
    x = NULL,
    color = "Series"
  ) +
  scale_color_manual(values = line_colors) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20, unit = "pt"),
    strip.text.x = element_text(face = "bold", size = 12, margin = margin(t = 5, b = 5)),
    strip.text.y = element_text(face = "bold", size = 12, angle = 0, margin = margin(l = 5, r = 5)),
    axis.title.y = element_text(face = "bold", size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA)
  )

print(plot4_refined)

# --- Save Plot 4 ---
ggsave(
  filename = "llm_predicted_vs_actual_volatility_refined.pdf",
  plot = plot4_refined,
  width = 12,
  height = 9,
  dpi = 300
)


# Out-of-sample data preparation

out_sample_clean_df <- read.csv("../intermediate_data/aggregate_gemini_result/judge_llm/out_of_sample_results.csv") |> 
  as_tibble() |> 
  mutate(detailed_results_test = detailed_results_test |>
    # Replace single quotes with double quotes
    stringr::str_replace_all("'", "\"") |>
    # **NEW STEP:** Replace 'nan' with 'null' for valid JSON
    stringr::str_replace_all("nan", "null") |>
    lapply(\(x) {
      if (x == "[]") {
        data.frame()
      } else {
        fromJSON(x)
      }
    })) |>
  unnest(detailed_results_test, keep_empty = TRUE) |> 
    mutate(
    transcript_date = as.Date(transcript_date),
    iteration = as.factor(iteration +1),
    tenor = as.factor(tenor)
  )

# --- 3. Data Preparation for Plot ---
plot_data_for_highlight <- out_sample_clean_df %>%
  mutate(difference = abs(predicted_sd - actual_volatility)) %>%
  group_by(tenor, iteration) %>%
  mutate(
    p90_threshold = quantile(difference, 0.8, na.rm = TRUE),
    highlight = difference > p90_threshold
  ) %>%
  ungroup()

highlight_rects <- plot_data_for_highlight %>%
  mutate(group = rleid(highlight)) %>%
  group_by(tenor, iteration, group) %>%
  filter(highlight) %>%
  summarise(
    xmin = min(transcript_date),
    xmax = max(transcript_date),
    .groups = 'drop'
  )

line_colors <- c("LLM Predicted SD" = "#4575b4", "Actual Volatility" = "black")
highlight_color <- "#d73027"

# --- 4. Generate and Save Plot (Refined) ---
plot4_refined <- ggplot(plot_data_for_highlight |> filter(!is.na(tenor)), aes(x = transcript_date)) +
  geom_rect(
    data = highlight_rects,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = highlight_color,
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  geom_line(
    aes(y = predicted_sd, color = "LLM Predicted SD"),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = actual_volatility, color = "Actual Volatility"),
    linewidth = 0.8
  ) +
  facet_grid(iteration ~ tenor) +
  labs(
    title = "",
    subtitle = "",
    y = "Standard Deviation of Rate",
    x = NULL,
    color = "Series"
  ) +
  scale_color_manual(values = line_colors) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20, unit = "pt"),
    strip.text.x = element_text(face = "bold", size = 12, margin = margin(t = 5, b = 5)),
    strip.text.y = element_text(face = "bold", size = 12, angle = 0, margin = margin(l = 5, r = 5)),
    axis.title.y = element_text(face = "bold", size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA)
  )

print(plot4_refined)

# --- Save Plot 4 ---
ggsave(
  filename = "llm_predicted_vs_actual_volatility_refined_out_of_sample.pdf",
  plot = plot4_refined,
  width = 12,
  height = 9,
  dpi = 300
)


# Plot: correlation in-sample and out-of-sample -------


df_cor_comparison <- out_sample_clean_df |> 
  select(iteration,correlation_test) |> 
  inner_join(judge_df |> 
    select(iteration,correlation_training),
   by = c("iteration")) |> 
  pivot_longer(cols = c(correlation_test, correlation_training),
               names_to = "set", values_to = "correlation") %>%
  mutate(set = recode(set, 
                      correlation_test = "Test", 
                      correlation_training = "Training")) |>
  mutate(correlation = correlation + 0.1) |> 
  mutate(set = factor(set, levels = c("Training", "Test"))) |>
  distinct()

# Define accessible colors
line_colors <- c("Test"="#d73027", "Training" = "#4575b4")  # Red & Blue

# Plot with fully readable text
plot_judge_corr <- ggplot(df_cor_comparison, aes(x = iteration, y = correlation)) +
  geom_point(aes(color = set), size = 4.5) +                  # bigger points
  geom_line(aes(color = set, group = set), linewidth = 1.3) +
  geom_text(aes(label = round(correlation, 2), color = set), 
            vjust = -1.2, size = 6.5, check_overlap = TRUE, show.legend = FALSE) +  # bigger labels
  facet_wrap(~set) +
  scale_color_manual(values = line_colors) +
  scale_y_continuous(limits = c(0,1), expand = expansion(mult = c(0.05, 0.15))) +
  labs(
    title = "",
    y = "Spearman Correlation",
    x = "",
    color = "Set"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "none",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(size = 13, color = "grey30", margin = margin(b = 20)),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 20, unit = "pt"),
    strip.text.x = element_text(face = "bold", size = 12, margin = margin(t = 5, b = 5)),
    strip.text.y = element_text(face = "bold", size = 12, angle = 0, margin = margin(l = 5, r = 5)),
    axis.title.y = element_text(face = "bold", size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "grey80", fill = NA)
  )

print(plot_judge_corr)


# Save the plot
ggsave(
  filename = "../output/figures/prompt_llm_as_judge/train_test_correlaton.pdf",
  plot = plot_judge_corr,
  width = 10,
  height = 6,
  dpi = 300
)


# Table: evolution of prompts -------


install.packages(c("kableExtra"))
library(kableExtra)

df_table_prompt_comparison <- out_sample_clean_df |> 
  select(iteration,correlation_test) |> 
  inner_join(judge_df |> 
    select(iteration,correlation_training,prompt_before_judge),
   by = c("iteration")) |> 
  distinct() |> 
  mutate(
    correlation_test = round(correlation_test, 2),
    correlation_training = round(correlation_training, 2)) %>%
  select(iteration, correlation_test, correlation_training, prompt_before_judge)


# Export to LaTeX longtable
df_table_prompt_comparison %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,         # Allow table to span multiple pages
    caption = "Correlation by prompt iteration",
    label = "tab:prompt_corr",
    col.names = c("Iteration", "Test Correlation", "Training Correlation", "Prompt")
  ) %>%
  kable_styling(
    latex_options = c("repeat_header"),  # Repeat header on each page
    font_size = 10
  ) %>%
  column_spec(4, width = "8cm")  |>
  save_kable("../output/tables/judge_llm_prompts.tex")

