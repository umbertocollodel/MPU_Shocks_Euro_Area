# ============================================================================
# OUT-OF-SAMPLE EXPERIMENT: RE-RUN LLM FOR POST-JAN 2025 CONFERENCES
# ============================================================================

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(gemini.R)

setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# Set API key
setAPI(Sys.getenv("GEMINI_API_KEY"))

# ============================================================================
# 1. IDENTIFY POST-JAN 2025 CONFERENCES
# ============================================================================

cutoff_date <- as.Date("2025-01-01")

# Load conference files
conference_files <- list.files(
  "../intermediate_data/texts/", 
  pattern = "\\d{4}-\\d{2}-\\d{2}",
  full.names = TRUE
)

# Extract dates
all_dates <- str_extract(basename(conference_files), "\\d{4}-\\d{2}-\\d{2}") %>%
  as.Date()

# Filter for post-Jan 2025
test_files <- conference_files[all_dates >= cutoff_date]
test_dates <- all_dates[all_dates >= cutoff_date]

cat("\n=== TEST SAMPLE ===\n")
cat(sprintf("Conferences to simulate: %d (Jan-Dec 2025)\n", length(test_dates)))
cat("\nTest conference dates:\n")
print(sort(test_dates))

# ============================================================================
# 2. LOAD TRANSCRIPTS
# ============================================================================

transcripts <- tibble(
  date = test_dates,
  file_path = test_files
) %>%
  arrange(date) %>%
  mutate(
    transcript_text = map_chr(file_path, ~read_file(.x))
  )

cat(sprintf("\nLoaded %d transcripts\n", nrow(transcripts)))

# ============================================================================
# 3. LOAD PROMPT TEMPLATE
# ============================================================================

prompt_oos=prompt_naive

# ============================================================================
# 4. RUN LLM SIMULATION
# ============================================================================

results_list <- list()

for (i in 1:nrow(transcripts)) {
  conf_date <- transcripts$date[i]
  conf_text <- transcripts$transcript_text[i]
  
  cat(sprintf("\n[%d/%d] Processing: %s\n", 
              i, nrow(transcripts), conf_date))
  
  # Build prompt exactly like process_single_conference
  full_prompt <- gsub("\\[date\\]", conf_date, prompt_oos)
  full_prompt <- paste0(full_prompt, conf_text, "\n\n")
  
  # Call Gemini using gemini.R
  tryCatch({
    response <- new_gemini(
      prompt = full_prompt,
      model = "2.5-flash",
      temperature = 1,
      maxOutputTokens = 100000,
      topK = 40,
      topP = 0.95,
      seed = 120  # Use same seed as your other scripts
    )
    
    # Parse markdown table
    lines <- str_split(response, "\n")[[1]]
    table_lines <- lines[str_detect(lines, "\\|")]
    
    if (length(table_lines) > 2) {
      parsed_data <- table_lines[3:length(table_lines)] %>%
        map_dfr(~{
          parts <- str_split(.x, "\\|")[[1]] %>% str_trim()
          if (length(parts) >= 6) {
            tibble(
              date = parts[2],
              trader_id = parts[3],
              tenor = parts[4],
              direction = parts[5],
              rate = as.numeric(parts[6])
            )
          }
        }) %>%
        # Override with actual conference date
        mutate(date = as.character(conf_date))
      
      results_list[[i]] <- parsed_data
      cat(sprintf("✓ Parsed %d predictions\n", nrow(parsed_data)))
    } else {
      warning(sprintf("Failed to parse response for %s", conf_date))
    }
    
  }, error = function(e) {
    warning(sprintf("API call failed for %s: %s", conf_date, e$message))
  })
  
  # Rate limiting (5 seconds like your function)
  Sys.sleep(5)
}

# ============================================================================
# 5. COMBINE AND SAVE
# ============================================================================

test_results <- bind_rows(results_list) %>%
  mutate(
    date = as.Date(date),
    tenor = case_when(
      tenor == "3MNT" ~ "3M",
      TRUE ~ tenor
    )
  )

# Save raw results
output_file <- sprintf("../intermediate_data/aggregate_gemini_result/prompt_naive/oos_test_%s.xlsx",
                      Sys.Date())
write_xlsx(test_results, path = output_file)

cat("\n=== SIMULATION COMPLETE ===\n")
cat(sprintf("Results saved: %s\n", output_file))
cat(sprintf("Total predictions: %d\n", nrow(test_results)))
cat(sprintf("Conferences simulated: %d\n", n_distinct(test_results$date)))

# ============================================================================
# 6. COMPUTE DISAGREEMENT
# ============================================================================

test_disagreement <- test_results %>%
  group_by(date, tenor) %>%
  summarise(
    llm_std = sd(rate, na.rm = TRUE),
    llm_mean = mean(rate, na.rm = TRUE),
    n_agents = n(),
    .groups = "drop"
  )

cat("\n=== DISAGREEMENT METRICS ===\n")
print(test_disagreement)

# Save
write_xlsx(
  test_disagreement,
  path = "../intermediate_data/oos_test_disagreement.xlsx"
)

cat("\nDisagreement metrics saved.\n")

# ============================================================================
# 7. VISUALIZE DISAGREEMENT BY TENOR
# ============================================================================

library(showtext)

# Load font
if (!("Segoe UI" %in% font_families())) {
  if (file.exists("C:/Windows/Fonts/segoeui.ttf")) {
    font_add("Segoe UI", regular = "C:/Windows/Fonts/segoeui.ttf")
  }
}
showtext_auto()

# Color palette (matching your existing scripts)
color_palette_tenors <- c("10Y" = "#d73027", "2Y" = "#4575b4", "3M" = "#91bfdb")

# Create output directory
dir.create("../output/figures/oos_jan2025", showWarnings = FALSE, recursive = TRUE)

# Plot disagreement time series
p_disagreement <- test_disagreement %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) %>%
  ggplot(aes(x = date, y = llm_std, color = tenor)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~tenor, ncol = 1, scales = "free_y") +
  scale_color_manual(values = color_palette_tenors, guide = "none") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "LLM Disagreement: Out-of-Sample Period (Jan-Dec 2025)",
    x = NULL,
    y = "Standard Deviation of Predicted Rate (pp)"
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(colour = "grey80", fill = NA),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )

ggsave(
  "../output/figures/oos_jan2025/disagreement_by_tenor.png",
  plot = p_disagreement,
  width = 10,
  height = 8,
  dpi = 300
)

cat("\n✓ Plot saved: ../output/figures/oos_jan2025/disagreement_by_tenor.png\n")

# Summary stats by tenor
summary_stats <- test_disagreement %>%
  group_by(tenor) %>%
  summarise(
    mean_std = mean(llm_std, na.rm = TRUE),
    median_std = median(llm_std, na.rm = TRUE),
    min_std = min(llm_std, na.rm = TRUE),
    max_std = max(llm_std, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

cat("\n=== SUMMARY STATISTICS BY TENOR ===\n")
print(summary_stats, width = Inf)
