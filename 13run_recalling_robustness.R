# Simple Analysis: Run LLM on Post-June 2024 ECB Conferences
library(tidyverse)
library(readtext)
library(writexl)
library(crayon)

# Load your LLM functions
source("create_prompts.R")  # For prompt_naive

# ==============================================================================
# STEP 1: IDENTIFY POST-JUNE 2024 CONFERENCES
# ==============================================================================

get_new_conferences <- function() {
  
  cat("=== FINDING POST-JUNE 2024 CONFERENCES ===\n")
  
  conference_files <- list.files("../intermediate_data/texts/", 
                                pattern = "\\d{4}-\\d{2}-\\d{2}", full.names = TRUE)
  
  new_conferences <- tibble(
    file_path = conference_files,
    filename = basename(conference_files),
    date_str = str_extract(filename, "\\d{4}-\\d{2}-\\d{2}"),
    date = as.Date(date_str)
  ) %>%
    filter(date >= as.Date("2024-06-01")) %>%
    arrange(date)
  
  cat("Found", nrow(new_conferences), "conferences from June 2024 onwards:\n")
  print(new_conferences %>% select(date_str))
  
  return(new_conferences)
}

# ==============================================================================
# STEP 2: RUN LLM ON NEW CONFERENCES
# ==============================================================================

run_llm_simple <- function(conferences) {
  
  cat("\n=== RUNNING LLM ===\n")
  
  results <- list()
  
  for(i in seq_len(nrow(conferences))) {
    
    conf <- conferences[i, ]
    cat("Processing:", conf$date_str, "\n")
    
    # Read text
    text <- readtext(conf$file_path)$text
    
    # Create prompt
    full_prompt <- gsub("\\[date\\]", conf$date_str, prompt_naive)
    full_prompt <- paste0(full_prompt, 
                         "Press Conference on ", conf$date_str, "\n",
                         "Text:", text, "\n\n")
    
    # Call LLM
    result <- tryCatch({
      res <- new_gemini(full_prompt, seed = 120, temperature = 1)
      cat(crayon::green("✅ Success\n"))
      return(res)
    }, error = function(e) {
      cat(crayon::red("❌ Error:", e$message, "\n"))
      return(NULL)
    })
    
    results[[conf$date_str]] <- result
    
    # Brief pause
    if(i < nrow(conferences)) Sys.sleep(3)
  }
  
  return(results)
}

# ==============================================================================
# STEP 3: CLEAN RESULTS
# ==============================================================================

clean_results <- function(raw_results) {
  
  cat("\n=== CLEANING RESULTS ===\n")
  
  names_col <- c("date", "id", "tenor", "direction", "rate", "confidence")
  
  clean_list <- map(raw_results, function(result) {
    if(is.null(result)) return(NULL)
    
    tryCatch({
      result %>% 
        readr::read_delim(delim = "|", trim_ws = TRUE, skip = 1, show_col_types = FALSE) %>% 
        select(-1, -ncol(.)) %>%
        slice(-nrow(.)) %>%
        setNames(names_col) %>%
        slice(-1) %>%
        mutate(
          date = as.character(date),
          rate = as.numeric(rate),
          confidence = as.numeric(confidence)
        ) %>%
        filter(tenor %in% c("3M", "2Y", "10Y"))
    }, error = function(e) NULL)
  })
  
  clean_df <- clean_list %>% 
    keep(~ !is.null(.x)) %>%
    bind_rows()
  
  cat("Final dataset:", nrow(clean_df), "observations\n")
  cat("Conferences:", length(unique(clean_df$date)), "\n")
  
  return(clean_df)
}

# ==============================================================================
# STEP 4: PLOT LLM DISAGREEMENT ONLY
# ==============================================================================

plot_llm_results <- function(llm_data) {
  
  cat("\n=== CREATING PLOT ===\n")
  
  # Compute LLM disagreement
  llm_disagreement <- llm_data %>%
    mutate(date = as.Date(date)) %>%
    group_by(date, tenor) %>%
    summarise(
      llm_std = sd(rate, na.rm = TRUE),
      n_agents = n(),
      .groups = "drop"
    ) %>%
    mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) %>%
    arrange(date)
  
  # Show data
  cat("LLM disagreement results:\n")
  print(llm_disagreement)
  
  # Plot LLM disagreement (standard deviation)
  p <- ggplot(llm_disagreement, aes(x = date, y = llm_std, color = tenor)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    facet_wrap(~ tenor, scales = "free_y", ncol = 1) +
    scale_color_manual(values = c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(
      title = "LLM Disagreement: Post-June 2024 ECB Conferences",
      subtitle = "Standard deviation of rate predictions across 30 agents",
      x = "Date", 
      y = "Standard Deviation of Predicted Rates",
      color = "Tenor"
    ) +
    theme_minimal(base_family = "Segoe UI") +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey40"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold", size = 12)
    )
  
  # Save plot
  ggsave("../output/figures/post_june2024_disagreement.pdf", p, 
         width = 12, height = 10, dpi = 320, bg = "white")
  
  cat("Plot saved: ../output/figures/post_june2024_disagreement.pdf\n")
  
  return(list(plot = p, data = llm_disagreement))
}

# ==============================================================================
# MAIN FUNCTION
# ==============================================================================

run_simple_analysis <- function() {
  
  cat("=== SIMPLE POST-JUNE 2024 ANALYSIS ===\n\n")
  
  # Step 1
  conferences <- get_new_conferences()
  
  if(nrow(conferences) == 0) {
    cat("No conferences found\n")
    return(NULL)
  }
  
  # Step 2
  raw_results <- run_llm_simple(conferences)
  
  # Step 3  
  clean_data <- clean_results(raw_results)
  
  if(nrow(clean_data) == 0) {
    cat("No clean data produced\n")
    return(NULL)
  }
  
  # Save clean data
  writexl::write_xlsx(clean_data, "../output/post_june2024_llm_results.xlsx")
  cat("Clean data saved: ../output/post_june2024_llm_results.xlsx\n")
  
  # Step 4
  plots <- plot_llm_results(clean_data)
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Look at the plot to see if LLM tracks market volatility in post-June 2024 period\n")
  
  return(list(
    conferences = conferences,
    data = clean_data,
    plots = plots
  ))
}

# ==============================================================================
# EXECUTE
# ==============================================================================

cat("Ready to run simple analysis on post-June 2024 conferences\n")
cat("Execute: results <- run_simple_analysis()\n")

# Uncomment to run:
results <- run_simple_analysis()
