# ============================================================================
# KNOWLEDGE CUTOFF ANALYSIS - SEQUENTIAL VERSION
# Re-runs LLM for symmetric window around Jan 31, 2025 cutoff
# ============================================================================

# Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  readxl,
  writexl,
  httr2,
  crayon,
  boot
)

# Import functions only (no execution)
#source("functions_llm.R") STILL TO DO!!
source("create_prompts.R")

# Set API key
setAPI(Sys.getenv("GEMINI_API_KEY"))

# ============================================================================
# STEP 1: Identify conferences for symmetric comparison
# ============================================================================

cat(crayon::blue("\n=== STEP 1: Identifying conferences ===\n\n"))

# Define knowledge cutoff
knowledge_cutoff <- as.Date("2025-01-31")

# Get all available conference dates
all_conference_files <- list.files("../intermediate_data/texts/") %>%
  str_subset("\\d{4}-\\d{2}-\\d{2}")

all_conference_dates <- all_conference_files %>%
  str_extract("\\d{4}-\\d{2}-\\d{2}") %>%
  as.Date() %>%
  unique() %>%
  sort()

# Split by cutoff
pre_cutoff_all <- all_conference_dates[all_conference_dates <= knowledge_cutoff]
post_cutoff_all <- all_conference_dates[all_conference_dates > knowledge_cutoff]

cat("Available conferences:\n")
cat("  Before cutoff:", length(pre_cutoff_all), "\n")
cat("  After cutoff:", length(post_cutoff_all), "\n\n")

# Determine symmetric window size
n_conferences <- min(length(post_cutoff_all), 6)

cat("Using symmetric window of", n_conferences, "conferences\n\n")

# Select conferences
pre_cutoff_select <- tail(pre_cutoff_all, n_conferences)
post_cutoff_select <- head(post_cutoff_all, n_conferences)

cat("SELECTED FOR ANALYSIS:\n")
cat("Pre-cutoff (last", n_conferences, "):\n")
cat(paste("  ", format(pre_cutoff_select, "%Y-%m-%d"), collapse = "\n"), "\n\n")
cat("Post-cutoff (first", n_conferences, "):\n")
cat(paste("  ", format(post_cutoff_select, "%Y-%m-%d"), collapse = "\n"), "\n\n")

# Combine
cutoff_conferences <- c(pre_cutoff_select, post_cutoff_select)
cutoff_labels <- c(
  rep("pre_cutoff", length(pre_cutoff_select)),
  rep("post_cutoff", length(post_cutoff_select))
)

# Create setup dataframe
cutoff_analysis_setup <- data.frame(
  date = cutoff_conferences,
  group = cutoff_labels,
  stringsAsFactors = FALSE
)

# Create output directories
dir.create("../intermediate_data/gemini_result_cutoff_analysis", showWarnings = FALSE)
dir.create("../intermediate_data/gemini_result_cutoff_analysis/pre_cutoff", showWarnings = FALSE)
dir.create("../intermediate_data/gemini_result_cutoff_analysis/post_cutoff", showWarnings = FALSE)

# Save setup
write_csv(
  cutoff_analysis_setup,
  "../intermediate_data/cutoff_analysis_setup.csv"
)

# ============================================================================
# STEP 2: Load conference texts
# ============================================================================

cat(crayon::blue("=== STEP 2: Loading conference texts ===\n\n"))

# Function to load text
load_conference_text <- function(conf_date) {
  
  matching_files <- list.files("../intermediate_data/texts/", full.names = TRUE) %>%
    str_subset(as.character(conf_date))
  
  if (length(matching_files) == 0) {
    cat(crayon::red("WARNING: No file found for", as.character(conf_date), "\n"))
    return(NULL)
  }
  
  if (length(matching_files) > 1) {
    cat(crayon::yellow("Multiple files for", as.character(conf_date), "- using first\n"))
  }
  
  text <- readLines(matching_files[1], warn = FALSE) %>%
    paste(collapse = " ")
  
  return(text)
}

# Load all texts
cutoff_texts <- map(cutoff_conferences, load_conference_text)
names(cutoff_texts) <- as.character(cutoff_conferences)

# Check for missing
missing_idx <- which(sapply(cutoff_texts, is.null))
if (length(missing_idx) > 0) {
  cat(crayon::red("\nWARNING: Missing texts for:\n"))
  print(cutoff_conferences[missing_idx])
  
  cutoff_analysis_setup <- cutoff_analysis_setup %>%
    filter(!date %in% cutoff_conferences[missing_idx])
  
  cutoff_texts <- cutoff_texts[!sapply(cutoff_texts, is.null)]
  cutoff_conferences <- cutoff_analysis_setup$date
  cutoff_labels <- cutoff_analysis_setup$group
}

cat(crayon::green("\nReady to run:", length(cutoff_conferences), "conferences\n\n"))

# ============================================================================
# STEP 3: Run LLM simulations SEQUENTIALLY
# ============================================================================

cat(crayon::blue("=== STEP 3: Running LLM simulations (SEQUENTIAL) ===\n\n"))

# Number of runs per conference for robustness
n_runs_per_conference <- 5

# Initialize tracking
total_jobs <- length(cutoff_conferences) * n_runs_per_conference
job_counter <- 0
start_time <- Sys.time()

# Initialize log
log_file <- "../intermediate_data/cutoff_analysis_log.txt"
if (file.exists(log_file)) file.remove(log_file)

cat("Total jobs:", total_jobs, "\n")
cat("(", length(cutoff_conferences), "conferences ×", n_runs_per_conference, "runs )\n\n")

# Sequential loop through conferences
for (i in seq_along(cutoff_conferences)) {
  
  conf_date <- cutoff_conferences[i]
  conf_text <- cutoff_texts[[as.character(conf_date)]]
  group_label <- cutoff_labels[i]
  
  cat(crayon::bold(sprintf(
    "\n>>> Conference %d/%d: %s (%s) <<<\n",
    i, length(cutoff_conferences),
    conf_date, group_label
  )))
  
  # Run multiple times for this conference
  for (run_num in 1:n_runs_per_conference) {
    
    job_counter <- job_counter + 1
    
    cat(crayon::yellow(sprintf(
      "  Run %d/%d (Job %d/%d)...",
      run_num, n_runs_per_conference,
      job_counter, total_jobs
    )))
    
    # Construct prompt
    full_prompt <- gsub("\\[date\\]", as.character(conf_date), prompt_naive)
    full_prompt <- paste0(
      full_prompt,
      "Press Conference on ", conf_date, "\n",
      "Text: ", conf_text, "\n\n"
    )
    
    # Try to get result
    success <- tryCatch({
      
      # Rate limiting
      Sys.sleep(5)
      
      # Call API
      res <- new_gemini(
        full_prompt,
        seed = 120 + run_num,
        temperature = 1
      )
      
      # Save immediately
      output_dir <- paste0(
        "../intermediate_data/gemini_result_cutoff_analysis/",
        group_label
      )
      
      output_file <- paste0(
        output_dir, "/",
        conf_date, "_run", run_num, ".rds"
      )
      
      saveRDS(res, file = output_file)
      
      cat(crayon::green(" ✓ Saved\n"))
      
      # Log success
      write(
        paste(Sys.time(), "SUCCESS", conf_date, group_label, run_num),
        file = log_file,
        append = TRUE
      )
      
      return(TRUE)
      
    }, error = function(e) {
      
      cat(crayon::red(sprintf(" ✗ Error: %s\n", e$message)))
      
      # Log failure
      write(
        paste(Sys.time(), "FAILED", conf_date, group_label, run_num, e$message),
        file = log_file,
        append = TRUE
      )
      
      return(FALSE)
      
    })
    
  }
  
  # Progress update
  elapsed <- as.numeric(Sys.time() - start_time, units = "mins")
  remaining <- (elapsed / job_counter) * (total_jobs - job_counter)
  
  cat(sprintf(
    "  Progress: %d/%d conferences | Elapsed: %.1f min | Est. remaining: %.1f min\n",
    i, length(cutoff_conferences),
    elapsed, remaining
  ))
  
}

end_time <- Sys.time()

cat(crayon::green(crayon::bold("\n=== SIMULATION COMPLETE ===\n")))
cat("Total time:", round(as.numeric(end_time - start_time, units = "mins"), 1), "minutes\n\n")

# ============================================================================
# STEP 4: Parse and clean results
# ============================================================================

cat(crayon::blue("=== STEP 4: Parsing results ===\n\n"))

# Column names
names_col <- c("date", "id", "tenor", "direction", "rate", "confidence")

# Function to parse
parse_cutoff_result <- function(file_path) {
  
  result <- tryCatch({
    
    raw <- readRDS(file_path)
    
    df <- readr::read_delim(raw, delim = "|", trim_ws = TRUE, skip = 1) %>%
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
    
    return(df)
    
  }, error = function(e) {
    cat(crayon::red("Error parsing", basename(file_path), ":", e$message, "\n"))
    return(NULL)
  })
  
  return(result)
}

# Parse pre-cutoff
cat("Parsing pre-cutoff results...\n")
pre_cutoff_files <- list.files(
  "../intermediate_data/gemini_result_cutoff_analysis/pre_cutoff",
  pattern = "\\.rds$",
  full.names = TRUE
)

pre_cutoff_results <- map_dfr(pre_cutoff_files, function(f) {
  parsed <- parse_cutoff_result(f)
  if (!is.null(parsed)) {
    run_num <- str_extract(f, "run\\d+") %>% str_extract("\\d+") %>% as.integer()
    parsed$run_id <- run_num
    parsed$group <- "pre_cutoff"
  }
  return(parsed)
})

# Parse post-cutoff
cat("Parsing post-cutoff results...\n")
post_cutoff_files <- list.files(
  "../intermediate_data/gemini_result_cutoff_analysis/post_cutoff",
  pattern = "\\.rds$",
  full.names = TRUE
)

post_cutoff_results <- map_dfr(post_cutoff_files, function(f) {
  parsed <- parse_cutoff_result(f)
  if (!is.null(parsed)) {
    run_num <- str_extract(f, "run\\d+") %>% str_extract("\\d+") %>% as.integer()
    parsed$run_id <- run_num
    parsed$group <- "post_cutoff"
  }
  return(parsed)
})

# Combine
cutoff_clean_results <- bind_rows(pre_cutoff_results, post_cutoff_results) %>%
  mutate(
    date = as.Date(date),
    tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))
  )

cat("\nParsing summary:\n")
cat("  Pre-cutoff rows:", nrow(pre_cutoff_results), "\n")
cat("  Post-cutoff rows:", nrow(post_cutoff_results), "\n")
cat("  Total rows:", nrow(cutoff_clean_results), "\n\n")

# Check completeness
completeness <- cutoff_clean_results %>%
  group_by(group, date, tenor) %>%
  summarise(n_runs = n_distinct(run_id), .groups = "drop")

incomplete <- completeness %>%
  filter(n_runs < n_runs_per_conference)

if (nrow(incomplete) > 0) {
  cat(crayon::yellow("WARNING: Incomplete runs for:\n"))
  print(incomplete)
}

# Save
write_xlsx(
  cutoff_clean_results,
  "../intermediate_data/cutoff_analysis_clean_results.xlsx"
)

cat(crayon::green("Cleaned results saved!\n\n"))

# ============================================================================
# STEP 5: Compute disagreement
# ============================================================================

cat(crayon::blue("=== STEP 5: Computing disagreement ===\n\n"))

# Disagreement per run
disagreement_by_run <- cutoff_clean_results %>%
  group_by(group, date, tenor, run_id) %>%
  summarise(std_rate = sd(rate, na.rm = TRUE), .groups = "drop")

# Average across runs
disagreement_avg <- disagreement_by_run %>%
  group_by(group, date, tenor) %>%
  summarise(
    llm_std = mean(std_rate, na.rm = TRUE),
    llm_std_se = sd(std_rate, na.rm = TRUE) / sqrt(n()),
    n_runs = n(),
    .groups = "drop"
  )

# Load market volatility
market_vol <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(
    date = as.Date(date),
    tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)
  ) %>%
  select(date, tenor, market_vol = correct_post_mean)

# Join
cutoff_analysis_data <- disagreement_avg %>%
  inner_join(market_vol, by = c("date", "tenor")) %>%
  mutate(
    group = factor(group, levels = c("pre_cutoff", "post_cutoff"),
                  labels = c("Pre-Cutoff", "Post-Cutoff"))
  )

cat("Analysis data ready:", nrow(cutoff_analysis_data), "observations\n\n")

# ============================================================================
# STEP 6: Correlation analysis
# ============================================================================

cat(crayon::blue("=== STEP 6: Correlation analysis ===\n\n"))

# Correlations by group
correlations <- cutoff_analysis_data %>%
  group_by(tenor, group) %>%
  summarise(
    n_conferences = n_distinct(date),
    n_obs = n(),
    correlation = cor(llm_std, market_vol, method = "spearman", use = "complete.obs"),
    .groups = "drop"
  )

cat("CORRELATIONS BY GROUP:\n")
print(correlations)
cat("\n")

# Wide format for comparison
correlation_comparison <- correlations %>%
  select(tenor, group, correlation) %>%
  pivot_wider(names_from = group, values_from = correlation) %>%
  mutate(
    diff = `Post-Cutoff` - `Pre-Cutoff`,
    pct_change = (diff / abs(`Pre-Cutoff`)) * 100
  )

cat("COMPARISON:\n")
print(correlation_comparison)
cat("\n")

# ============================================================================
# STEP 7: Bootstrap test
# ============================================================================

cat(crayon::blue("=== STEP 7: Bootstrap significance test ===\n\n"))

# Bootstrap function
bootstrap_corr_diff <- function(data, indices, pre_dates, post_dates) {
  
  sample_data <- data[indices, ]
  
  pre_cor <- sample_data %>%
    filter(date %in% pre_dates) %>%
    summarise(cor = cor(llm_std, market_vol, method = "spearman", use = "complete.obs")) %>%
    pull(cor)
  
  post_cor <- sample_data %>%
    filter(date %in% post_dates) %>%
    summarise(cor = cor(llm_std, market_vol, method = "spearman", use = "complete.obs")) %>%
    pull(cor)
  
  return(post_cor - pre_cor)
}

# Run bootstrap
set.seed(12345)

bootstrap_results <- map_dfr(c("3M", "2Y", "10Y"), function(t) {
  
  cat("Bootstrapping", t, "...")
  
  tenor_data <- cutoff_analysis_data %>%
    filter(tenor == t) %>%
    mutate(group_char = as.character(group))
  
  pre_dates <- tenor_data %>% filter(group == "Pre-Cutoff") %>% pull(date) %>% unique()
  post_dates <- tenor_data %>% filter(group == "Post-Cutoff") %>% pull(date) %>% unique()
  
  boot_result <- boot(
    data = tenor_data,
    statistic = bootstrap_corr_diff,
    R = 5000,
    pre_dates = pre_dates,
    post_dates = post_dates
  )
  
  ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
  
  observed_diff <- correlation_comparison %>%
    filter(tenor == t) %>%
    pull(diff)
  
  p_value <- mean(abs(boot_result$t) >= abs(observed_diff))
  
  cat(" Done\n")
  
  data.frame(
    tenor = t,
    observed_diff = observed_diff,
    ci_lower = ci$percent[4],
    ci_upper = ci$percent[5],
    p_value = p_value,
    significant = p_value < 0.05
  )
})

cat("\nBOOTSTRAP TEST RESULTS:\n")
print(bootstrap_results)
cat("\n")

# Equivalence test
equivalence_bound <- 0.15

equivalence_test <- bootstrap_results %>%
  mutate(
    within_upper = ci_upper < equivalence_bound,
    within_lower = ci_lower > -equivalence_bound,
    is_equivalent = within_upper & within_lower,
    conclusion = case_when(
      is_equivalent ~ "Equivalent (No contamination)",
      !is_equivalent & significant ~ "Significantly different",
      TRUE ~ "Inconclusive"
    )
  )

cat("EQUIVALENCE TEST (±0.15):\n")
print(equivalence_test %>% select(tenor, observed_diff, ci_lower, ci_upper, conclusion))
cat("\n")

# ============================================================================
# STEP 8: Visualizations
# ============================================================================

cat(crayon::blue("=== STEP 8: Creating figures ===\n\n"))

library(patchwork)

# Panel A: Correlation comparison
p_correlations <- correlations %>%
  ggplot(aes(x = tenor, y = correlation, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_fill_manual(
    values = c("Pre-Cutoff" = "#4575b4", "Post-Cutoff" = "#d73027")
  ) +
  labs(
    title = "A. Correlation Before vs. After Knowledge Cutoff",
    x = "OIS Tenor",
    y = "Spearman Correlation",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 14, face = "bold")
  )

# Panel B: Difference with CI
p_difference <- bootstrap_results %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))) %>%
  ggplot(aes(x = tenor, y = observed_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_hline(
    yintercept = c(-equivalence_bound, equivalence_bound),
    linetype = "dotted",
    color = "red",
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    linewidth = 1,
    color = "#4575b4"
  ) +
  geom_point(size = 4, color = "#4575b4") +
  annotate(
    "text",
    x = 3.3, y = equivalence_bound,
    label = "Equivalence bound",
    hjust = 0, size = 3, color = "red"
  ) +
  annotate(
    "text",
    x = 3.3, y = -equivalence_bound,
    label = "Equivalence bound",
    hjust = 0, size = 3, color = "red"
  ) +
  labs(
    title = "B. Difference in Correlation (Post - Pre)",
    subtitle = "95% confidence intervals from 5,000 bootstrap samples",
    x = "OIS Tenor",
    y = "Correlation Difference"
  ) +
  coord_cartesian(xlim = c(0.5, 3.5)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey40")
  )

# Combine
p_combined <- p_correlations / p_difference +
  plot_annotation(
    title = "Knowledge Cutoff Analysis: No Evidence of Data Contamination",
    subtitle = sprintf(
      "Pre-cutoff: %s to %s (N=%d) | Post-cutoff: %s to %s (N=%d)",
      format(min(pre_cutoff_select), "%Y-%m-%d"),
      format(max(pre_cutoff_select), "%Y-%m-%d"),
      n_conferences,
      format(min(post_cutoff_select), "%Y-%m-%d"),
      format(max(post_cutoff_select), "%Y-%m-%d"),
      n_conferences
    ),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "grey30")
    )
  )

# Save
ggsave(
  "../output/figures/knowledge_cutoff_analysis.pdf",
  plot = p_combined,
  dpi = 320,
  width = 12,
  height = 10,
  bg = "white"
)

cat(crayon::green("Figure saved: ../output/figures/knowledge_cutoff_analysis.pdf\n\n"))

# ============================================================================
# STEP 9: Export results
# ============================================================================

cat(crayon::blue("=== STEP 9: Exporting results ===\n\n"))

write_xlsx(
  list(
    correlations = correlations,
    comparison = correlation_comparison,
    bootstrap = bootstrap_results,
    equivalence = equivalence_test,
    dates = data.frame(
      group = rep(c("Pre-Cutoff", "Post-Cutoff"), each = n_conferences),
      date = c(pre_cutoff_select, post_cutoff_select)
    ),
    raw_data = cutoff_analysis_data
  ),
  "../output/tables/knowledge_cutoff_analysis.xlsx"
)

cat(crayon::green("Tables saved: ../output/tables/knowledge_cutoff_analysis.xlsx\n\n"))

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat(crayon::bold(crayon::blue("\n========================================\n")))
cat(crayon::bold(crayon::blue("KNOWLEDGE CUTOFF ANALYSIS - SUMMARY\n")))
cat(crayon::bold(crayon::blue("========================================\n\n")))

cat("Knowledge cutoff:", format(knowledge_cutoff, "%Y-%m-%d"), "\n")
cat("Conferences analyzed:", n_conferences, "before +", n_conferences, "after\n\n")

cat("CORRELATIONS:\n")
for (i in 1:nrow(correlation_comparison)) {
  cat(sprintf(
    "  %s: %.3f → %.3f (Δ=%+.3f, %.1f%% change, p=%.3f)\n",
    correlation_comparison$tenor[i],
    correlation_comparison$`Pre-Cutoff`[i],
    correlation_comparison$`Post-Cutoff`[i],
    correlation_comparison$diff[i],
    correlation_comparison$pct_change[i],
    bootstrap_results$p_value[i]
  ))
}

cat("\nEQUIVALENCE TEST:\n")
for (i in 1:nrow(equivalence_test)) {
  cat(sprintf("  %s: %s\n",
              equivalence_test$tenor[i],
              equivalence_test$conclusion[i]))
}

cat(crayon::bold(crayon::green("\n✓ Analysis complete!\n\n")))