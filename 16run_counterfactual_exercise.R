# ============================================================================
# CROSS-LLM COUNTERFACTUAL EXPERIMENT - HIGH VOLATILITY SAMPLE
# ============================================================================
# Modified to select 40 conferences with highest market-based disagreement

# Load libraries ----
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  openai, httr2, crayon, stringr, purrr, readr, writexl, readxl, 
  tidyverse, glue, gemini.R
)

# Configuration ----
setwd("~/../Desktop/Projects/Uncertainty_surprises/code/")

# API keys
Sys.setenv(OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY"))
Sys.setenv(GEMINI_API_KEY = Sys.getenv("GEMINI_API_KEY"))

setAPI(Sys.getenv("GEMINI_API_KEY"))

TEMPERATURE_EDIT <- 1.0
TEMPERATURE_EVAL <- 1.0
SEED <- 42
N_RUNS <- 1  # Number of evaluation runs per edited text

# Directories
OUTPUT_DIR <- "../intermediate_data/counterfactual_cross_llm_high_vol"
RAW_DIR <- file.path(OUTPUT_DIR, "raw_data")
PARSED_DIR <- file.path(OUTPUT_DIR, "parsed_data")

walk(c(OUTPUT_DIR, RAW_DIR, PARSED_DIR), 
     ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# =============================================================================
# STEP 1: API FUNCTIONS (Same as before)
# =============================================================================

# Load naive prompt
source("create_prompts.R")

# Editing prompt template
targeted_edit_prompt <- function(text, date) {
  glue(
    "You are editing an ECB press conference to reduce ONLY linguistic ambiguity.

STRICT CONSTRAINTS - DO NOT CHANGE:
- Any policy decisions (interest rates, asset purchases, programs)
- Any economic data, statistics, or forecasts
- The overall hawkish/dovish stance
- Substantive content or meaning

ONLY EDIT:
1. **Vague temporal phrases**: 
   'sufficiently long duration' → Use context from same text to infer timeframe
   'some time', 'appropriate period' → Make more specific using surrounding discussion
   
2. **Internal contradictions**:
   If prepared statement and Q&A conflict → Align using the clearer version
   
3. **Excessive hedging**:
   Multiple qualifiers ('may potentially consider') → Simplify if context supports

IMPORTANT: Base any specificity ONLY on information already in the text.
Do NOT invent dates, numbers, or commitments.

ORIGINAL TEXT ({date}):
{text}

EDITED VERSION:"
  )
}

# Evaluation prompt template
evaluation_prompt <- function(text, date) {
  full_prompt <- gsub("\\[date\\]", date, prompt_naive)
  paste0(full_prompt, "Press Conference on ", date, "\nText: ", text)
}

# --- ChatGPT Functions ---
call_chatgpt_edit <- function(text, date) {
  tryCatch({
    result <- create_chat_completion(
      model = "gpt-5-mini",
      messages = list(
        list(role = "user", content = targeted_edit_prompt(text, date))
      ),
      temperature = TEMPERATURE_EDIT
    )
    return(result$choices$message.content)
  }, error = function(e) {
    cat(crayon::red(glue("ChatGPT Edit Error: {e$message}\n")))
    return(NULL)
  })
}

call_chatgpt_eval <- function(text, date) {
  tryCatch({
    result <- create_chat_completion(
      model = "gpt-5-mini",
      messages = list(
        list(role = "user", content = evaluation_prompt(text, date))
      ),
      temperature = TEMPERATURE_EVAL
    )
    return(result$choices$message.content)
  }, error = function(e) {
    cat(crayon::red(glue("ChatGPT Eval Error: {e$message}\n")))
    return(NULL)
  })
}

# --- Gemini Functions ---
call_gemini_edit <- function(text, date) {
  tryCatch({
    result <- new_gemini(
      targeted_edit_prompt(text, date),
      model = "2.5-flash",
      temperature = TEMPERATURE_EDIT
    )
    return(result)
  }, error = function(e) {
    cat(crayon::red(glue("Gemini Edit Error: {e$message}\n")))
    return(NULL)
  })
}

call_gemini_eval <- function(text, date) {
  tryCatch({
    result <- new_gemini(
      evaluation_prompt(text, date),
      model = "2.5-flash",
      temperature = TEMPERATURE_EVAL
    )
    return(result)
  }, error = function(e) {
    cat(crayon::red(glue("Gemini Eval Error: {e$message}\n")))
    return(NULL)
  })
}

# --- Unified Wrapper ---
call_llm <- function(model_name, operation, text, date) {
  result <- switch(
    paste(model_name, operation, sep = "_"),
    "chatgpt_edit" = call_chatgpt_edit(text, date),
    "chatgpt_eval" = call_chatgpt_eval(text, date),
    "gemini_edit" = call_gemini_edit(text, date),
    "gemini_eval" = call_gemini_eval(text, date),
    {
      cat(crayon::red(glue("Unknown combination: {model_name}_{operation}\n")))
      return(NULL)
    }
  )
  
  return(result)
}

# =============================================================================
# STEP 2: DATA LOADING - MODIFIED FOR HIGH VOLATILITY SAMPLE
# =============================================================================

cat(crayon::blue$bold("\n=== LOADING DATA ===\n\n"))

# Load existing Gemini baseline results
clean_df <- read_xlsx("../intermediate_data/aggregate_gemini_result/prompt_naive/2.5flash_2025-07-21.xlsx") %>%
  mutate(date = as.character(date))

# Compute baseline disagreement for ALL conferences
baseline_disagreement_full <- clean_df %>%
  group_by(date, tenor) %>%
  summarise(baseline_std = sd(rate, na.rm = TRUE), .groups = "drop")

# Calculate average disagreement across tenors for ranking
avg_disagreement_by_date <- baseline_disagreement_full %>%
  group_by(date) %>%
  summarise(avg_std = mean(baseline_std, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_std))

# Select top 40 conferences with highest average disagreement
selected_conferences <- avg_disagreement_by_date %>%
  slice_head(n = 40) %>%
  pull(date)

cat(crayon::green("Selected 40 conferences with HIGHEST market-based disagreement:\n"))
cat(paste(sort(selected_conferences), collapse = ", "), "\n\n")

# Show summary statistics
cat(crayon::cyan("\n=== SAMPLE STATISTICS ===\n"))
sample_stats <- avg_disagreement_by_date %>%
  slice_head(n = 40) %>%
  summarise(
    min_std = min(avg_std),
    median_std = median(avg_std),
    mean_std = mean(avg_std),
    max_std = max(avg_std)
  )

cat(glue("Average std dev across sample:\n"))
cat(glue("  Min: {round(sample_stats$min_std, 4)}\n"))
cat(glue("  Median: {round(sample_stats$median_std, 4)}\n"))
cat(glue("  Mean: {round(sample_stats$mean_std, 4)}\n"))
cat(glue("  Max: {round(sample_stats$max_std, 4)}\n\n"))

# Save sample metadata
write_csv(
  avg_disagreement_by_date %>% 
    slice_head(n = 40) %>%
    mutate(sample_order = 1:40),
  file.path(OUTPUT_DIR, "sample_conferences_high_vol.csv")
)

# Load original press conference texts
ecb_texts <- list.files("../intermediate_data/texts/", full.names = TRUE) %>%
  str_subset("\\d{4}-\\d{2}-\\d{2}") %>%
  {
    dates <- basename(.) %>% str_extract("\\d{4}-\\d{2}-\\d{2}")
    texts <- map(., ~ read_file(.x))
    set_names(texts, dates)
  }

# Get baseline disagreement for selected conferences only
baseline_disagreement <- baseline_disagreement_full %>%
  filter(date %in% selected_conferences)

write_csv(baseline_disagreement, file.path(OUTPUT_DIR, "baseline_disagreement.csv"))

# =============================================================================
# PART 1: DATA COLLECTION (Same as before, but with selected_conferences)
# =============================================================================

cat(crayon::blue$bold("\n=== PART 1: DATA COLLECTION ===\n\n"))

# =============================================================================
# Experiment A: ChatGPT edits → Gemini evaluates
# =============================================================================

cat(crayon::cyan$bold("--- Experiment A: ChatGPT edits → Gemini evaluates ---\n\n"))

dir.create(file.path(RAW_DIR, "exp_a"), recursive = TRUE, showWarnings = FALSE)

for (conf_date in selected_conferences) {
  cat(crayon::yellow(conf_date), ": ")
  
  # Define file paths
  edit_file <- file.path(RAW_DIR, "exp_a", paste0(conf_date, "_chatgpt_edit.txt"))
  eval_files <- paste0(file.path(RAW_DIR, "exp_a", conf_date), "_gemini_eval_run", 1:N_RUNS, ".txt")
  
  # Skip if all files exist
  if (all(file.exists(c(edit_file, eval_files)))) {
    cat(crayon::green("(cached) ✓\n"))
    next
  }
  
  tryCatch({
    original_text <- ecb_texts[[conf_date]]
    
    # Step 1: ChatGPT edits
    if (!file.exists(edit_file)) {
      cat("edit(GPT)...")
      edited_text <- call_llm("chatgpt", "edit", original_text, conf_date)
      
      if (!is.null(edited_text)) {
        write_file(edited_text, edit_file)
        cat(crayon::green("✓ "))
        Sys.sleep(3)
      } else {
        cat(crayon::red("✗ "))
        next
      }
    } else {
      edited_text <- read_file(edit_file)
      cat(crayon::silver("edit(cached) "))
    }
    
    # Step 2: Gemini evaluates (N_RUNS times)
    for (run in 1:N_RUNS) {
      if (!file.exists(eval_files[run])) {
        cat(glue("eval{run}(Gemini)..."))
        
        eval_result <- call_llm("gemini", "eval", edited_text, conf_date)
        
        if (!is.null(eval_result)) {
          write_file(eval_result, eval_files[run])
          cat(crayon::green("✓ "))
          Sys.sleep(3)
        } else {
          cat(crayon::red("✗ "))
        }
      } else {
        cat(crayon::silver(glue("eval{run}(cached) ")))
      }
    }
    
    cat(crayon::green("Complete!\n"))
    
  }, error = function(e) {
    cat(crayon::red(glue("ERROR: {e$message}\n")))
  })
}

# =============================================================================
# Experiment B: Gemini edits → ChatGPT evaluates
# =============================================================================

cat(crayon::cyan$bold("\n--- Experiment B: Gemini edits → ChatGPT evaluates ---\n\n"))

dir.create(file.path(RAW_DIR, "exp_b"), recursive = TRUE, showWarnings = FALSE)

for (conf_date in selected_conferences) {
  cat(crayon::yellow(conf_date), ": ")
  
  # Define file paths
  edit_file <- file.path(RAW_DIR, "exp_b", paste0(conf_date, "_gemini_edit.txt"))
  baseline_eval_files <- paste0(file.path(RAW_DIR, "exp_b", conf_date), "_chatgpt_baseline_run", 1:N_RUNS, ".txt")
  eval_files <- paste0(file.path(RAW_DIR, "exp_b", conf_date), "_chatgpt_eval_run", 1:N_RUNS, ".txt")
  
  # Skip if all files exist
  if (all(file.exists(c(edit_file, baseline_eval_files, eval_files)))) {
    cat(crayon::green("(cached) ✓\n"))
    next
  }
  
  tryCatch({
    original_text <- ecb_texts[[conf_date]]
    
    # Step 1: ChatGPT evaluates original (baseline for Exp B)
    for (run in 1:N_RUNS) {
      if (!file.exists(baseline_eval_files[run])) {
        cat(glue("baseline{run}(GPT)..."))
        
        baseline_result <- call_llm("chatgpt", "eval", original_text, conf_date)
        
        if (!is.null(baseline_result)) {
          write_file(baseline_result, baseline_eval_files[run])
          cat(crayon::green("✓ "))
          Sys.sleep(3)
        } else {
          cat(crayon::red("✗ "))
        }
      } else {
        cat(crayon::silver(glue("baseline{run}(cached) ")))
      }
    }
    
    # Step 2: Gemini edits
    if (!file.exists(edit_file)) {
      cat("edit(Gemini)...")
      edited_text <- call_llm("gemini", "edit", original_text, conf_date)
      
      if (!is.null(edited_text)) {
        write_file(edited_text, edit_file)
        cat(crayon::green("✓ "))
        Sys.sleep(3)
      } else {
        cat(crayon::red("✗ "))
        next
      }
    } else {
      edited_text <- read_file(edit_file)
      cat(crayon::silver("edit(cached) "))
    }
    
    # Step 3: ChatGPT evaluates edited (N_RUNS times)
    for (run in 1:N_RUNS) {
      if (!file.exists(eval_files[run])) {
        cat(glue("eval{run}(GPT)..."))
        
        eval_result <- call_llm("chatgpt", "eval", edited_text, conf_date)
        
        if (!is.null(eval_result)) {
          write_file(eval_result, eval_files[run])
          cat(crayon::green("✓ "))
          Sys.sleep(3)
        } else {
          cat(crayon::red("✗ "))
        }
      } else {
        cat(crayon::silver(glue("eval{run}(cached) ")))
      }
    }
    
    cat(crayon::green("Complete!\n"))
    
  }, error = function(e) {
    cat(crayon::red(glue("ERROR: {e$message}\n")))
  })
}

cat(crayon::blue$bold("\n✓ DATA COLLECTION COMPLETE!\n"))
cat(crayon::silver("Raw data saved in:"), RAW_DIR, "\n\n")

# =============================================================================
# PART 2: PARSING (Same as before)
# =============================================================================

cat(crayon::blue$bold("\n=== PART 2: PARSING RAW OUTPUTS ===\n\n"))

# Parsing function
parse_llm_table <- function(text_result, date) {
  tryCatch({
    parsed <- read_delim(I(text_result), delim = "|", trim_ws = TRUE, 
                        skip = 1, show_col_types = FALSE) %>%
      select(-1, -ncol(.)) %>%
      slice(-nrow(.))
    
    if (ncol(parsed) != 6) {
      return(tibble(date = date, tenor = c("3M", "2Y", "10Y"), rate = NA_real_))
    }
    
    parsed <- parsed %>%
      setNames(c("date", "id", "tenor", "direction", "rate", "confidence")) %>%
      mutate(
        date = date,
        rate = suppressWarnings(as.numeric(rate)),
        tenor = as.character(tenor)
      ) %>%
      filter(tenor %in% c("3M", "2Y", "10Y")) %>%
      select(date, id, tenor, rate)
    
    if (all(is.na(parsed$rate))) {
      return(tibble(date = date, tenor = c("3M", "2Y", "10Y"), rate = NA_real_))
    }
    
    parsed
    
  }, error = function(e) {
    tibble(date = date, tenor = c("3M", "2Y", "10Y"), rate = NA_real_)
  })
}

# Parse Experiment A
cat(crayon::cyan("Parsing Experiment A...\n"))

parsed_exp_a <- map_dfr(selected_conferences, function(conf_date) {
  cat(crayon::yellow(conf_date), ": ")
  
  eval_files <- paste0(file.path(RAW_DIR, "exp_a", conf_date), "_gemini_eval_run", 1:N_RUNS, ".txt")
  
  if (!all(file.exists(eval_files))) {
    cat(crayon::red("Missing files, skipping\n"))
    return(tibble())
  }
  
  # Parse all runs
  all_runs <- map_dfr(1:N_RUNS, function(run) {
    raw_text <- read_file(eval_files[run])
    parsed <- parse_llm_table(raw_text, conf_date) %>%
      mutate(run = run, date = as.Date(date))
  })
  
  # Calculate std dev across agents for each run, then average across runs
  result <- all_runs %>%
    group_by(date, tenor, run) %>%
    summarise(std_dev = sd(rate, na.rm = TRUE), .groups = "drop") %>%
    group_by(date, tenor) %>%
    summarise(edited_std = mean(std_dev, na.rm = TRUE), .groups = "drop")
  
  cat(crayon::green("✓\n"))
  result
})

# Add baseline and compute reduction
results_exp_a <- baseline_disagreement %>%
  mutate(date = as.Date(date)) %>%
  left_join(parsed_exp_a, by = c("date", "tenor")) %>%
  mutate(
    reduction_pct = (baseline_std - edited_std) / baseline_std * 100,
    reduction_abs = baseline_std - edited_std,
    experiment = "ChatGPT → Gemini"
  )

write_csv(results_exp_a, file.path(PARSED_DIR, "exp_a_results.csv"))

# Parse Experiment B
cat(crayon::cyan("Parsing Experiment B...\n"))

parsed_exp_b <- map_dfr(selected_conferences, function(conf_date) {
  cat(crayon::yellow(conf_date), ": ")
  
  baseline_files <- paste0(file.path(RAW_DIR, "exp_b", conf_date), "_chatgpt_baseline_run", 1:N_RUNS, ".txt")
  eval_files <- paste0(file.path(RAW_DIR, "exp_b", conf_date), "_chatgpt_eval_run", 1:N_RUNS, ".txt")
  
  if (!all(file.exists(c(baseline_files, eval_files)))) {
    cat(crayon::red("Missing files, skipping\n"))
    return(tibble())
  }
  
  # Parse baseline runs
  baseline_runs <- map_dfr(1:N_RUNS, function(run) {
    raw_text <- read_file(baseline_files[run])
    parsed <- parse_llm_table(raw_text, conf_date) %>%
      mutate(run = run)
    parsed
  })
  
  baseline_std <- baseline_runs %>%
    group_by(date, tenor, run) %>%
    summarise(std_dev = sd(rate, na.rm = TRUE), .groups = "drop") %>%
    group_by(date, tenor) %>%
    summarise(baseline_std = mean(std_dev, na.rm = TRUE), .groups = "drop")
  
  # Parse edited runs
  edited_runs <- map_dfr(1:N_RUNS, function(run) {
    raw_text <- read_file(eval_files[run])
    parsed <- parse_llm_table(raw_text, conf_date) %>%
      mutate(run = run)
    parsed
  })
  
  edited_std <- edited_runs %>%
    group_by(date, tenor, run) %>%
    summarise(std_dev = sd(rate, na.rm = TRUE), .groups = "drop") %>%
    group_by(date, tenor) %>%
    summarise(edited_std = mean(std_dev, na.rm = TRUE), .groups = "drop")
  
  # Combine
  result <- baseline_std %>%
    left_join(edited_std, by = c("date", "tenor"))
  
  cat(crayon::green("✓\n"))
  result
})

results_exp_b <- parsed_exp_b %>%
  mutate(
    reduction_pct = (baseline_std - edited_std) / baseline_std * 100,
    reduction_abs = baseline_std - edited_std,
    experiment = "Gemini → ChatGPT"
  )

write_csv(results_exp_b, file.path(PARSED_DIR, "exp_b_results.csv"))

cat(crayon::blue$bold("\n✓ PARSING COMPLETE!\n"))
cat(crayon::silver("Parsed data saved in:"), PARSED_DIR, "\n\n")

# =============================================================================
# PART 3: ANALYSIS & VISUALIZATION
# =============================================================================

cat(crayon::blue$bold("\n=== PART 3: ANALYSIS ===\n\n"))

# Combined results
all_results <- bind_rows(results_exp_a, results_exp_b)

# Summary statistics
summary_stats <- all_results %>%
  drop_na(reduction_pct) %>%
  group_by(experiment, tenor) %>%
  summarise(
    n = n(),
    mean_reduction_pct = mean(reduction_pct),
    median_reduction_pct = median(reduction_pct),
    sd_reduction = sd(reduction_pct),
    se = sd_reduction / sqrt(n),
    pct_improved = mean(reduction_pct > 0) * 100,
    mean_baseline = mean(baseline_std),
    mean_edited = mean(edited_std),
    .groups = "drop"
  ) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

cat(crayon::green("=== SUMMARY STATISTICS ===\n"))
print(summary_stats)

# Statistical tests
t_test_results <- all_results %>%
  drop_na(reduction_pct) %>%
  group_by(experiment, tenor) %>%
  summarise(
    mean_reduction = mean(reduction_pct),
    t_stat = t.test(reduction_pct, mu = 0, alternative = "greater")$statistic,
    p_value = t.test(reduction_pct, mu = 0, alternative = "greater")$p.value,
    ci_lower = t.test(reduction_pct, mu = 0)$conf.int[1],
    ci_upper = t.test(reduction_pct, mu = 0)$conf.int[2],
    .groups = "drop"
  )

cat(crayon::green("\n=== STATISTICAL TESTS ===\n"))
print(t_test_results)

# Cross-experiment correlation
correlation_check <- results_exp_a %>%
  filter(tenor == "2Y") %>%
  select(date, reduction_a = reduction_pct) %>%
  inner_join(
    results_exp_b %>% filter(tenor == "2Y") %>% select(date, reduction_b = reduction_pct),
    by = "date"
  ) %>%
  drop_na()

if (nrow(correlation_check) > 0) {
  cross_cor <- cor(correlation_check$reduction_a, correlation_check$reduction_b)
  cat(crayon::green("\n=== CROSS-EXPERIMENT AGREEMENT (2Y) ===\n"))
  cat(glue("Correlation: {round(cross_cor, 3)}\n\n"))
}

# Save all results
write_xlsx(
  list(
    "Summary" = summary_stats,
    "Statistical_Tests" = t_test_results,
    "Exp_A_Detailed" = results_exp_a,
    "Exp_B_Detailed" = results_exp_b,
    "Cross_Correlation" = correlation_check
  ),
  file.path(OUTPUT_DIR, "counterfactual_results_high_vol.xlsx")
)

# =============================================================================
# VISUALIZATION
# =============================================================================

library(showtext)
font_add("Segoe UI", regular = "segoeui.ttf")
showtext_auto()

# Plot 1: Mean reduction by experiment
p1 <- summary_stats %>%
  ggplot(aes(x = tenor, y = mean_reduction_pct, fill = experiment)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = mean_reduction_pct - 1.96 * se, 
        ymax = mean_reduction_pct + 1.96 * se),
    position = position_dodge(0.8),
    width = 0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(
    values = c("ChatGPT → Gemini" = "#4575b4", "Gemini → ChatGPT" = "#d73027")
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "Disagreement Reduction (%)",
    fill = NULL,
    caption = glue("Error bars: 95% CI. Each edited version evaluated with {N_RUNS} runs.\nSample: 40 conferences with highest market-based disagreement.")
  ) +
  theme_minimal(base_family = "Segoe UI") +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey50")
  )

ggsave("../output/figures/counterfactual_cross_llm_mean_high_vol.pdf",
       plot = p1, width = 10, height = 6, dpi = 320, bg = "white")

cat(crayon::blue$bold("\n✓ ALL STEPS COMPLETE!\n\n"))
cat(crayon::green("Results saved to:\n"))
cat(" - Raw data:", RAW_DIR, "\n")
cat(" - Parsed data:", PARSED_DIR, "\n")
cat(" - Analysis:", file.path(OUTPUT_DIR, "counterfactual_results_high_vol.xlsx"), "\n")
cat(" - Figure:", "../output/figures/counterfactual_cross_llm_mean_high_vol.pdf", "\n")


summary_stats_a

