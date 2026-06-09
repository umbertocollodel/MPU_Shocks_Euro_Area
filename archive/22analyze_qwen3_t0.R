#===============================================================================
# ANALYZE QWEN3 TEMPERATURE-0 RESULTS
#===============================================================================
# Project: Interpreting the Interpreter - ECB Communication Analysis
# Author: Umberto Collodel
# Institution: Central Bank of Malta
#
# Purpose:
#   Parse raw Qwen3 (temperature = 0) LLM outputs and evaluate their correlation
#   with realized OIS market volatility. Also compares t=0 vs t=1 to assess
#   whether stochastic sampling matters for predictive performance.
#
# Inputs:
#   ../intermediate_data/openrouter_result/qwen3_naive_t0/*.rds   (from 21run_qwen3_t0.R)
#   ../intermediate_data/openrouter_result/qwen3_naive/*.rds      (temperature=1 baseline)
#   ../intermediate_data/range_difference_df.rds                  (market MPU measure)
#
# Outputs:
#   ../output/figures/qwen3_t0/
#     spearman_corr_t0_vs_market.pdf    — bar chart: t=0 vs market by tenor
#     t0_vs_t1_comparison.pdf           — side-by-side: t=0 vs t=1 vs market
#     t0_vs_t1_scatter.pdf              — scatter: t=0 synthetic SD vs t=1 synthetic SD
#
# Usage:
#   source("22analyze_qwen3_t0.R")
#===============================================================================

cat("\n", strrep("=", 80), "\n")
cat("ANALYZE QWEN3 TEMPERATURE-0 RESULTS\n")
cat(strrep("=", 80), "\n\n")

# Packages ---------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  boot,
  showtext,
  patchwork
)

# Font -------------------------------------------------------------------------
if (!("Segoe UI" %in% font_families())) {
  font_path <- file.path(getwd(), "segoeui.ttf")
  if (file.exists(font_path)) {
    font_add("Segoe UI", regular = font_path)
  } else {
    warning("Segoe UI font file not found. Using default font.")
  }
}
showtext_auto()

# Configuration ----------------------------------------------------------------
input_dir_t0 <- "../intermediate_data/openrouter_result/qwen3_naive_t0"
input_dir_t1 <- "../intermediate_data/openrouter_result/qwen3_naive"
output_dir   <- "../output/figures/qwen3_t0"
n_bootstrap  <- 1500
ci_level     <- 0.88

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

color_palette <- c("3M" = "#91bfdb", "2Y" = "#4575b4", "10Y" = "#d73027")

# Theme ------------------------------------------------------------------------
theme_paper <- theme_minimal(base_family = "Segoe UI") +
  theme(
    axis.title.x    = element_text(margin = margin(t = 10), size = 16),
    axis.title.y    = element_text(margin = margin(r = 10), size = 16),
    axis.text       = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#===============================================================================
# SECTION 1: PARSE RAW RDS FILES
#===============================================================================

#' Parse a single markdown-table LLM response into a tibble
parse_llm_response <- function(response, conf_date) {
  tryCatch({
    if (is.null(response) || length(response) == 0 || nchar(response) == 0) {
      cat("  Warning: Empty response for", conf_date, "\n")
      return(NULL)
    }

    lines      <- strsplit(response, "\n")[[1]]
    data_lines <- lines[grepl("\\d{4}-\\d{2}-\\d{2}", lines)]

    if (length(data_lines) == 0) {
      cat("  Warning: No data rows found for", conf_date, "\n")
      return(NULL)
    }

    parsed_list <- lapply(data_lines, function(line) {
      parts <- trimws(strsplit(line, "\\|")[[1]])
      parts <- parts[parts != ""]
      if (length(parts) >= 6) {
        data.frame(
          date      = parts[1],
          id        = parts[2],
          tenor     = parts[3],
          direction = parts[4],
          rate      = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", parts[5]))),
          confidence = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", parts[6]))),
          stringsAsFactors = FALSE
        )
      } else NULL
    })

    parsed <- bind_rows(parsed_list)

    if (nrow(parsed) == 0) {
      cat("  Warning: No valid rows parsed for", conf_date, "\n")
      return(NULL)
    }

    cat("  Parsed", nrow(parsed), "rows for", conf_date, "\n")
    as_tibble(parsed)

  }, error = function(e) {
    cat("  Error parsing", conf_date, ":", e$message, "\n")
    NULL
  })
}

#' Read and parse all RDS files from a directory
load_rds_dir <- function(dir_path, label) {
  cat(crayon::blue(paste0("Parsing ", label, " from: ", dir_path, "\n")))
  rds_files <- list.files(dir_path, pattern = "\\d{4}-\\d{2}-\\d{2}\\.rds$",
                          full.names = TRUE)
  if (length(rds_files) == 0) stop(paste0("No RDS files found in: ", dir_path))
  cat("  Found", length(rds_files), "files\n")

  dates <- basename(rds_files) %>% str_extract("\\d{4}-\\d{2}-\\d{2}")

  results <- map2(rds_files, dates, function(f, d) {
    resp <- tryCatch(readRDS(f), error = function(e) NULL)
    parse_llm_response(resp, d)
  })

  valid <- keep(results, ~ !is.null(.x) && nrow(.x) > 0)
  cat(crayon::green(paste0("  Parsed ", length(valid), "/", length(rds_files), " files successfully\n\n")))

  bind_rows(valid) %>%
    filter(tenor %in% c("3M", "2Y", "10Y"), !is.na(rate)) %>%
    mutate(date = as.Date(date))
}

# Load both temperature variants -----------------------------------------------
raw_t0 <- load_rds_dir(input_dir_t0, "Qwen3 t=0")
raw_t1 <- load_rds_dir(input_dir_t1, "Qwen3 t=1")

#===============================================================================
# SECTION 2: COMPUTE SYNTHETIC STANDARD DEVIATIONS
#===============================================================================

std_t0 <- raw_t0 %>%
  group_by(date, tenor) %>%
  summarise(std_rate = sd(rate, na.rm = TRUE), n_traders = n(), .groups = "drop")

std_t1 <- raw_t1 %>%
  group_by(date, tenor) %>%
  summarise(std_rate = sd(rate, na.rm = TRUE), n_traders = n(), .groups = "drop")

cat("Synthetic SD computed:\n")
cat("  t=0 date-tenor obs:", nrow(std_t0), "\n")
cat("  t=1 date-tenor obs:", nrow(std_t1), "\n\n")

#===============================================================================
# SECTION 3: LOAD MARKET MPU MEASURE
#===============================================================================

cat(crayon::blue("Loading market OIS volatility data...\n"))
range_df <- read_rds("../intermediate_data/range_difference_df.rds") %>%
  mutate(tenor = case_when(tenor == "3mnt" ~ "3M", TRUE ~ tenor)) %>%
  select(tenor, date, correct_post_mean_1) %>%
  rename(correct_post_mean = correct_post_mean_1) %>%
  filter(tenor %in% c("3M", "2Y", "10Y"))

cat(crayon::green(paste0("Loaded ", n_distinct(range_df$date), " OIS dates\n\n")))

# Join with market data --------------------------------------------------------
combined_t0 <- range_df %>% inner_join(std_t0, by = c("date", "tenor"))
combined_t1 <- range_df %>% inner_join(std_t1, by = c("date", "tenor"))

cat("After joining with OIS:\n")
cat("  t=0 matched rows:", nrow(combined_t0), "\n")
cat("  t=1 matched rows:", nrow(combined_t1), "\n\n")

#===============================================================================
# SECTION 4: BOOTSTRAP SPEARMAN CORRELATION
#===============================================================================

boot_spearman <- function(data, indices) {
  d <- data[indices, ]
  cor(d$std_rate, d$correct_post_mean, method = "spearman", use = "complete.obs")
}

compute_ci <- function(df, n_boot = n_bootstrap, conf = ci_level) {
  df %>%
    group_by(tenor) %>%
    nest() %>%
    mutate(
      point_est = map_dbl(data, ~ cor(.x$std_rate, .x$correct_post_mean,
                                     method = "spearman", use = "complete.obs")),
      boot_obj  = map(data, ~ boot(.x, boot_spearman, R = n_boot,
                                   sim = "ordinary", stype = "i")),
      ci        = map(boot_obj, ~ boot.ci(.x, type = "perc", conf = conf)),
      ci_lower  = map_dbl(ci, ~ .x$percent[4]),
      ci_upper  = map_dbl(ci, ~ .x$percent[5]),
      n_obs     = map_int(data, nrow)
    ) %>%
    select(tenor, correlation = point_est, ci_lower, ci_upper, n_obs) %>%
    ungroup() %>%
    mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))
}

set.seed(42)
cat(crayon::blue("Computing bootstrap CIs for t=0...\n"))
ci_t0 <- compute_ci(combined_t0)

set.seed(42)
cat(crayon::blue("Computing bootstrap CIs for t=1...\n"))
ci_t1 <- compute_ci(combined_t1)

#===============================================================================
# SECTION 5: PRINT RESULTS
#===============================================================================

cat("\n", strrep("=", 60), "\n")
cat("SPEARMAN CORRELATION: Qwen3 t=0 vs Market\n")
cat(strrep("=", 60), "\n")
print(ci_t0)

cat("\n", strrep("=", 60), "\n")
cat("SPEARMAN CORRELATION: Qwen3 t=1 vs Market\n")
cat(strrep("=", 60), "\n")
print(ci_t1)

cat("\n", strrep("=", 60), "\n")
cat("SUMMARY TABLE\n")
cat(strrep("=", 60), "\n")
summary_table <- tibble(
  Tenor         = c("3M", "2Y", "10Y"),
  `t=0 vs Market` = ci_t0$correlation,
  `t=1 vs Market` = ci_t1$correlation,
  `Difference`    = ci_t0$correlation - ci_t1$correlation
)
print(summary_table)
cat("\n")

#===============================================================================
# SECTION 6: FIGURES
#===============================================================================

# --- Figure 1: t=0 vs Market (bar chart with CIs) ----------------------------

p1 <- ggplot(ci_t0, aes(x = tenor, y = correlation, fill = tenor)) +
  geom_col(width = 0.4, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.15, color = "grey10", linewidth = 1
  ) +
  geom_text(aes(label = sprintf("%.2f", correlation)),
            vjust = -0.5, nudge_y = (max(ci_t0$ci_upper, na.rm = TRUE) * 0.05),
            size = 5) +
  scale_fill_manual(values = color_palette) +
  labs(
    x = "OIS Tenor",
    y = "Spearman Correlation"
  ) +
  expand_limits(y = c(-0.05, max(ci_t0$ci_upper, na.rm = TRUE) + 0.15)) +
  theme_paper

ggsave(file.path(output_dir, "spearman_corr_t0_vs_market.pdf"),
       p1, dpi = 320, width = 8, height = 6, bg = "white")
cat("Saved: spearman_corr_t0_vs_market.pdf\n")

# --- Figure 2: t=0 vs t=1 side-by-side comparison ----------------------------

comparison_df <- bind_rows(
  ci_t0 %>% mutate(model = "Qwen3  t = 0"),
  ci_t1 %>% mutate(model = "Qwen3  t = 1")
) %>%
  mutate(
    model = factor(model, levels = c("Qwen3  t = 0", "Qwen3  t = 1")),
    tenor = factor(tenor, levels = c("3M", "2Y", "10Y"))
  )

p2 <- ggplot(comparison_df,
             aes(x = tenor, y = correlation, fill = tenor, alpha = model)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.4,
           show.legend = TRUE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.5),
    width = 0.15, color = "grey10", linewidth = 0.8
  ) +
  scale_fill_manual(values = color_palette, guide = "none") +
  scale_alpha_manual(
    values = c("Qwen3  t = 0" = 1, "Qwen3  t = 1" = 0.5),
    name = NULL
  ) +
  labs(
    x = "OIS Tenor",
    y = "Spearman Correlation vs Market"
  ) +
  expand_limits(y = c(-0.05, max(comparison_df$ci_upper, na.rm = TRUE) + 0.15)) +
  theme_paper +
  theme(legend.position = "bottom", legend.text = element_text(size = 13))

ggsave(file.path(output_dir, "t0_vs_t1_comparison.pdf"),
       p2, dpi = 320, width = 8, height = 6, bg = "white")
cat("Saved: t0_vs_t1_comparison.pdf\n")

# --- Figure 3: Scatter of synthetic SDs, t=0 vs t=1 --------------------------

scatter_df <- std_t0 %>%
  inner_join(std_t1, by = c("date", "tenor"), suffix = c("_t0", "_t1")) %>%
  mutate(tenor = factor(tenor, levels = c("3M", "2Y", "10Y")))

cor_scatter <- scatter_df %>%
  group_by(tenor) %>%
  summarise(
    r = cor(std_rate_t0, std_rate_t1, method = "spearman", use = "complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("rho == %.2f", r))

p3 <- ggplot(scatter_df, aes(x = std_rate_t1, y = std_rate_t0, color = tenor)) +
  geom_point(alpha = 0.6, size = 2, show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_text(
    data = cor_scatter,
    aes(label = label, color = tenor),
    x = -Inf, y = Inf, hjust = -0.2, vjust = 1.4,
    size = 4.5, parse = TRUE, show.legend = FALSE
  ) +
  scale_color_manual(values = color_palette) +
  facet_wrap(~ tenor, scales = "free") +
  labs(
    x = "Synthetic SD  (t = 1)",
    y = "Synthetic SD  (t = 0)"
  ) +
  theme_paper +
  theme(strip.text = element_text(size = 14, face = "bold"))

ggsave(file.path(output_dir, "t0_vs_t1_scatter.pdf"),
       p3, dpi = 320, width = 12, height = 5, bg = "white")
cat("Saved: t0_vs_t1_scatter.pdf\n")

cat("\n", strrep("=", 80), "\n")
cat("All figures saved to:", output_dir, "\n")
cat(strrep("=", 80), "\n\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
