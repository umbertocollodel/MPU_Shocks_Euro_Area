# IS Novelty measure following Ehrmann & Talmi (2020)
# Introductory statements only; TF-IDF cosine distance between consecutive IS.
# Includes structural-break test at July 2021 (IS → Monetary Policy Statement).

pacman::p_load(
  tidyverse, quanteda, quanteda.textstats, SnowballC,
  strucchange, showtext, ggplot2, crayon
)

font_add("Segoe UI Light", "C:/Windows/Fonts/segoeuil.ttf")
showtext_auto()

is_dir   <- "../intermediate_data/texts/introductory_statements/"
is_files <- list.files(is_dir, pattern = "\\.txt$", full.names = TRUE) %>% sort()

# Deduplicate by date — keep first file, warn about extras: ----
is_dates_raw <- basename(is_files) %>%
  str_remove("\\.txt$") %>%
  str_split_fixed("_", 2) %>%
  { as.Date(.[, 1]) }

dup_dates <- unique(is_dates_raw[duplicated(is_dates_raw)])
if (length(dup_dates) > 0) {
  walk(dup_dates, function(d) {
    dups <- is_files[is_dates_raw == d]
    cat(yellow(sprintf("Duplicate date %s — dropping: %s\n",
                       d, paste(basename(dups[-1]), collapse = ", "))))
  })
  is_files <- is_files[!duplicated(is_dates_raw)]
}

is_meta <- tibble(filepath = is_files) %>%
  mutate(
    stem     = basename(filepath) %>% str_remove("\\.txt$"),
    date     = as.Date(str_split_fixed(stem, "_", 2)[, 1]),
    governor = str_split_fixed(stem, "_", 2)[, 2]
  )

is_texts <- map_chr(is_files, ~ paste(readLines(.x, warn = FALSE), collapse = " "))
cat(green(sprintf("Loaded %d introductory statements (after deduplication).\n", length(is_texts))))

# Step 1: Strip boilerplate: ----

greeting_re <- "Ladies and gentlemen|Good (afternoon|morning|evening)"
rate_re <- paste(
  "Governing Council decided",
  "decided (that |to )(raise|lower|increase|reduce|keep|maintain|leave|hold|set|cut)",
  "key ECB interest rates",
  "interest rate[s]? on the (main refinancing|deposit facility|marginal lending)",
  "(main refinancing operations|deposit facility|marginal lending facility) will (be|remain)",
  "three key ECB interest rates by \\d",
  sep = "|"
)

strip_boilerplate <- function(text) {
  if (str_detect(text, regex(greeting_re, ignore_case = TRUE)))
    text <- str_replace(
      text,
      regex(paste0("^.*?(?=", greeting_re, ")"), dotall = TRUE, ignore_case = TRUE),
      ""
    )
  sentences <- str_split(text, "(?<=[.!?])\\s+")[[1]]
  paste(sentences[!str_detect(sentences, regex(rate_re, ignore_case = TRUE))], collapse = " ")
}

is_texts_stripped <- map_chr(is_texts, strip_boilerplate)

# Step 2: ECB-specific multi-word term concatenation: ----

term_map <- c(
  "pandemic emergency purchase programme"        = "pepp",
  "targeted longer-term refinancing operations"  = "tltro",
  "longer-term refinancing operations"           = "ltro",
  "transmission protection instrument"           = "tpi",
  "asset purchase programme"                     = "app",
  "governing council"                            = "governingcouncil",
  "deposit facility rate"                        = "depositfacilityrate",
  "deposit facility"                             = "depositfacility",
  "main refinancing operations"                  = "mro",
  "marginal lending facility"                    = "marginallendingfacility",
  "forward guidance"                             = "forwardguidance",
  "key ecb interest rates"                       = "keyinterestrates",
  "key interest rates"                           = "keyinterestrates",
  "interest rates"                               = "interestrates",
  "price stability"                              = "pricestability",
  "monetary policy"                              = "monetarypolicy",
  "euro area"                                    = "euroarea",
  "per cent"                                     = "percent",
  "basis points"                                 = "basispoints",
  "quantitative easing"                          = "quantitativeeasing",
  "federal reserve"                              = "federalreserve",
  "bank of england"                              = "bankofengland"
)

apply_term_map <- function(text) {
  text <- tolower(text)
  for (phrase in names(term_map)) text <- str_replace_all(text, fixed(phrase), term_map[[phrase]])
  text
}

is_texts_mapped <- map_chr(is_texts_stripped, apply_term_map)

# Step 3: Tokenise, clean, stem: ----

is_corpus <- corpus(is_texts_mapped,
                    docnames = paste0(is_meta$date, "_", is_meta$governor))

is_tokens <- is_corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove(pattern = stopwords("english")) %>%
  tokens_wordstem(language = "english")

# Step 4: DFM and TF-IDF with ln-based IDF (E&T): ----

is_dfm <- dfm(is_tokens) %>% dfm_trim(min_docfreq = 2)

is_dfm_tfidf <- dfm_tfidf(is_dfm, scheme_tf = "count", scheme_df = "inverse", base = exp(1))

cat(sprintf("DFM: %d documents × %d terms.\n", ndoc(is_dfm), nfeat(is_dfm)))

# Step 5: Consecutive cosine similarity → IS Novelty: ----

sim_mat   <- as.matrix(textstat_simil(is_dfm_tfidf, method = "cosine"))
n         <- nrow(sim_mat)
cos_tfidf <- sim_mat[cbind(2:n, 1:(n - 1))]

is_novelty_df <- is_meta %>%
  arrange(date) %>%
  mutate(
    cos_sim_tfidf    = c(NA_real_, cos_tfidf),
    is_novelty_tfidf = 1 - cos_sim_tfidf,
    governor         = factor(governor,
                              levels = c("Willem F. Duisenberg",
                                         "Jean-Claude Trichet",
                                         "Mario Draghi",
                                         "Christine Lagarde"))
  ) %>%
  select(-filepath, -stem)

# Step 6: Structural-break test at July 2021: ----
# The ECB renamed the Introductory Statement to Monetary Policy Statement in July 2021,
# altering document structure and potentially the boilerplate-stripping behaviour.

ts_clean  <- is_novelty_df %>% filter(!is.na(is_novelty_tfidf)) %>% arrange(date)
break_idx <- which(ts_clean$date >= as.Date("2021-07-22"))[1]

chow_res  <- sctest(ts_clean$is_novelty_tfidf ~ 1, type = "Chow", point = break_idx)
lm_dummy  <- lm(is_novelty_tfidf ~ I(date >= as.Date("2021-07-22")), data = ts_clean)
coef_post <- coef(summary(lm_dummy))["I(date >= as.Date(\"2021-07-22\"))TRUE", ]

cat(sprintf(
  "Structural break (Chow, July 2021): F = %.3f, p = %.4f\n  Dummy coef = %.4f (SE = %.4f, p = %.4f)\n",
  chow_res$statistic, chow_res$p.value,
  coef_post["Estimate"], coef_post["Std. Error"], coef_post["Pr(>|t|)"]
))

# Within-regime-demeaned series (carried forward regardless of significance,
# since the format change is documented and the break is large).
is_novelty_df <- is_novelty_df %>%
  mutate(post_jul2021 = date >= as.Date("2021-07-22")) %>%
  group_by(post_jul2021) %>%
  mutate(is_novelty_demeaned = is_novelty_tfidf - mean(is_novelty_tfidf, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-post_jul2021)

saveRDS(is_novelty_df, "../intermediate_data/is_novelty_df.rds")
cat(green("Saved: ../intermediate_data/is_novelty_df.rds\n"))

# Step 7: Plot IS Novelty over time, coloured by ECB president: ----

pres_colors <- c(
  "Willem F. Duisenberg" = "#4575b4",
  "Jean-Claude Trichet"  = "#74add1",
  "Mario Draghi"         = "#fc8d59",
  "Christine Lagarde"    = "#d73027"
)

is_novelty_df %>%
  filter(!is.na(is_novelty_tfidf)) %>%
  ggplot(aes(date, is_novelty_tfidf, colour = governor)) +
  geom_line(aes(group = 1), colour = "grey80", linewidth = 0.4) +
  geom_point(size = 1.8, alpha = 0.85) +
  geom_smooth(aes(group = governor),
              method = "loess", span = 0.5,
              se = FALSE, linewidth = 1.1) +
  scale_colour_manual(values = pres_colors, na.value = "grey50") +
  labs(x = NULL, y = "IS Novelty  (1 − cosine similarity)", colour = NULL) +
  theme_minimal(base_family = "Segoe UI Light") +
  theme(
    legend.position  = "bottom",
    axis.text        = element_text(size = 14),
    axis.title       = element_text(size = 16),
    legend.text      = element_text(size = 13),
    panel.grid.minor = element_blank()
  )

ggsave("../output/figures/is_novelty_timeseries.pdf",
       dpi = "retina", width = 12, height = 5, bg = "white", create.dir = TRUE)
cat(green("Saved: ../output/figures/is_novelty_timeseries.pdf\n"))

# Save term map for appendix: ----

tibble(phrase = names(term_map), replacement = unname(term_map)) %>%
  write_csv("../output/tables/is_novelty_term_map.csv")
cat(green("Saved: ../output/tables/is_novelty_term_map.csv\n"))
