########### Description: script to plot monetary policy surprises and monetary policy uncertainty, figures and tables


# Figure: monetary and monetary uncertainty surprises over time (by tenor) ------

df_comparison <- read_excel("../raw_data/00EA_MPD_update_june2025.xlsx",sheet = 4) %>%
  mutate(date = {
    # Convert to character to unify everything
    date_char <- as.character(date)
    
    # Now parse based on the character representation
    map_chr(date_char, function(x) {
      if (is.na(x)) return(NA_character_)
      if (str_detect(x, "/")) {
        # Format like "14/12/2023"
        as.character(dmy(x))
      } else if (str_detect(x, "^[0-9]{5}$")) {
        # Excel serial number like "44812"
        as.character(as.Date(as.numeric(x), origin = "1899-12-30"))
      } else if (str_detect(x, "^[0-9]{4}-[0-9]{2}-[0-9]{2}")) {
        # Already YYYY-MM-DD
        x
      } else {
        NA_character_
      }
    }) %>% as.Date()
  }) |> 
  select(date, OIS_3M,OIS_6M,OIS_1Y,OIS_2Y,OIS_5Y,OIS_10Y) %>% 
  setNames(c("date","3mnt","6mnt","1Y","2Y","5Y","10Y")) %>%
  pivot_longer(`3mnt`:`10Y`,names_to = "tenor",values_to = "monetary") %>% 
  inner_join(differences_df %>% select(date,tenor,diff_3)) %>% 
  pivot_longer(monetary:diff_3,names_to = "type",values_to = "value") %>% 
  mutate(tenor = factor(tenor,levels=c("3mnt","6mnt","1Y","2Y","5Y","10Y"))) %>%
  mutate(type = case_when(type == "monetary" ~ "Monetary",
                          type == "diff_3" ~ "Monetary Uncertainty"))

# Plot:


df_comparison %>% 
  ggplot(aes(date,value,col=type)) +
  geom_line(size=0.8) +
  facet_wrap(~tenor) +
  labs(title="",
       col="",
       y="Bps",
       x="",
       fill="",
       caption = "") +
  theme_bw() +
  theme(text=element_text(family="Segoe UI Light")) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5,angle =90)) +
  theme( axis.text = element_text( size = 18 ),
         axis.title = element_text( size = 20),
         legend.text = element_text(size=16),
         # The new stuff
         strip.text = element_text(size = 18)) +
  theme(legend.position = "bottom")
  
# Export:

ggsave("../output/figures/comparison_surprises.pdf",
       dpi = 320,
       width = 12, # Wider to match second script's direction plot
       height = 9, # Adjusted height
       bg="white")
  
  
# Figure: correlation betwen MP and MPU ----

df_comparison %>%
  split(.$tenor) %>% 
  map(~ .x %>% pivot_wider(names_from = type, values_from = value)) %>%
  map(~ .x[,3:4]) %>% 
  map(~ .x %>% mutate(Monetary_abs = abs(Monetary))) %>% 
  map(~ .x %>% correlate()) %>%
  map(~ .x %>% select(term, `Monetary Uncertainty`) %>% filter(term %in% c("Monetary", "Monetary_abs"))) %>% 
  bind_rows(.id = "tenor") %>% 
  mutate(term = case_when(
    term == "Monetary_abs" ~ "Size of MP Surprise",
    TRUE ~ "Direction of MP Surprise"
  )) %>% 
  mutate(term = factor(term, levels = c("Direction of MP Surprise", "Size of MP Surprise"))) %>% 
  mutate(tenor = factor(tenor, levels = c("3mnt","6mnt","1Y","2Y","5Y","10Y"))) %>% 
  mutate(significant = 
           case_when(term == "Size of MP Surprise" & tenor %in% c("6mnt","2Y","10Y") ~ "**",
                     term == "Direction of MP Surprise" & tenor %in% c("2Y") ~ "*",
                     term == "Size of MP Surprise" & tenor %in% c("5Y") ~ "***",
                     TRUE ~ "")
  ) %>% 
  ggplot(aes(tenor, `Monetary Uncertainty`)) +
  geom_col(width = 0.3, fill = "#F8766D", alpha = 0.8) +
  geom_text(aes(y = `Monetary Uncertainty` + 0.1, label = paste0(round(`Monetary Uncertainty`, 2), significant)), size = 6) +
  ylim(-1, 1) +
  facet_wrap(~ term) +
  labs(title = "", y = "", x = "", caption = "") +
  theme_bw() +
  theme(text = element_text(family = "Segoe UI Light"),
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 20))
# Export:

ggsave("../output/figures/correlation_surprises.pdf",
       dpi = 320,
       width = 12, # Wider to match second script's direction plot
       height = 9, # Adjusted height
       bg="white")


# Figure: 10 year MPU over time: -----


differences_df %>% 
  filter(tenor == "10Y") %>% 
  ggplot(aes(date,diff_3)) +
  geom_col() +
  labs(title="",
       col="",
       y="Bps",
       x="",
       fill="",
       caption = "") +
  theme_bw() +
  theme(text=element_text(family="Segoe UI Light")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16),
         legend.text = element_text(size=14),
         # The new stuff
         strip.text = element_text(size = 20)) +
  theme(legend.position = "bottom") +
  theme(
    plot.title = element_text(
      size = 20,            # Set font size
      face = "bold", # Make the title bold and italic
      color = "black",
      family = "Segoe UI Light")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme(plot.caption = element_text(hjust = 0,size=12))
  

# Export:

ggsave("../output/figures/mpu.pdf",
       dpi = 320,
       width = 12, # Wider to match second script's direction plot
       height = 9, # Adjusted height
       bg="white")

# Figure: Monetary Policy Uncertainty (MPU) over time, by tenor -------

differences_df %>% 
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))) %>%
  ggplot(aes(x = date, y = diff_3)) +
  geom_col(fill = "#2E75B6") +   # nice muted blue
  facet_wrap(~ tenor, scales = "free_y", ncol = 2) +
  labs(
    title = "",
    y = "Bps",
    x = "",
    caption = ""
  ) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme_bw() +
  theme(
    text = element_text(family = "Segoe UI Light"),
    axis.text.x = element_text(size = 16, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 18),
    strip.text = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 12),
    panel.spacing = unit(1, "lines")
  )

# Export to PDF
ggsave(
  "../output/figures/mpu_all_tenors.pdf",
  dpi = 320,
  width = 14,    # wider for multi-panel layout
  height = 10,
  bg = "white"
)

# Table: correlation matrix of MPU across tenors -------
# Compute correlation matrix of MPU across tenors 
correlation_table <- differences_df %>%
  select(date, tenor, diff_3) %>%
  pivot_wider(names_from = tenor, values_from = diff_3) %>%
  select(-date) %>%
  cor(use = "pairwise.complete.obs")

# Round for readability
correlation_table_rounded <- round(correlation_table, 2)

# Convert to LaTeX with nice formatting
latex_table <- xtable(
  correlation_table_rounded,
  caption = "Correlation Matrix of Monetary Policy Uncertainty (MPU) across Tenors",
  label = "tab:mpu_correlation",
  align = c("l", rep("c", ncol(correlation_table_rounded)))
)

# Add booktabs style and save to file
print(
  latex_table,
  type = "latex",
  file = "../output/tables/mpu_correlation.tex",
  include.rownames = TRUE,
  booktabs = TRUE,
  caption.placement = "top"
)

# Figure: post-conference volatility for 3m, 2Y and 10Y: (LLM paper target var) -------


# Remove outliers using IQR method per tenor
filtered_df <- differences_df %>%
  filter(tenor %in% c("3mnt", "2Y", "10Y")) %>%
  group_by(tenor) %>%
  filter(correct_post_mean_3 < quantile(correct_post_mean_3, 0.9)) %>%
  ungroup() %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "2Y", "10Y")))


# Plot
ggplot(filtered_df, aes(date, correct_post_mean_3, col = tenor)) +
  geom_col() +
  labs(title = "",
       col = "",
       y = "Basis Points",
       x = "",
       fill = "",
       caption = "") +
  facet_wrap(~ tenor, scales = "free_y", nrow = 1) +
  scale_color_manual(values = c("10Y" = "#d73027", "2Y" = "#4575b4", "3M" = "#91bfdb"), guide = "none") +
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
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 12)
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

ggsave("../output/figures/mpu_all.png",
       width = 5,
       height = 2.5,
       dpi="retina")


 #Table: top 5 increases/decreases MPU -----


differences_df %>% 
  filter(tenor == "10Y") %>%
  arrange(-diff_3) %>% 
  slice(c(1:5)) %>% 
  select(date,diff_3) %>% 
  mutate(date = as.character(date)) %>% 
  mutate(diff_3 = round(diff_3,2)) %>% 
  mutate(`Decision` = c(
    "ECB decided to keep interest rates unchanged.",
    "ECB announced a series of measures including a cut in the deposit rate by 10 basis points to -0.30% and an extension of the asset purchase programme (APP)",
    "ECB unexpectedly cut interest rates by 25 basis points.",
    "ECB announced the end of net asset purchases under the asset purchase programme (APP) and signaled future interest rate hikes",
    "ECB launched an expanded asset purchase programme (APP) and maintained low interest rates."),
  ) %>% 
  mutate(Description = c(
   "The Governing Council expressed significant concerns about the risks to the economic outlook, particularly due to the sovereign debt crisis in the Eurozone. President Jean-Claude Trichet emphasized the heightened uncertainty and the need for strong vigilance.",
   "President Mario Draghi’s announcements were seen as less aggressive than expected, leading to increased uncertainty about the ECB’s commitment to combat low inflation.",
   "In his first meeting as ECB President, Mario Draghi emphasized the need for decisive action to support the economy, leading to a rate cut. This shift in policy direction created uncertainty about the future path of monetary policy under new leadership.",
   "The ECB’s decision to end net asset purchases and signal future rate hikes is a response to rising inflation. This shift towards tightening monetary policy has raised questions about the timing and pace of rate hikes and their impact on economic recovery.",
   "The announcement of such a large-scale asset purchase programme and the commitment to maintaining low interest rates until inflation approached the target added to market uncertainty about the long-term effects of these measures.")) %>% 
  setNames(c("GovC Date","MPU Surprise","Decision","Description")) %>% 
  stargazer(summary = FALSE,
            rownames = FALSE,
            out = "../output/tables/top_5_mpu.tex")

differences_df %>% 
  filter(tenor == "10Y") %>%
  arrange(diff_3) %>% 
  slice(c(1:5)) %>% 
  select(date,diff_3) %>%
  mutate(date = as.character(date)) %>% 
  mutate(diff_3 = round(diff_3,2)) %>% 
    mutate(`Decision` = c(
      "ECB increased the three key interest rates by 50 basis points. This decision was aimed at ensuring the timely return of inflation to the 2% medium-term target.",
      "ECB kept interest rates unchanged but signaled a shift towards less accommodative monetary policy. President Mario Draghi mentioned that the urgency for further actions had diminished.",
      "ECB confirmed the end of its net asset purchases under the asset purchase programme (APP) by the end of December 2018.",
      "ECB signaled potential future rate cuts and a resumption of asset purchases if inflation did not move towards its target.",
      "ECB kept interest rates unchanged but emphasized its readiness to act if necessary to support the euro area economy."),
      ) %>%
    mutate(`Description` = c(
  "Christine Lagarde emphasized the resilience of the euro area banking sector and the ECB’s readiness to provide liquidity support if needed. She also highlighted that the elevated level of uncertainty reinforced the importance of a data-dependent approach to policy rate decisions.",
  "Draghi’s comments about the reduced urgency for additional measures provided clarity and reduced market uncertainty.",
  "Draghi highlighted the ECB’s confidence in the sustained convergence of inflation to its target, which reduced uncertainty.",
  "Draghi’s clear communication about the ECB’s readiness to act helped reduce market uncertainty.",
  "Draghi’s reassurances about the ECB’s commitment to maintaining price stability and supporting the economy reduced uncertainty."),
    ) %>% 
  setNames(c("GovC Date","MPU Surprise","Decision","Description")) %>% 
  stargazer(summary = FALSE,
            rownames=F,
            out = "../output/tables/bottom_5_mpu.tex")


# Table: share of increaseas and decreases (%) by tenor ------

differences_df %>%
  group_by(tenor) %>% 
  summarise(positive = sum(diff_3 >0 ),negative = sum(diff_3 < 0)) %>%
  mutate(total = positive + negative, 
         share_positive = round(positive/(positive + negative)*100,1)) %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))) %>%
  .[order(.$tenor),] %>% 
  select(tenor,total, everything()) %>% 
  setNames(c("Tenor","N.obs.","MPU>0","MPU<0","MPU>0 (%)")) %>% 
  mutate(Tenor = as.character(Tenor)) %>% 
  stargazer(summary = FALSE,
            rownames=F,
            out = "../output/tables/share_increases.tex")

# Table: summary statistics of MPU by tenor -------

differences_df %>%
  group_by(tenor) %>%
  summarise(
    N = n(),
    Mean = round(mean(diff_3, na.rm = TRUE), 2),
    SD = round(sd(diff_3, na.rm = TRUE), 2),
    Min = round(min(diff_3, na.rm = TRUE), 2),
    `1st Qu.` = round(quantile(diff_3, 0.25, na.rm = TRUE), 2),
    Median = round(median(diff_3, na.rm = TRUE), 2),
    `3rd Qu.` = round(quantile(diff_3, 0.75, na.rm = TRUE), 2),
    Max = round(max(diff_3, na.rm = TRUE), 2)
  ) %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))) %>%
  .[order(.$tenor),] %>%
  select(tenor, N, Mean, Median, SD) %>%
  setNames(c("Tenor", "N.obs.", "Mean", "Median","SD")) %>%
  mutate(Tenor = as.character(Tenor)) %>%
  stargazer(summary = FALSE,
            rownames = FALSE,
            out = "../output/tables/summary_statistics_mpu.tex")


# Section: CESIUSD vs MPU Analysis -------

# Install and load RefinitivR if not available
if (!require("RefinitivR", quietly = TRUE)) {
  if (!require("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("GreenGrassBlueOcean/RefinitivR")
}
library(Refinitiv)

# Create custom helper functions

connect_to_workspace <- function(api_key) {
  message("Connecting to LSEG Workspace...")
  message("Note: LSEG Workspace desktop app must be running and logged in.")

  tryCatch({
    eikon <- EikonConnect(
      Eikonapplication_id = api_key,
      PythonModule = "JSON"
    )
    message("Successfully connected to LSEG Workspace!")
    return(eikon)
  }, error = function(e) {
    stop(paste(
      "Failed to connect to LSEG Workspace.",
      "Ensure Workspace is running and you have a valid API key.",
      "Error:", e$message
    ))
  })
}

try_download_ric <- function(ric, eikon_obj) {
  raw_data <- tryCatch({
    rd_GetHistoricalPricing(
      universe = ric,
      interval = "P1D",
      count = 15000L,
      RDObject = eikon_obj
    )
  }, error = function(e) {
    return(NULL)
  })

  # Check if data is valid (not NULL, has rows, and doesn't contain error messages)
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(NULL)
  }

  # Check for error response in data
  col_names <- tolower(names(raw_data))
  if (any(grepl("error|code|message", col_names))) {
    # Check if it's actually an error response
    if (nrow(raw_data) <= 2 && any(grepl("not found|error", tolower(as.character(raw_data)), ignore.case = TRUE))) {
      return(NULL)
    }
  }

  return(raw_data)
}


# Download CESIUSD data from Refinitiv
# .CESIUSD is the Citi Economic Surprise Index for Eurozone

# First establish connection to LSEG Workspace
# NOTE: Set your API key here (get it from Workspace: App Key Generator)
api_key <- Sys.getenv("REFINITIV_API_KEY")  # Or set directly: "your-api-key-here"

eikon <- connect_to_workspace(api_key)

# Download CESIUSD using the connection
CESIUSD_raw <- try_download_ric(".CESIUSD", eikon)

if (is.null(CESIUSD_raw)) {
  warning("Failed to download .CESIUSD - trying alternative RIC: EURCESI=CI")
  CESIUSD_raw <- try_download_ric("EURCESI=CI", eikon)
}

if (is.null(CESIUSD_raw)) {
  stop("Could not download CESIUSD data. Please check your Refinitiv connection and RIC code.")
}

# Clean and prepare CESIUSD data
# Column names may vary: CLOSE, TRDPRC_1, VALUE, etc.
CESIUSD_df <- CESIUSD_raw %>%
  mutate(date = as.Date(Date)) %>%
  {
    # Find the value column (could be CLOSE, TRDPRC_1, VALUE, etc.)
    value_col <- intersect(names(.), c("CLOSE", "TRDPRC_1", "VALUE", "close", "value"))
    if (length(value_col) == 0) {
      # If no standard column found, use second column (first is usually Date)
      value_col <- names(.)[2]
    }
    select(., date, CESIUSD = all_of(value_col[1]))
  } %>%
  filter(!is.na(CESIUSD)) %>%
  mutate(CESIUSD = as.numeric(CESIUSD)) %>%
  arrange(date)

cat("\n=== CESIUSD Data Downloaded ===\n")
cat("Observations:", nrow(CESIUSD_df), "\n")
cat("Date range:", as.character(min(CESIUSD_df$date)), "to",
    as.character(max(CESIUSD_df$date)), "\n")

# Align CESIUSD to ECB Governing Council dates
# Get unique ECB dates from MPU data
ecb_dates <- differences_df %>%
  distinct(date) %>%
  arrange(date)

# Merge CESIUSD with ECB dates (exact date matching)
CESIUSD_aligned <- ecb_dates %>%
  left_join(CESIUSD_df, by = "date") %>%
  filter(!is.na(CESIUSD))

# Create combined dataset: MPU (diff_3) + CESIUSD for all tenors
mpu_CESIUSD_df <- differences_df %>%
  select(date, tenor, diff_3) %>%
  inner_join(CESIUSD_aligned %>% select(date, CESIUSD), by = "date") %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

cat("\n=== MPU-CESIUSD Merged Dataset ===\n")
cat("Total observations:", nrow(mpu_CESIUSD_df), "\n")
cat("Date range:", as.character(min(mpu_CESIUSD_df$date)), "to",
    as.character(max(mpu_CESIUSD_df$date)), "\n")
cat("Observations per tenor:\n")
print(table(mpu_CESIUSD_df$tenor))

# Calculate correlations between CESIUSD and MPU (diff_3) by tenor
cor_CESIUSD_mpu <- mpu_CESIUSD_df %>%
  split(.$tenor) %>%
  map_dfr(~ {
    pearson_test <- cor.test(.x$diff_3, .x$CESIUSD, method = "pearson")

    tibble(
      tenor = unique(.x$tenor),
      n_obs = nrow(.x),
      pearson = pearson_test$estimate,
      pearson_pval = pearson_test$p.value
    )
  }) %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

# Add significance stars
cor_CESIUSD_mpu <- cor_CESIUSD_mpu %>%
  mutate(
    pearson_sig = case_when(
      pearson_pval < 0.01 ~ "***",
      pearson_pval < 0.05 ~ "**",
      pearson_pval < 0.10 ~ "*",
      TRUE ~ ""
    )
  )

cat("\n=== Correlation: CESIUSD vs MPU (diff_3) by Tenor ===\n")
print(cor_CESIUSD_mpu)

# Figure: Scatter plot of MPU vs CESIUSD by tenor ------

# Create labels for correlation annotation
cor_labels <- cor_CESIUSD_mpu %>%
  mutate(label = paste0("r = ", round(pearson, 2), pearson_sig))

mpu_CESIUSD_df %>%
  ggplot(aes(x = CESIUSD, y = diff_3)) +
  geom_point(size = 2.5, alpha = 0.5, color = "#4575b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#d73027", fill = "#d73027",
              alpha = 0.15, linewidth = 1) +
  geom_text(data = cor_labels,
            aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.5, size = 5,
            color = "grey30", family = "Segoe UI Light",
            inherit.aes = FALSE) +
  facet_wrap(~ tenor, scales = "free", nrow = 2) +
  labs(
    title = "",
    x = "Citi Economic Surprise Index (USD)",
    y = "MPU Surprise (Bps)",
    caption = ""
  ) +
  theme_bw() +
  theme(text = element_text(family = "Segoe UI Light")) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 16)) +
  theme(axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 18, face = "bold"),
        panel.border = element_rect(colour = "grey80", fill = NA))

# Export scatter plot
ggsave("../output/figures/CESIUSD_vs_mpu_scatter.pdf",
       dpi = 320,
       width = 14,
       height = 10,
       bg = "white")


