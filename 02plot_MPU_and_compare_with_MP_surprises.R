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














