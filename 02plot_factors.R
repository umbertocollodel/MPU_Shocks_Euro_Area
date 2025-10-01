######## The script produces and saves all plots related to the factor 
######## decomposition.



# Figure: Monetary surprises over time: ----



# Clean for time serie plotting

time_serie_df <- df_surprises %>%
  pivot_longer(matches("Path|Timing|QE"),names_to = "Factor") %>% 
  mutate(Factor = factor(Factor, levels = c("Timing","Path","QE")))


# Plot (there is a filter for time period):


time_serie_df %>% 
  filter(year(date) >= 2015) %>% 
  ggplot(aes(date,value, fill=Factor)) +
  geom_col(width = 17) +
  labs(title="",
       y="Sd",
       x="",
       fill="",
       caption = "**Note**: The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors:
       downward-sloping (Timing), hump-shaped (Path) and upward-sloping (QE). It takes three principal components  
       from the variation in yields range (high minus low) around GovC meetings, rotates and scales them.
      **Source**: Authors' calculation, Thomson Reuters.  
      **Latest observation**: 15 June 2023.") +  
  theme_bw() +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#35BBCA","#0191B4","#D3DD18")) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16 ),
         legend.position="bottom",
         legend.text = element_text(size=14),
         strip.text = element_text(size = 20)) +
  theme(plot.caption = element_markdown(hjust = 0,size=12))

# Export: 

ggsave("output/figures/monetary_surprises_20_23.png",
       width = 19,
       height = 7,
       dpi="retina")




# Figure: Mean monetary uncertainty surprises  and monetary surprises over time (by factor) ----

time_serie_df %>%
  mutate(mean_surprise = mean(value),.by = Factor) %>% 
  ggplot(aes(date,value, fill=Factor)) +
  geom_col(width = 17) +
  geom_hline(aes(yintercept=mean_surprise)) +
  facet_wrap(~ Factor) +
  labs(title="",
       y="Sd",
       x="",
       fill="",
       caption = "**Note**: The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors:
       downward-sloping (Timing), hump-shaped (Path) and upward-sloping (QE). It takes three principal components  
       from the variation in yields range (high minus low) around GovC meetings, rotates and scales them. 
       **Source**: Authors' calculation, Thomson Reuters.  
       **Latest observation**: 15 June 2023.") +  
  theme_bw() +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#35BBCA","#0191B4","#D3DD18")) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16 ),
         legend.position="bottom",
         legend.text = element_text(size=14),
         strip.text = element_text(size = 20)) +
  theme(plot.caption = element_markdown(hjust = 0,size=12))




# Create a df for comparison btween monetary and monetary uncertainty surprises:



df <- read_rds("../PEPP_effect/intermediate_data/plot_df.rds") %>%
  filter(id == "Monetary Statement") %>% 
  setNames(c("id","date","Factor","mon_surprise")) %>%
  select(date,Factor,mon_surprise) %>% 
  filter(Factor != "Transmission") %>% 
  mutate(date = as.Date(date)) %>% 
  merge(time_serie_df,by=c("date","Factor")) %>%
  as_tibble() %>% 
  pivot_longer(mon_surprise:value, names_to = "moment",values_to = "value") %>%
  mutate(moment = case_when(moment == "mon_surprise" ~ "mon",
                            moment == "value" ~ "uncertainty"))



# df %>% 
#   ggplot(aes(moment,value, fill=Factor)) +
#   geom_col(width = 0.3,position = "dodge") +
#   labs(title="",
#        y="Standard Deviation",
#        x="",
#        fill="",
#        caption = "**Note**: The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors: downward-sloping (Timing), hump-shaped (Path) and upward-sloping (QE). It takes three principal components  
# from the variation in yields around GovC meetings (Press Release and Press Conference), rotates and scales them. An additional model employs the same methodology for the variation of 10 years spreads (IT-DE, SP-DE   
# and FR-DE) against German bunds around GovC meetings. The movements can be interpreted as similar to a 1 standard deviation movement in the reference asset for that factor (OIS 1m, OIS 1y, OIS 10y, IT-DE 10Y Spread).  
# **Source**: Authors' calculation, Bloomberg, EA-MPD (Altavilla et al., 2019)  
# **Latest observation**: 15 June 2023.") +  
#   facet_wrap(~date,scales = "free_y") +
#   theme_bw() +
#   theme(plot.caption = element_text(hjust=0)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   scale_fill_manual(values=c("#35BBCA","#0191B4","#D3DD18","#FE7A15")) +
#   theme( axis.text = element_text( size = 14 ),
#          axis.text.x = element_text( size = 20 ),
#          axis.title = element_text( size = 16 ),
#          legend.position="bottom",
#          legend.text = element_text(size=14),
#          strip.text = element_text(size = 20)) +
#   theme(plot.caption = element_markdown(hjust = 0,size=12))
  
  

# Figure: correlation between monetary and monetary uncertainty surprises -----
# (overall correlation)


# Get dates of govc

width=23

dates_cor=time_serie_df %>% 
  select(date) %>%
  filter(date <= "2023-06-15") %>% 
  unique() %>% 
  slice(-c(1:width))

# Plot:

df %>% 
  pivot_wider(names_from = moment, values_from = value) %>% 
  select(mon,uncertainty) %>% 
  mutate(mon = abs(mon)) %>% 
  rollapply(width = 24*3,  # window size
            FUN = function(x) cor(x),
            by.column = FALSE) %>% 
  as_tibble() %>% 
  select(mon2) %>% 
  rowid_to_column("date") %>%
  mutate(date = rep(dates_cor %>% pull(date),each = 3) %>% head(-2)) %>%
  mutate(label = case_when(date == "2017-10-26" ~ "Zero Lower Bound",
                           date == " 2023-02-02" ~ "Tightening Cycle",
                           date == "2021-07-22" ~ "Covid-19 Pandemic",
                           T ~ NA)) %>% 
  mutate(y_label = case_when(date == "2017-10-26" ~ -0.2,
                           date == " 2023-02-02" ~ -0.3,
                           date == "2021-07-22" ~ 0.3,
                           T ~ NA)) %>% 
  mutate(date_label = case_when(date == "2017-10-26" ~ date,
                             date == " 2023-02-02" ~ date,
                             date == "2021-07-22" ~ date,
                             T ~ NA)) %>% 
  mutate(yend_label = case_when(date == "2017-10-26" ~ mon2-0.02,
                                date == " 2023-02-02" ~ mon2 -0.02,
                                date == "2021-07-22" ~ mon2 +0.02,
                                T ~ NA)) %>% 
  ggplot(aes(date,mon2,group=1)) +
  geom_line(size=1.5, col="orange") +
  geom_smooth(method = "loess", span = 0.2, color = "red") +
  geom_hline(yintercept = 0, size =1, linetype = "dashed") +
  geom_segment(aes(x = date_label,y = y_label,xend = date_label, yend =yend_label),size=0.5) +
  geom_text(aes(x = date_label,y = y_label,label = label),hjust = 1.1,vjust = 1.1,parse = FALSE) +
  geom_point(aes(x = date_label,y = y_label),size=2,alpha=0.5) +
  labs(title="Correlation - Monetary and monetary uncertainty surprises",
       subtitle = expression(italic("2yrs rolling window")),
       col="",
       y="",
       x="",
       fill="",
       caption = "**Note**: 
             **Source**: Authors' calculation   
             **Latest observation**: 15 June 2023.") +
  ylim(-0.4,0.4) +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0)) +
  theme(title = element_text(size=20)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 20 ),
        axis.title = element_text( size = 16, face = "bold" ),
        legend.text = element_text(size=14),
        # The new stuff
        strip.text = element_text(size = 20)) +
  theme(plot.caption = element_markdown(hjust = 0,size=12))





# Figure: rolling correlation by factor

df %>% 
  pivot_wider(names_from = moment, values_from = value) %>% 
  split(.$Factor) %>%
  discard(~ nrow(.x) == 0) %>% 
  map(~ .x %>% select(mon,uncertainty)) %>% 
  map(~ .x %>% mutate_all(~ (.x - mean(.x))/sd(.x))) %>%
  map(~ .x %>% mutate(mon = abs(mon))) %>% 
  map(~ .x %>% rollapply(width = 24,  # window size
    FUN = function(x) cor(x),
    by.column = FALSE)
  ) %>% 
  map(~ .x %>% as_tibble() %>% select(mon2)) %>% 
  map(~.x %>% cbind(dates_cor)) %>% 
  bind_rows(.id = "Factor") %>%
  mutate(Factor = factor(Factor,levels = c("Timing","Path","QE"))) %>% 
  ggplot(aes(date,mon2,group=1)) +
        geom_line(size=1.5, col="orange") +
        geom_hline(yintercept = 0, size =1, linetype = "dashed") +
        facet_wrap(~ Factor, scales = "free") +
        labs(title="Rolling correlation - Monetary and uncertainty surprises",
             subtitle = expression(italic("2yrs")),
             col="",
             y="",
             x="",
             fill="",
             caption = "**Note**: 
             **Source**: Authors' calculation   
             **Latest observation**: 15 June 2023.") +  
        theme_bw() +
        theme(plot.caption = element_text(hjust=0)) +
        theme(title = element_text(size=20)) +
        theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(axis.text = element_text( size = 14 ),
               axis.text.x = element_text( size = 20 ),
               axis.title = element_text( size = 16, face = "bold" ),
               legend.text = element_text(size=14),
               # The new stuff
               strip.text = element_text(size = 20)) +
        theme(plot.caption = element_markdown(hjust = 0,size=12))


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

ggsave("../output/figures/comparison_surprises.png",
       width = 6,
       height = 3.5,
       dpi="retina")
  
  
# Figure: correlation comparison ----

df_comparison %>%
  split(.$tenor) %>% 
  map(~ .x %>% pivot_wider(names_from = type,values_from = value)) %>%
  map(~ .x[,3:4]) %>% 
  map(~ .x %>% mutate(Monetary_abs = abs(Monetary))) %>% 
  map(~ rcorr(as.matrix(.x))) %>% 
  map(~ round(.x$P,2)) 
  
  
  

  df_comparison %>%
    split(.$tenor) %>% 
    map(~ .x %>% pivot_wider(names_from = type,values_from = value)) %>%
    map(~ .x[,3:4]) %>% 
    map(~ .x %>% mutate(Monetary_abs = abs(Monetary))) %>% 
    map(~ .x %>% correlate()) %>%
    map(~ .x %>% select(term, `Monetary Uncertainty`) %>%  filter(term %in% c("Monetary", "Monetary_abs"))) %>% 
    bind_rows(.id = "tenor") %>% 
    mutate(term = case_when(term == "Monetary_abs" ~ "|Monetary|",
                            T ~ term)) %>% 
    mutate(term = factor(term, levels = c("Monetary","|Monetary|"))) %>% 
    mutate(tenor = factor(tenor,levels = c("3mnt","6mnt","1Y","2Y","5Y","10Y"))) %>% 
    mutate(significant = 
             case_when(term == "|Monetary|" & tenor %in% c("6mnt","2Y","10Y") ~ "**",
                       term == "Monetary" & tenor %in% c("2Y") ~ "*",
                       term == "|Monetary|" & tenor %in% c("5Y") ~ "***",
                       T ~ "")
    ) %>% 
    ggplot(aes(tenor,`Monetary Uncertainty`)) +
    geom_col(width = 0.3, fill = "#F8766D",alpha=0.8) +
    geom_text(aes(y=`Monetary Uncertainty`+0.1,label = paste0(round(`Monetary Uncertainty`,2),significant)),size=4.5) +
    ylim(-1,1) +
    facet_wrap(~ term) +
  labs(title="",
       col="",
       y="",
       x="",
       fill="",
       caption = "") +
  theme_bw() +
  theme(text=element_text(family="Segoe UI Light")) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16, face = "bold" ),
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
  theme(plot.caption = element_text(hjust = 0,size=12))
    
# Export:

ggsave("../output/figures/correlation_surprises.png",
       dpi = 320,
       width = 12, # Wider to match second script's direction plot
       height = 9, # Adjusted height
       bg="white")


# Figure: 10 year mpu over time: ----


differences_df %>% 
  filter(tenor == "10Y") %>% 
  ggplot(aes(date,diff_3)) +
  geom_col() +
  labs(title="",
       col="",
       y="Basis Points",
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

# Figure: Mpu over time (all tenors): ----
library(dplyr)
library(ggplot2)

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

ggsave("output/figures/mpu_all.png",
       width = 5,
       height = 2.5,
       dpi="retina")


#Table: top 5 increases/decreases -----


differences_df %>% 
  filter(tenor == "10Y") %>%
  arrange(-diff) %>% 
  slice(c(1:5)) %>% 
  select(date,diff) %>% 
  mutate(date = as.character(date)) %>% 
  mutate(diff = round(diff,2)) %>% 
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
            out = "output/tables/top_5_mpu.tex")

differences_df %>% 
  filter(tenor == "10Y") %>%
  arrange(diff) %>% 
  slice(c(1:5)) %>% 
  select(date,diff) %>%
  mutate(date = as.character(date)) %>% 
  mutate(diff = round(diff,2)) %>% 
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
            out = "output/tables/bottom_5_mpu.tex")


# Table: share of increaseas and decreases (%) by tenor

differences_df %>%
  group_by(tenor) %>% 
  summarise(positive = sum(diff >0 ),negative = sum(diff < 0)) %>%
  mutate(total = positive + negative, 
         share_positive = round(positive/(positive + negative)*100,1)) %>%
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))) %>%
  .[order(.$tenor),] %>% 
  select(tenor,total, everything()) %>% 
  setNames(c("Tenor","N.obs.","MPU>0","MPU<0","MPU>0 (%)")) %>% 
  mutate(Tenor = as.character(Tenor)) %>% 
  stargazer(summary = FALSE,
            rownames=F,
            out = "output/tables/share_increases.tex")














