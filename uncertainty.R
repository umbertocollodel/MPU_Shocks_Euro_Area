####### Script to produce an MPU index based on OIS rates around ECB Governing Council meetings:

# Prepare environment: ----- 

# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# Load all packages at once
pacman::p_load(
  padr,
  DescTools,
  readxl,
  tidyverse,
  zoo,
  corrr,
  stargazer,
  Hmisc,
  broom
)

# Enable Segoe UI font (ensure it's installed on your system)
# Check if "Segoe UI" font is available, if not, add it
if (!("Segoe UI" %in% font_families())) {
  # Adjust this path if 'segoeui.ttf' is not in the specified location
  font_add("Segoe UI", regular = "C:/Users/collodelu/OneDrive - centralbankmalta.org/Desktop/Projects/Uncertainty_surprises/code/segoeui.ttf")
}
showtext_auto()

#### Custom functions: ---------

calculate_lags <- function(df, var, lags){
  map_lag <- lags %>% map(~partial(lag, n = .x))
  return(df %>% mutate(across(.cols = {{var}}, .fns = map_lag, .names = "{.col}_lag{lags}")))
}

calculate_leads <- function(df, var, leads){
  map_leads <- leads %>% map(~partial(lead, n = .x))
  return(df %>% mutate(across(.cols = {{var}}, .fns = map_leads, .names = "{.col}_lead{leads}")))
}


# Set parameters: 

names_sheets=excel_sheets("../raw_data/OIS.xls")

tenors=names_sheets %>% 
  str_remove("_.*")


##### Clean daily data for OIS: -----

df_list <- names_sheets %>% 
  map(~ read_excel("../raw_data/daily_OIS_updated15Sept_2025..xls", sheet = .x)) %>%
  map(~.x %>% select(1:5)) %>% 
  map(~ .x |> rename(Daily = Timestamp)) %>%
  map(~ .x %>% mutate(Daily = as.Date(Daily))) %>% 
  map(~ .x %>% setNames(c("daily","high","low","first","last"))) %>% 
  map(~ .x %>% mutate_at(2:ncol(.),as.numeric)) |> 
  map(~ .x %>% arrange(daily)) 
  
  
# Pad the dataframe (otherwise govc_id are staggered): 


final_date_to_pad=df_list %>% 
  map(~ .x %>% 
  .$daily %>%  
  .[1])

  
df_list <- df_list %>% 
  map(~.x %>% 
  add_row(daily = as.Date("1999-01-03")) %>% 
  arrange(daily)) %>%
  map2(final_date_to_pad, ~ .x %>% filter(daily <= .y)) %>% 
  map(~ .x %>% pad("day")) %>%
  map2(df_list,~ rbind(.x,.y))
  
  
  

  

# Retrieve Governing Council dates (updated until September 2024)


dates=read_xlsx("../raw_data/dates_govc.xlsx") %>% 
  unite("date",day:year,sep="-",remove = T) %>% 
  mutate(date = as.Date(date,"%d-%m-%Y")) %>% 
  .$date
  

###### Calculate difference between min-max range pre-post govc: ---------

df_list_clean <- df_list %>% 
  map(~ {
    # ============================================================================
    # STEP 1: Basic setup - mark GovC days and calculate daily gap
    # ============================================================================
    df <- .x %>%
      mutate(
        govc = ifelse(daily %in% dates, 1, 0),      # Mark GovC announcement days
        gap = high - low,                            # Daily high-low range (proxy for volatility)
        sum_govc = cumsum(govc),                     # Cumulative count of GovC meetings
        sum_govc = ifelse(govc == 1, sum_govc, 0)   # Keep count only on GovC days, else 0
      ) %>%
      calculate_lags(sum_govc, 1:7) %>%              # Create lag_1 through lag_7 of sum_govc
      calculate_leads(sum_govc, 1:7) %>%             # Create lead_1 through lead_7 of sum_govc
      mutate(govc_id = rowSums(select(., starts_with("sum_")), na.rm = TRUE))  
      # govc_id groups days around same GovC meeting (all lags/leads have same ID)
    
    # ============================================================================
    # STEP 2: Define all window specifications
    # ============================================================================
    # Key: window name (e.g., "3" = 3-day symmetric, "1_5" = 1 day before, 5 after)
    # Value: list with 'pre' = days before GovC, 'post' = days after GovC
    windows <- list(
      "1"   = list(pre = 1, post = 1),   # 1-day symmetric
      "2"   = list(pre = 2, post = 2),   # 2-day symmetric
      "3"   = list(pre = 3, post = 3),   # 3-day symmetric (baseline)
      "5"   = list(pre = 5, post = 5),   # 5-day symmetric
      "7"   = list(pre = 7, post = 7),   # 7-day symmetric
      "1_5" = list(pre = 1, post = 5),   # Asymmetric: short anticipation, long absorption
      "2_4" = list(pre = 2, post = 4),   # Asymmetric: moderate anticipation, longer absorption
      "4_2" = list(pre = 4, post = 2)    # Asymmetric: long anticipation, short absorption
    )
    
    # ============================================================================
    # STEP 3: Calculate MPU for each window specification
    # ============================================================================
    for (w in names(windows)) {
      pre_days <- windows[[w]]$pre
      post_days <- windows[[w]]$post
      
      df <- df %>%
        # Create indicators for pre/post windows
        # Reduce with `|` creates: lag(govc,1)==1 | lag(govc,2)==1 | ... | lag(govc,post_days)==1
        # ifelse ensures we get numeric 1/0 instead of logical TRUE/FALSE
        mutate(
          # post_govc = 1 if any of the next 'post_days' has govc == 1
          post_govc = ifelse(Reduce(`|`, map(1:post_days, ~lag(govc, .x) == 1)), 1, 0),
          # pre_govc = 1 if any of the previous 'pre_days' has govc == 1  
          pre_govc = ifelse(Reduce(`|`, map(1:pre_days, ~lead(govc, .x) == 1)), 1, 0)
        ) %>%
        
        # Calculate mean gap in the PRE window for this govc_id
        group_by(govc_id, pre_govc) %>%
        mutate(pre_mean = mean(gap, na.rm = TRUE)) %>%
        
        # Calculate mean gap in the POST window for this govc_id
        group_by(govc_id, post_govc) %>%
        mutate(post_mean = mean(gap, na.rm = TRUE)) %>%
        ungroup() %>%
        
        # Shift means to align with GovC day (day 0)
        # lead(post_mean, 1) gets the post-window mean from the perspective of GovC day
        # lag(pre_mean, 1) gets the pre-window mean from the perspective of GovC day
        mutate(
          !!paste0("correct_post_mean_", w) := lead(post_mean, 1),
          !!paste0("correct_pre_mean_", w) := lag(pre_mean, 1),
          # Calculate difference and convert to basis points (*100)
          !!paste0("diff_", w) := (lead(post_mean, 1) - lag(pre_mean, 1)) * 100
        ) %>%
        
        # Clean up temporary variables to avoid conflicts in next iteration
        select(-post_govc, -pre_govc, -post_mean, -pre_mean)
    }
    
    # ============================================================================
    # STEP 4: Keep only GovC announcement days with complete data
    # ============================================================================
    df %>%
      distinct(sum_govc, .keep_all = TRUE) %>%       # One row per GovC meeting
      filter(!is.na(gap)) %>%                         # Remove any missing data
      select(govc_id, starts_with("correct"), starts_with("diff_"))
  })


# Add back govc dates:

dates_format= dates %>% 
    data.frame(date=.) %>% 
    mutate(govc_id = seq_len(nrow(.)))


final_df_list <- df_list_clean %>% 
    map(~ .x %>% left_join(dates_format))


# Plot result: ------

final_df_list[[2]] %>%
  slice(-1) %>% 
  ggplot(aes(date,diff_1)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Winsorize outliers and highlight "big" spike in baseline MPU index (3-days pre-post): ---

# Winsorize all diff variables before analysis
differences_df <- final_df_list %>% 
  map(~ .x %>% filter(!is.na(diff_3))) %>% 
  # Remove rows with ANY NA in diff columns before winsorizing
  map(~ .x %>% filter(if_all(starts_with("diff_"), ~!is.na(.x)))) %>%
  # Winsorize all diff variables
  map(~ .x %>% mutate(across(starts_with("diff_"), 
                              ~DescTools::Winsorize(.x)))) %>%
  # Calculate thresholds and spikes based on winsorized diff_3
  map(~ .x %>% mutate(
    up_threshold = mean(diff_3, na.rm = TRUE) + 1.5 * sd(diff_3, na.rm = TRUE),
    low_threshold = mean(diff_3, na.rm = TRUE) - 1.5 * sd(diff_3, na.rm = TRUE),
    spike = if_else(diff_3 >= up_threshold | diff_3 <= low_threshold, 1, 0)
  )) %>% 
  set_names(tenors) %>% 
  bind_rows(.id = "tenor")




differences_df %>% 
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))) %>%
  ggplot(aes(date, diff_3)) +
  geom_col() +
  facet_wrap(~tenor, scales = "free_y") +
  #geom_vline(data = differences_df %>% filter(spike == 1 & diff >= 0), aes(xintercept = date), color = "red", size = 2, alpha = 0.3) +
  #geom_vline(data = differences_df %>% filter(spike == 1 & diff <= 0), aes(xintercept = date), color = "blue", size = 2, alpha = 0.3) +
  theme_minimal() +
  labs(y = "Bps (difference between pre and post GovC)", x = "") +
  scale_x_date(date_breaks = "18 weeks") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Plot: correlation alternative MPU measures: -----

# Filter to only symmetric short windows and calculate correlations
baseline_cor_plot <- differences_df %>% 
  split(.$tenor) %>%
  map_dfr(~ {
    cors <- .x %>% 
      select(diff_1, diff_2, diff_3, diff_5) %>%  # Only keep symmetric windows
      cor(use = "complete.obs") %>%
      as.data.frame() %>%
      rownames_to_column("window") %>%
      select(window, baseline = diff_3) %>%
      filter(window != "diff_3") %>%
      mutate(window = str_remove(window, "diff_"),
             tenor = unique(.x$tenor))
  }) %>%
  # Improvement 1: Logical ordering
  mutate(window = factor(window, levels = c("1", "2", "5"))) |> 
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y")))

# Improvement 3: Facet by tenor instead of dodging
ggplot(baseline_cor_plot, aes(x = window, y = baseline, fill = baseline)) +
  geom_col(width=0.8) +
  facet_wrap(~tenor, nrow = 2) +
  # Improvement 2: Reference line at 0.7
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  # Color gradient: red (low) to green (high)
scale_fill_gradient2(low = "#1a9850", mid = "#fee08b", high = "#d73027", 
                     midpoint = 0.5, limits = c(0, 1),
                     name = "")+
  labs(title = "",
       subtitle = "",
       x = "Alternative Window (days pre-post GovC)", 
       y = "Correlation with 3-days baseline",
       caption = "") +
  theme_bw() +
  theme(text=element_text(family="Segoe UI Light")) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 18),
         axis.title = element_text( size = 20),
         legend.text = element_text(size=16),
         # The new stuff
         strip.text = element_text(size = 18)) 

# Export figure:

ggsave("../output/figures/correlation_alternative_windows.pdf",
       dpi = 320,
       width = 12, # Wider to match second script's direction plot
       height = 9, # Adjusted height
       bg="white")


# Export intermediate dataset: -----


dir.create("../intermediate_data")

differences_df %>% 
  select(tenor,date, correct_pre_mean_3, correct_post_mean_3,diff_1,diff_2,diff_3, correct_post_mean_1,
    correct_post_mean_2,spike) %>% 
  saveRDS("../intermediate_data/range_difference_df.rds")
  