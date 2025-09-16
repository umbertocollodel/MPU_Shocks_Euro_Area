####### Script to produce an uncertainty of monetary policy factor

library(padr)
library(DescTools)
library(readxl)
library(tidyverse)
library(zoo)
library(corrr)
library(stargazer)
library(Hmisc)
library(broom)


### Custom functions: ---------

calculate_lags <- function(df, var, lags){
  map_lag <- lags %>% map(~partial(lag, n = .x))
  return(df %>% mutate(across(.cols = {{var}}, .fns = map_lag, .names = "{.col}_lag{lags}")))
}

calculate_leads <- function(df, var, leads){
  map_leads <- leads %>% map(~partial(lead, n = .x))
  return(df %>% mutate(across(.cols = {{var}}, .fns = map_leads, .names = "{.col}_lead{leads}")))
}


# Set parameters: 

names_sheets=excel_sheets("raw_data/OIS.xls")

tenors=names_sheets %>% 
  str_remove("_.*")


# Clean daily data for OIS:

df_list <- names_sheets %>% 
  map(~ read_excel("raw_data/OIS.xls", sheet = .x)) %>%
  map(~.x %>% slice(-1)) %>% 
  map(~.x %>% select(1:5)) %>% 
  map(~ .x %>% mutate(Daily = as.numeric(Daily))) %>% 
  map(~ .x %>% mutate(Daily = as.Date(Daily, origin = "1899-12-30"))) %>% 
  map(~ .x %>% setNames(c("daily","first","high","low","last"))) %>% 
  map(~ .x %>% mutate_at(2:ncol(.),as.numeric)) 
  
  
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


dates=read_xlsx("raw_data/dates_govc.xlsx") %>% 
  unite("date",day:year,sep="-",remove = T) %>% 
  mutate(date = as.Date(date,"%d-%m-%Y")) %>% 
  .$date
  

# Calculate difference between min-max range pre-post govc: 

df_list_clean <- df_list %>% 
  map(~ .x %>%
        # mark government change days
        mutate(govc = ifelse(daily %in% dates, 1, 0)) %>%
        # daily high-low gap
        mutate(gap = high - low) %>%
        
        # post_govc and pre_govc for 3-day window
        mutate(post_govc = ifelse(lag(govc, 1) == 1 | lag(govc, 2) == 1 | lag(govc, 3) == 1, 1, 0),
               pre_govc  = ifelse(lead(govc, 1) == 1 | lead(govc, 2) == 1 | lead(govc, 3) == 1, 1, 0)) %>%
        
        # cumulative govc count
        mutate(sum_govc = cumsum(govc),
               sum_govc = ifelse(govc == 1, sum_govc, 0)) %>%
        
        # calculate lags and leads for 3-day window
        calculate_lags(sum_govc, 1:3) %>%
        calculate_leads(sum_govc, 1:3) %>%
        
        # govc_id as sum of lag/lead indicators
        mutate(govc_id = rowSums(select(., starts_with("sum_")), na.rm = TRUE)) %>%
        
        # pre/post means for 3-day window
        group_by(govc_id, pre_govc) %>%
        mutate(pre_mean = mean(gap, na.rm = TRUE)) %>%
        group_by(govc_id, post_govc) %>%
        mutate(post_mean = mean(gap, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(correct_post_mean = lead(post_mean, 1),
               correct_pre_mean  = lag(pre_mean, 1)) %>%
        
        # ---- new: post means for shorter windows ----
        # 1-day window
        mutate(post_govc_1 = ifelse(lag(govc, 1) == 1, 1, 0)) %>%
        group_by(govc_id, post_govc_1) %>%
        mutate(post_mean_1 = mean(gap, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(correct_post_mean_1 = lead(post_mean_1, 1)) %>%
        
        # 2-day window
        mutate(post_govc_2 = ifelse(lag(govc, 1) == 1 | lag(govc, 2) == 1, 1, 0)) %>%
        group_by(govc_id, post_govc_2) %>%
        mutate(post_mean_2 = mean(gap, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(correct_post_mean_2 = lead(post_mean_2, 1)) %>%
        
        # keep only relevant rows and variables
        distinct(sum_govc, .keep_all = TRUE) %>%
        filter(!is.na(gap)) %>%
        select(govc_id, correct_pre_mean, correct_post_mean, correct_post_mean_1, correct_post_mean_2) %>%
        
        # difference in gap for main 3-day window
        mutate(diff = (correct_post_mean - correct_pre_mean) * 100)
      )


# Add back govc dates:

dates_format= dates %>% 
    data.frame(date=.) %>% 
    mutate(govc_id = seq_len(nrow(.)))


final_df_list <- df_list_clean %>% 
    map(~ .x %>% left_join(dates_format))


# Plot result: ------

final_df_list[[2]] %>%
  slice(-1) %>% 
  ggplot(aes(date,diff)) +
  geom_col() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Winsorize outliers and highlight "big" spikes: ---

differences_df <-  final_df_list %>% 
    map(~ .x %>% filter(!is.na(diff))) %>% 
    map(~ .x %>% mutate(diff = DescTools::Winsorize(diff))) %>% 
    map(~ .x %>% mutate(up_threshold = mean(diff,na.rm=T) + 1.5*sd(diff,na.rm=T))) %>% 
    map(~ .x %>% mutate(low_threshold = mean(diff,na.rm=T) - 1.5*sd(diff,na.rm=T))) %>% 
    map(~ .x %>% mutate(spike = case_when(diff >= up_threshold |
                                          diff <= low_threshold~ 1,
                        T ~ 0))) %>% 
  set_names(tenors) %>% 
  bind_rows(.id = "tenor")
  




differences_df %>% 
  mutate(tenor = factor(tenor, levels = c("3mnt", "6mnt", "1Y", "2Y", "5Y", "10Y"))) %>%
  ggplot(aes(date, diff)) +
  geom_col() +
  facet_wrap(~tenor, scales = "free_y") +
  #geom_vline(data = differences_df %>% filter(spike == 1 & diff >= 0), aes(xintercept = date), color = "red", size = 2, alpha = 0.3) +
  #geom_vline(data = differences_df %>% filter(spike == 1 & diff <= 0), aes(xintercept = date), color = "blue", size = 2, alpha = 0.3) +
  theme_minimal() +
  labs(y = "Bps (difference between pre and post GovC)", x = "") +
  scale_x_date(date_breaks = "18 weeks") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#https://www.nytimes.com/2017/06/08/business/economy/europe-ecb-rates.html
#https://www.ecb.europa.eu/press/press_conference/monetary-policy-statement/2023/html/ecb.is230316~6c10b087b5.en.html
  



# Export dataset: -----


dir.create("intermediate_data")

differences_df %>% 
  select(tenor,date, correct_pre_mean, correct_post_mean,diff, correct_post_mean_1,
    correct_post_mean_2,spike) %>% 
  saveRDS("../intermediate_data/range_difference_df.rds")
  