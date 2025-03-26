# The script performes a factor decomposition of the range difference
# pre-post govc across different maturities, in the same fashion of the
# monetary surprises

library(psych)
library(ggtext)

# Set matrix and date vector: -----

X <- read_rds("intermediate_data/range_difference_df.rds") %>% 
 select(tenor,date,diff) %>% 
 pivot_wider(names_from = tenor,values_from = diff) %>% 
 filter(date >= "2011-08-04") %>% 
 rename(`3M` = `3mnt`,`6M` = `6mnt`) %>%
 select(-date) %>%
 slice(-nrow(.)) %>%  #chck this thing! why NAs for July??
 scale(.)

date=read_rds("intermediate_data/range_difference_df.rds") %>% 
  filter(date >= "2011-08-04") %>% 
  .$date %>% 
  unique()


# Performing PCA on OIS yields range -----

## Checking the number of eigenvalues
ev <- eigen(cor(X)) # get eigenvalues

fa.parallel(X)  # Use


## Compute factors.


Nfacs <- 3  # This is for three factors. You can change this as needed.

fit <- X %>% factanal(Nfacs, rotation="equamax",scores = "regression")

fit %>% print(digits=2, cutoff=0.3, sort=TRUE)


# Clean loadings ----

load_df <- fit$loadings[1:6,] %>% 
  round(3) %>%
  data.frame() %>% 
  rownames_to_column(var="Term") %>% 
  as_tibble()

names(load_df) <- c("Term","Timing","QE","Path")


# Reshape for plotting:

loadings_final_df <- load_df %>%
  mutate(Term = factor(Term, levels = c("3M","6M","1Y","2Y","5Y","10Y"))) %>% 
  pivot_longer(matches("Path|QE|Timing"),names_to = "Factor") %>% 
  mutate(Factor = factor(Factor, levels = c("Timing","Path","QE")))

# Save:

#saveRDS(loadings_final_df,"code/app/app_data/app_data_loadings.rds")


# Figure: loadings OIS factor -----

loadings_final_df %>% 
  ggplot(aes(Term,value,group=1)) +
  geom_line(size=2) +
  facet_wrap(~Factor) +
  labs(title="",
       col="",
       y="",
       x="",
       fill="",
       caption = "**Note**: The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors: downward-sloping (Timing), hump-shaped (Path) and upward-sloping (QE).  
       It takes three principal components from the variation in yields around GovC meetings, rotates and scales them.  
**Source**: Authors' calculation   
**Latest observation**: 15 June 2023.") +  
  theme_bw() +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.text = element_text(size=14),
         # The new stuff
         strip.text = element_text(size = 20)) +
  theme(plot.caption = element_markdown(hjust = 0,size=12))


#Export:

ggsave("output/figures/loadings_surprises.png",
       width = 16,
       height= 4,
       dpi = "retina")


# Monetary policy uncertainty surprises: clean the final dataframe ----
# Problem with July obs that's why I removed it

# Merge dataframe with scores of yields and spreads

df_surprises <- data.frame(fit$scores,date %>% .[-length(.)]) %>% 
  as_tibble()

# Change factor names 

names(df_surprises) <- c("Timing","QE","Path","date")


# Save intermediate data: -------


df_surprises %>% 
  saveRDS(file="intermediate_data/monetary_policy_uncertainty.rds")




