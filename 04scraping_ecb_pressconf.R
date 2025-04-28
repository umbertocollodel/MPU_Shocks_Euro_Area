library(webshot)
library(RSelenium)
library(chromote)
library(rvest)
library(crayon)

# Set parameters

years=seq(1998,2025,by=1)


# Navigate to the ECB page
website="https://www.ecb.europa.eu/press/press_conference/monetary-policy-statement/"

# Get links to individual press conferences

ecb_links_press_conferences=years %>% 
  map(~ paste0(website,.x,"/html/index_include.en.html")) %>% 
  map(~ read_html(.x)) %>% 
  map(~ .x %>% html_nodes("a")) %>% 
  map(~ .x %>% html_attr("href")) %>% 
  map(~.x %>% str_subset("/press/press_conference/monetary-policy-statement/")) %>% 
  map(~.x %>% str_subset("en.html")) %>% 
  map(~.x %>% unique()) %>% 
  map(~ paste0("https://www.ecb.europa.eu",.x)) %>% 
  map(~ .x %>% rev())
  

# Read them as HTML

ecb_site_pressconf=map(ecb_links_press_conferences, ~ map(.x, read_html))

# Scrape texts

ecb_pressconf_text=ecb_site_pressconf %>% 
  modify_depth(2,~ .x %>% html_nodes("p")) %>% 
  modify_depth(2,~ .x %>% html_text()) %>% 
  map(~ paste(.x))

# Remove weird characters


ecb_pressconf_text_clean=ecb_pressconf_text %>% 
  map(~ gsub("[^[:alnum:] .?-]", "", .x))
  

# Dates: ----
  
dates_ecb_pressconf=years %>% 
  map(~ paste0(website,.x,"/html/index_include.en.html")) %>% 
  map(~ read_html(.x)) %>% 
  map(~ .x %>% html_elements(".date")) %>% 
  map(~ .x %>% html_text()) %>% 
  map(~ .x %>% dmy()) %>% 
  unlist() %>% 
  as.Date(., origin = "1970-01-01") 



# Filter out dates after "2021-12-16" and remove duplicates:
# after 2021 there are three links for every meeting so every date is repeated
  
  cutoff_date <- as.Date("2021-12-16")
  dates_after_cutoff <- dates_ecb_pressconf[dates_ecb_pressconf >= cutoff_date]
  unique_dates_after_cutoff <- unique(dates_after_cutoff)
# Combine the filtered unique dates with the dates before the cutoff:
  
  dates_before_cutoff <- dates_ecb_pressconf[dates_ecb_pressconf < cutoff_date]
  final_dates <- c(dates_before_cutoff, unique_dates_after_cutoff) %>% sort()



# Governor names ----

governor_ecb_pressconf=years %>% 
  map(~ paste0(website,.x,"/html/index_include.en.html")) %>% 
  map(~ read_html(.x)) %>% 
  map(~ .x %>% html_elements(".title")) %>% 
  map(~ .x %>% html_text()) %>% 
  map(~ .x[!str_detect(.x, str_c(c("Related","Combined monetary policy","President Lagarde","Monetary policy decision"), collapse = "|"))]) %>% 
  map(~ .x %>% str_extract("^[^,:]*")) %>% 
  map(~ .x %>% rev()) %>% 
  unlist() %>% 
  str_replace("Lucas Papademos","Jean-Claude Trichet") %>% 
  str_replace("Gertrude Tumpel-Gugerell","Jean-Claude Trichet") %>% 
  str_replace("Vítor Constâncio","Mario Draghi") %>% 
  str_replace("José Manuel González-Páramo","Jean-Claude Trichet") %>% 
  str_replace("Eugenio Domingo Solans","Willem F. Duisenberg") 
  

# Put metadata together 

metadata_ecb_pressconf=final_dates %>% 
  map2_chr(governor_ecb_pressconf, ~ paste0(.x,"_",.y)) 
  

  
  
# Add metadata to scraped texts:

ecb_pressconf_final <- ecb_pressconf_text_clean %>%
  flatten() %>% 
  set_names(metadata_ecb_pressconf)

# Remove previous lists to contain overhead

rm(ecb_pressconf_text_clean,ecb_links_press_conferences)



# Export texts: ----

# Check if the files already exist, if not save

ecb_pressconf_final %>% 
  iwalk(~ if (file.exists(paste0("intermediate_data/texts/",.y,".txt"))) {
  cat(crayon::red(paste0("File ",.y,"already exists. Skipping save.\n")))
  } else {
  cat(.x, file = paste0("intermediate_data/texts/",.y,".txt"))
    cat(crayon::green(paste0("File ",.y," saved.\n")))
  })
  



# Work on the introductory statements and q&a separately: -------

# Expressions that separate the two:

expressions=c("\\* \\* \\*",
              "The next press conference will take place on 1 February 2001.",
              "I am now at your disposal for questions.",
              "I am now open to questions.",
              "We are now at your disposal for questions.",
              "We are now at your disposal, should you have any questions.",
              "We are now at your disposal should you have any questions.")

# Introductory statement (stop at first split since sometimes the expressions follow each other):

ecb_introductory_pressconf <- ecb_pressconf_text %>% 
  map( ~ str_split(.x, paste(expressions,collapse = "|"),n=2)) %>% 
  modify_depth(2, ~ gsub("[^[:alnum:] .?-]", "", .x)[1]) %>% 
  flatten() %>% 
  set_names(metadata_ecb_pressconf)


# Export texts

dir.create("intermediate_data/texts/introductory_statements")

ecb_introductory_pressconf %>% 
  iwalk(~ cat(.x, file = paste0("intermediate_data/texts/introductory_statements/",.y,".txt")))




# Q & A:

ecb_qa_pressconf <- ecb_pressconf_text %>% 
  map( ~ str_split(.x, paste(expressions,collapse = "|"),n=2)) %>% 
  modify_depth(2, ~ gsub("[^[:alnum:] .?-]", "", .x)[2]) %>% 
  flatten() %>% 
  set_names(metadata_ecb_pressconf)


# Export texts

dir.create("intermediate_data/texts/Q&A")

ecb_qa_pressconf %>% 
  iwalk(~ cat(.x, file = paste0("intermediate_data/texts/Q&A/",.y,".txt")))





  