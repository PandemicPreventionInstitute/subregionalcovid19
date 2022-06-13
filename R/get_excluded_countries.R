# get_excluded_countries.R
# This script generates a list of the countries to exclude in the 
# generate_map_assets function in fct_fgb.R in the c19-app-ppi repo
get_excluded_countries<- function() {
    library(tidyverse) # Data wrangling
    library(tibble) # Data wrangling
    library(janitor) # Column naming
    library(countrycode) # Country codes
    library(lubridate) # Date times
    library(readxl) # Excel import
    library(zoo) # Calculate rolling averages
    library(R.utils) # R utilities
    library(stringr) # To parse strings in R
    library(magrittr) # Needs to be run every time you start R and want to use %>%
    library(dplyr) # Data wrangling
    library(scales) # Comma formatting
    library(bpa) # To get the trim_ws working, which will allow you to join the lat and long files
    library(reshape2) # melt function for wrangling
    
    
    
    ALL_DATA_PATH<- url("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")
    
    
    find_raw <- read.csv(ALL_DATA_PATH) %>% clean_names()
    
    # Select and rename necessary columns
    find_testing_t <- find_raw %>%
        # Filter for country set
        filter(set == "country") %>%
        # Select the smoothed and raw test and case data by day (case data is also from OWID)
        select(name, time, unit, pop_100k, all_new_tests, all_new_cases, 
               pos, new_tests_orig, new_cases_orig) %>%
        # Rename columns as date, code, pop_100k, new_tests_smoothed, cum_tests, cum_cases, new_cases_smoothed
        rename(country = name, date = time, code = unit, new_tests_smoothed = all_new_tests, 
               new_cases_smoothed = all_new_cases) %>%
        
        # Parse date as date class
        # Data parsing is the process of taking data in one format & transforming it to another format
        mutate(date = as.Date(date, format = "%Y-%m-%d"), #reformatting date
               pop = pop_100k*100000, # get full pop
               # make country code column for joining
               code = countrycode(country, origin = 'country.name', destination = 'iso3c')) 
    
    # Add a new column that has the new cases smoothed with NA entries when new_tests_smoothed is NA
    find_testing_t['new_cases_smoothed_truncated']<-find_testing_t$new_cases_smoothed
    find_testing_t$new_cases_smoothed_truncated[is.na(find_testing_t$new_tests_smoothed)]<-NA
    
    
    # Inserts missing country codes manually 
    find_testing_t$code[find_testing_t$country == "Kosovo"] <- "XXK"
    find_testing_t$code[find_testing_t$country == "Namibia"] <- "NAM"
    
    # FILL IN MISSING DATES (just in case dates are missing in some countries in database)
    # Set start date
    first_date<-min(find_testing_t$date, na.rm = TRUE)
    date <- seq.Date(first_date, today(), by = "day")
    code <-unique(find_testing_t$code)
    date_country<-expand_grid(date, code)
    
    # Join the find_test_t dataframe with filled dates df
    find_testing_t<-left_join(date_country,find_testing_t, by = c("code", "date"))
    
    # 1. Find the data the country last reported tests
    find_test_update_date<-find_testing_t%>%
        group_by(code) %>% select(code, date, new_tests_orig)%>% #Is new_test_orig an existing column name? lost here
        filter(!is.na(new_tests_orig) & new_tests_orig != 0)%>%
        filter(date == max(as.Date(date)))%>%
        mutate(date_tests_last_reported = date)%>%
        select(code, date_tests_last_reported) %>%
        mutate(
            rept_tests_within_last_6_months = as.Date(date_tests_last_reported)> (today()-180),
            days_since_tests_reported = today() - as.Date(date_tests_last_reported))
    
    # 2. Find the avg daily tests per 1000 and the tpr over the past year 
    find_testing_last_year<- find_testing_t %>% filter(date>=(today() -364) & 
                                                           date<= today())%>%
        group_by(code) %>%
        summarise(tests_in_last_year_smoothed = sum(new_tests_smoothed, na.rm = TRUE),
                  cases_in_last_year_smoothed_truncated = sum(new_cases_smoothed_truncated, na.rm = TRUE),
                  tpr_year_smoothed_truncated = round(cases_in_last_year_smoothed_truncated/tests_in_last_year_smoothed,4), # used for archetype definition
                  tpr = round(100*tpr_year_smoothed_truncated,2),
                  avg_daily_tests_per_1000_last_year_smoothed = round(1000*mean(new_tests_smoothed/max(pop, na.rm = T), na.rm = TRUE),2), # used for archetype definition
                  population_size = max(pop)# pops should all be the same
        )%>% 
        filter(!is.na(code))
    
    # Combine two datasets (test reporting and )
    find_testing_metrics<- left_join(find_testing_last_year, find_test_update_date, by = "code")
    
    # Set thresholds for including countries (same as NGS mapping)
    daily_tests_thres <- 0.5
    TPR_thres<- 20 
    
    find_testing_metrics <- find_testing_metrics %>% 
        mutate(exclude_country = 
                   case_when(
                       rept_tests_within_last_6_months == FALSE ~ TRUE,
                       tpr >= TPR_thres & avg_daily_tests_per_1000_last_year_smoothed <= daily_tests_thres ~ TRUE, #bottom right
                       tpr >= TPR_thres & avg_daily_tests_per_1000_last_year_smoothed > daily_tests_thres ~ TRUE, # upper right
                       tpr < TPR_thres & avg_daily_tests_per_1000_last_year_smoothed <= daily_tests_thres ~ TRUE, # bottom left
                       tpr < TPR_thres & avg_daily_tests_per_1000_last_year_smoothed > daily_tests_thres ~ FALSE # upper left
                   )
        )
    countrycode(metadata$country, origin = 'country.name', destination = 'iso3c')
    
    excluded_countries<- find_testing_metrics %>% filter(exclude_country == TRUE) %>%
        mutate(country = countrycode(code, origin = 'iso3c', destination = 'country.name')) %>% 
        pull(country)
    
    return(excluded_countries)
}

