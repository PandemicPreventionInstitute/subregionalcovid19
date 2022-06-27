# get_excluded_countries.R
# This script generates a list of the countries to exclude in the 
# generate_map_assets function in fct_fgb.R in the c19-app-ppi repo

# Set thresholds for including countries (same as NGS mapping)
daily_tests_thres <- 0.1 # 0.5
TPR_thres<- 50 # 20
FIND_REPORT_FLAG<-TRUE
get_excluded_countries<- function(daily_tests_thres, TPR_thres, FIND_REPORT_FLAG) {
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
    ECONOMY_PATH<- url('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WB_class_data.csv')
    
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
    
    
    
    find_testing_metrics <- find_testing_metrics %>% 
        mutate(exclude_country = 
                   case_when(
                       (FIND_REPORT_FLAG == TRUE & rept_tests_within_last_6_months == FALSE) ~ TRUE,
                       tpr >= TPR_thres & avg_daily_tests_per_1000_last_year_smoothed <= daily_tests_thres ~ TRUE, #bottom right
                       tpr >= TPR_thres & avg_daily_tests_per_1000_last_year_smoothed > daily_tests_thres ~ TRUE, # upper right
                       tpr < TPR_thres & avg_daily_tests_per_1000_last_year_smoothed <= daily_tests_thres ~ TRUE, # bottom left
                       tpr < TPR_thres & avg_daily_tests_per_1000_last_year_smoothed > daily_tests_thres ~ FALSE # upper left
                   )
        )
  
    
    # add World Bank
    world_bank_background_raw <- read_csv(ECONOMY_PATH) %>%
        # Standardize names with this janitor function
        clean_names()
    # Remove buffer rows by iso3 code and select code and testing capacity columns
    world_bank_background_clean <- world_bank_background_raw %>%
        # Drop all columns except iso3 code and world_bank_economies
        select('code','income_group')%>%
        filter(code != 'x') 
    
    find_testing_metrics <- left_join(find_testing_metrics, world_bank_background_clean, by = "code")
    
    find_testing_metrics <- find_testing_metrics %>% mutate(
        LMIC = ifelse(income_group == "Upper middle income", FALSE, TRUE)
    )
    
    excluded_countries<- find_testing_metrics %>% filter(exclude_country == TRUE) %>%
        mutate(country = countrycode(code, origin = 'iso3c', destination = 'country.name')) 
    
    unique_countries_risk_repo <- read.csv('../data/unique_countries_risk_repo.csv') %>% 
        rename(country = x)  %>% mutate(
            code = countrycode(country, origin = 'country.name', destination = 'iso3c')
        ) %>% select(code) %>% pull(code)
    
    excluded_countries <- excluded_countries %>% filter(code %in% unique_countries_risk_repo)
    
    n_LMICs <- sum(excluded_countries$LMIC==TRUE)
    
    return(excluded_countries)
}


TPR_thres_vec<- seq(from = 20, to = 50, by = 10)
daily_tests_thres_vec <- seq(from = 0.1, to = 0.5, by = 0.1)
FIND_REPORT_FLAG_VEC<- c(TRUE, FALSE)
df<-c()
for (i in 1:length(TPR_thres_vec)){
    TPR_thres<- TPR_thres_vec[i]
    for(j in 1:length(daily_tests_thres_vec)){
        daily_tests_thres<-daily_tests_thres_vec[j]
        for(k in 1:length(FIND_REPORT_FLAG_VEC)){
            FIND_REPORT_FLAG<-FIND_REPORT_FLAG_VEC[k]
            n_LMICs<-get_excluded_countries(daily_tests_thres, TPR_thres,FIND_REPORT_FLAG)
            this_df<-data.frame(n_LMICs, TPR_thres, daily_tests_thres, FIND_REPORT_FLAG)
            df<-bind_rows(df, this_df)
        }
    }
}

df %>% ggplot() + geom_line(aes(x = daily_tests_thres, y = n_LMICs, group = TPR_thres, color = TPR_thres)) +
    facet_wrap(~FIND_REPORT_FLAG) +
    theme_bw() + xlab('Minimum daily tests per 1,000') + ylab('Number of LMICs excluded')

excluded_countries_req_FIND_data<-get_excluded_countries(0.2, 40, TRUE)
excluded_countries_dont_req_FIND_data<-get_excluded_countries(0.25, 40, FALSE)

