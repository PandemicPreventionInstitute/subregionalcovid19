# Authors: Aroon, edited by Kaitlyn
# Date added: 06-28-2022

# This script uses the functions in this repo (Load Conuntries) and generates the
# output data file needed for PEER. It has a column for each  the risk and 
# expected infected attendees for each event size. 

library(janitor)
library(tidyverse)
library(dplyr)
source("LoadCountries.R")


calc_risk_raw <- function (p, n){
    risk <- 1-((1-p)^n)
    return(risk)
}

get_flagged_countries <- function(){
    library(janitor)
    library(tidyverse)
    library(dplyr)
    
    NGS_data <- read.csv('https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/ngs_find_map/clean_dataset.csv') |> janitor::clean_names()
    data_for_countries_w_insuff_testing <- NGS_data %>% 
        dplyr::filter( test_archetype == 'Test - Increase diagnostic testing capacity') %>% 
        dplyr::select(country, date_tests_last_reported, test_positivity_rate, average_daily_tests) # columns we could add if we wanted to provide metrics to users
    return(data_for_countries_w_insuff_testing) # returns the countries with their metrics
    
}

generate_map_assets <- function(ascertainment_bias = 4,
                                event_sizes = c(2, 5, 10, 15, 20, 25, 50, 100, 150, 200, 250, 500, 1000),
                                prefix = "./",
                                format = "fgb",
                                risk_filter = 0,
                                overwrite = TRUE,
                                excluded_countries = NULL) {
    
    flagged_countries <- get_flagged_countries() %>% dplyr::pull(country) # not adding the corresponding metrics 
    GLOBALMAP <- LoadCountries(countries = NULL, interactiveMode = FALSE)
    GLOBALMAP <- LoadCountries(countries = NULL, interactiveMode = FALSE)
    GLOBALMAP$AB <- ascertainment_bias
    # Add a column for testing flag where TRUE = insufficient testing & need to explain, FALSE = sufficient testing
    GLOBALMAP <- GLOBALMAP |>
        mutate(testing_flag = 
                   ifelse(Country %in% flagged_countries, TRUE, FALSE))
    
    GLOBALDATA <- GLOBALMAP 
    # If data hasn't been reported for more than 14 days, replace pInf value with NA (or whatever makes the most sense for front end)
    GLOBALDATA <- GLOBALDATA %>% dplyr:: mutate(
        pInf = ifelse(
            lubridate::ymd(DateReport) < (lubridate::today() - lubridate::days(14)), NA, pInf)
    )
    OUTOFDATEDATA <- GLOBALDATA %>% dplyr::filter(is.na(pInf))
    stopifnot('Not all NAs in pInf are from out of date data' = max(OUTOFDATEDATA$DateReport) < lubridate::today() - lubridate::days(14))
    
    # Make a dataframe that we will concatenate each event size to
    GLOBALDATAALL<- c()
    for (size in event_sizes) {
        #new_col_name <- paste0("size_", size)
        
        THISGLOBALDATA <- GLOBALDATA %>%
            filter(!Country %in% excluded_countries) |>
            dplyr::mutate(risk = round(100 * calc_risk_raw(pInf * AB, size),3),
                          n = size) %>% 
            dplyr::mutate(
                #risk = ifelse( risk ==1, '> 99', risk), # will handle in frontend
                risk = ifelse( is.na(pInf), -1, risk) # set outdated data to -1
            ) %>% 
            dplyr::mutate(
                # Expected number of event attendees arriving infected
                # This is commented out, since the calc is happening on the PEER app frontend
                # exp_introductions = (pInf*AB*size), # on the front end, will want to set 0 to <1,
                # Cases per 100k in the past 14 days
                cases_per_100k_past_14_d = round(pInf*(7/5)*1e5, 3) # need to multiple by 14/10 because to estimate case prevalence (pInf) 
                # we cases in the past 14 days by 10 and dive by 14 (so need to undo it)
            )
        
        # Concatenate each event size
        GLOBALDATAALL <- dplyr::bind_rows(GLOBALDATAALL, THISGLOBALDATA)
        length_data <- nrow(THISGLOBALDATA)
        
    }
    
    # Pivot data from long to wide so that there are columns for each event size
    GLOBALDATAWIDE <- GLOBALDATAALL %>% 
      tidyr::pivot_wider(names_from = n, values_from = risk, names_prefix = 'risk_')
    
    # Unit test to make sure that all countries have estimates for risk regardless of event size
    stopifnot('Not all datasets are the same number of rows' = nrow(GLOBALDATAALL) == length_data*length(event_sizes))
    
    # Write output fgb file
    sf::st_write(
        sf::st_as_sf(GLOBALDATAWIDE),
        glue::glue("{prefix}/globalDataWide.{format}")
    )
}
data<-generate_map_assets()