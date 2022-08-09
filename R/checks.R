library(subregionalcovid19)
library(tidyverse)
library(lubridate)
library(xts)
library(forecast)
current_date=today()
# read in all country data using LoadCountries() function
all_countries=as_tibble(LoadCountries())%>%
  select(-geoid,-geometry)

# ensure all country names align w/ how they are represented in the country-specific load functions
all_countries_2=all_countries%>%
  mutate(Country=ifelse(Country=='United Kingdom','UK',
                        ifelse(Country=='United States','US',
                        ifelse(Country=='Switzerland','SwitzerlandLiechtenstein',
                        ifelse(Country=='Liechtenstein','SwitzerlandLiechtenstein',Country)))))
# save today's data for subsequent comparisons
all_countries_to_save=all_countries_2%>%
  select(DateReport,Country,pInf)
write.csv(all_countries_to_save,paste0("all_countries_", current_date, ".csv"),row.names=F)


## identify and count the number of unique countries in the data frame
## all_countries_list can serve as a reference list of countries to look out for 
all_countries_list=unique(all_countries_2$Country)
num_unique_countries=length(all_countries_list)

## identify the last date of report for each country
date_last_report_df=all_countries%>%
  select(DateReport,Country)%>%
  group_by(Country)%>%
  summarise(date_last_reported=max(DateReport))

## identify the countries with last date of report more than five days prior to the current day 
countries_last_report_flag=date_last_report_df%>%
  mutate(date_last_reported=strptime(date_last_reported,format='%Y-%m-%d'))%>%
  mutate(days_since_last_report=difftime(today,date_last_reported,units='days'))%>%
  mutate(days_since_last_report_rounded=round(as.numeric(days_since_last_report)))%>%
  filter(days_since_last_report_rounded>5)%>%
  select(Country)

# identify countries missing from the LoadCountries() output

## identify countries with country-specific load functions
list_load_country_functions=list.files()
countries=gsub("Load","",list_load_country_functions)
final_countries_load_each=gsub(".R","",countries)
## identify which (if any) of these countries is missing from the df generated upon running LoadCountries()
missing_countries_load_countries=final_countries_load_each[which(final_countries_load_each%in%all_countries_list==FALSE)]

## load in all Europe data
Europe=LoadEurope()
countries_europe=unique(Europe$Country)
### identify European countries w/out country-specific load functions, which are included in LoadEurope()
european_countries_second_list=countries_europe[which(countries_europe%in%final_countries_load_each==FALSE)]
### identify which (if any) of these countries is missing from the df generated upon running LoadCountries()
missing_countries_Europe2=european_countries_second_list[which(european_countries_second_list%in%unique(all_countries$Country)==FALSE)]

## Load in all Google sourced data
GoogleSourced=LoadGoogleSourced()
countries_google_sourced=unique(GoogleSourced$Country)
### identify which (if any) of these countries is missing from the df generated upon running LoadCountries()
missing_countries_google_sourced=countries_google_sourced[which(countries_google_sourced%in%list_countries==FALSE)]

## Load in all JHU data
JHUCSSE=LoadJHUCSSE()
countries_JHUCSSE=unique(JHUCSSE$Country)
### identify which (if any) of these countries is missing from the df generated upon running LoadCountries()
missing_countries_JHUCSSE=countries_JHUCSSE[which(countries_JHUCSSE%in%list_countries==FALSE)]


#collate all missing countries identified previously
missing_countries_all=unique(c(missing_countries_load_countries,
                        missing_countries_Europe2,
                        missing_countries_google_sourced,
                        missing_countries_JHUCSSE))

# plot latest trends from the past week


## get data frames from prior week
prior_week_dates=seq(current_date-7,current_date,by='1 day')### redo the above w/ yesterday's data
all_countries_2_prior_weeks=rbind.data.frame(read_csv(paste0("all_countries_", prior_week_dates[1], ".csv")),
                                            read_csv(paste0("all_countries_", prior_week_dates[2], ".csv")),
                                            read_csv(paste0("all_countries_", prior_week_dates[3], ".csv")),
                                            read_csv(paste0("all_countries_", prior_week_dates[4], ".csv")),
                                            read_csv(paste0("all_countries_", prior_week_dates[5], ".csv")),
                                            read_csv(paste0("all_countries_", prior_week_dates[6], ".csv")),
                                            read_csv(paste0("all_countries_", prior_week_dates[7], ".csv")))
                                            
## estimate cases/100k, as done in "get_daily_PEER_data.R" & summarize across all sub-areas within a country
all_countries_2_summarized=all_countries_2_prior_weeks%>%
  mutate(cases_per_100k_past_14_d = round(pInf*(7/5)*1e5, 3))%>%
  group_by(Country,DateReport)%>%
  summarise(summed_cases_per_100k_past_14_d=sum(cases_per_100k_past_14_d))

# CODE BELOW CANNOT BE RUN UNTIL WE'VE SAVED UP TO A WEEK OF PRIOR DATA
## plot cases per 100 k rolling average for the past week in countries of interest
### specify countries of interest
countries_of_interest=c()
all_countries_2_summarized_filtered=all_countries_2_summarized%>%
  filter(Country%in%countries_of_interest)

ggplot(all_countries_2_summarized_filtered,
       aes(x=DateReport,y=summed_cases_per_100k_past_14_d,group=Country,col=Country))+geom_line()

## quantitative check of aberrant patterns in the cases per 100k 
### identify outliers using forecast package (outliers defined as upper quartile + 3*IQR and lower quartile - 3*IQR )
all_countries_2_summarized_filtered_outliers=all_countries_2_summarized_filtered%>%
  group_by(Country)%>%
  mutate(outlier_index=ifelse(is.na(tsoutliers(xts(summed_cases_per_100k_past_14_d,DateReport))$index)==TRUE,'none',
                              tsoutliers(xts(summed_cases_per_100k_past_14_d,DateReport))$index))


