### Munge Starter and Weather Data -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
setwd('C:/Users/Kyle/Desktop/baseball_OU')
start.weather <- read.csv('Data/starters_weather.csv')
scraped.data <- readRDS('Data/clean_OU_scrape_2015.Rda')
# We need to summarize the weather data to the game level
# We can identify games by date and pitcher for matching with over under data. Watch out for double
# headers

weather <- start.weather %>%
  select(Date, Team, Oppt, H.A, Game_ID, Gametime_ET, Team_score, Oppt_score, Home_Ump, Temp,
         Condition, W_speed, W_dir, ADI, prior_ADI, Oppt_pitch_Name) %>%
  unique()

# Just keep the home teams data

weather.home <- weather %>% filter(H.A == 'h')

# some game_ids are duplicated, whats the deal?
# table(weather.home$Game_ID)[table(weather.home$Game_ID)>1]
# 20150628-cin-nym-1 20150718-lad-was-1 
# 2                  2 
# 20150912-stl-cin-1 
# 2

weather.home <- weather.home[!duplicated(weather.home$Game_ID),]

# Merge weather and OU -----------------------------------------------------------------------------

# we will merge by date, home team, and pitcher last name. Pitcher last name to avoid double headers
weather.home$away.pitch.last <- substr(weather.home$Oppt_pitch_Name, 1, 
                                       str_locate(weather.home$Oppt_pitch_Name, ',')-1) %>%
  tolower()

# Are the home team abbreviations the same in both datasets? yep!
unique(weather.home$Team) %in% unique(scraped.data$home.team)
# make sure dates are the same format
weather.home$Date <- ymd(weather.home$Date)
scraped.data$Date <- mdy(scraped.data$date)

# join em together
over.under.data <- left_join(scraped.data, weather.home,
                             by = c('Date','home.team' = 'Team' ,'away.pitch.last'))

# the missing 40 games appear to be random, we will just drop them, might  be rained out games?
over.under.data <- over.under.data[!is.na(over.under.data$ADI),]
# then 9 more don't have valid results, we can ddrop those too
over.under.data <- over.under.data[over.under.data$result != '',]

saveRDS(over.under.data, file = 'Data/over_under_weather.R')
