# Scrape data from Vegas Insider -------------------------------------------------------------------

# Setup
setwd('C:/Users/Kyle/Desktop/baseball_OU')
library(rvest)
library(lubridate)
library(dplyr)
library(magrittr)
library(stringr)

# Set start and end pull dates
start.date <- '2015-04-06'
end.date <- '2015-10-31'

big.data.frame <- data.frame(game = 'game',
                             away= 'away',
                             home = 'home',
                             home.odds = '000',
                             UO = '0',
                             away.odds = '000',
                             date = '00-00-0000')

URL.base <- 'http://www.vegasinsider.com/mlb/scoreboard/scores.cfm/game_date/'

# Lets use dates from the 2015 season
dates <- ymd(seq.Date(as.Date(start.date), as.Date(end.date), by = 'day'))

# Vegas Insider URLs use dates in mm-dd-yyyy format
dates2 <- format(dates,'%m-%d-%Y')

ou.list <- list()
for(i in seq_along(dates2)){
  
  baseball.site <- read_html(paste0(URL.base,dates2[i]))
  raw.out <- baseball.site %>% 
    html_nodes('.tanBg td') %>%
    html_text()
  
  # This comes out as a long vector, and games might have extra innings, so that makes it tough to
  # put into a nice table. We will use the comma in the pitchers' name to identify unique lines 
  
  line.identifier <- grepl(',', raw.out)
  # teams come 1 before the pitcher names 
  teams <- identify.team(raw.out[lead(line.identifier)])
  pitchers <- str_trim(gsub('Â|\\\n','',raw.out[line.identifier]))
  money.line <- raw.out[lag(line.identifier)]
  over.under <- raw.out[lag(line.identifier,2)]
  result <- str_replace(raw.out[grepl('Over:|Under:|Push:',raw.out)], 'Â','')
  
  # some days don't have games. If that is the case just go to the next day
  if(length(teams) == 0){next}
  
  # sometimes games are delayed or rained out. These are luckily funneled to the bottom of the 
  # site, but they will cause an error if length(results) < the length of the other stuff we are 
  # putting into this data.frame. In these cases we will just add in NAs to the results
  
  length.table <- length(over.under[nchar(over.under)>2])
  
  if(length(result) < length.table){
    result <- c(result, rep(NA, length.table - length(result)))
  }
  
  # combine these into a data frame, save to a list. Home team always listed second
  ou.list[[i]] <- data.frame(home.team = teams[seq(2,length(teams)-1,2)],
                             away.team = teams[seq(1,length(teams)-2,2)],
                             home.pitcher = pitchers[seq(2,length(teams),2)],
                             away.pitcer = pitchers[seq(1,length(teams)-1,2)],
                             home.money.line = money.line[seq(3,length(teams),2)],
                             away.money.line = money.line[seq(2,length(teams)-1,2)],
                             over.under = over.under[nchar(over.under)>2],
                             result = result,
                             date = dates2[i])
  
  # print progess after every 10 days
  if(i %% 10 == 0) {print(i)}
}

scraped.data <- rbind_all(ou.list)

saveRDS(scraped.data, file = 'Data/over_under_scrape_2015.Rda')

