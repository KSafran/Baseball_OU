### Cleaned scraped data ---------------------------------------------------------------------------

### This file cleans data created by the Data/Scrape OU.R script

# Setup
setwd('C:/Users/Kyle/Desktop/baseball_OU')
source('/Utilites/')

scraped.data <- as.data.frame(readRDS('Data/over_under_scrape_2015.Rda'))

result.split <- str_split_fixed(scraped.data$result, ':' , n = 2)

scraped.data <- scraped.data %>%
  mutate(over.under = remove.funky.string(over.under),
         home.money.line = remove.funky.string(home.money.line),
         away.money.line = remove.funky.string(away.money.line),
         runs = as.numeric(str_trim(result.split[,2])),
         result = str_trim(result.split[,1]),
         home.pitch.last = str_split_fixed(home.pitcher,',',n = 2)[,1] %>% 
           tolower(),
         away.pitch.last = str_split_fixed(away.pitcher,',',n = 2)[,1] %>% 
           tolower())

saveRDS(scraped.data, file = 'Data/clean_OU_scrape_2015.Rda')
