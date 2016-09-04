### A little EDA --------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(stringr)

# clean up a few variables
clean.wind.speed <- function(x){
  x %>%
    as.character() %>%
    str_replace('mph','') %>%
    str_trim() %>%
    as.numeric()    
}

over.under.data <- over.under.data %>%
  mutate(W_speed = clean.wind.speed(W_speed),
         competitiveness = abs(home.money.line),
         strong_w_dir = as.character(W_dir),
         over = result == 'Over')
over.under.data$strong_w_dir[over.under.data$W_speed < 10] <- 'None'

# keep an OOT validation set separate
validation <- over.under.data[over.under.data$Date >= '2015-09-01',]
training.ou <- over.under.data[over.under.data$Date < '2015-09-01',]

# which wins more, overs or unders? overs barely
table(training.ou$result)

plot.ou <- function(xvar, nbin = NULL){
  if(!is.null(nbin)){xvar = paste0('cut_number(',xvar, ', ', nbin,')')}
  ggplot(training.ou) + 
    geom_bar(aes_string(x = xvar, y = "..count..", fill = "result"),
             position = 'fill') +
    theme_bw()
}

plot.ou('Condition')
plot.ou('Temp', 10)
plot.ou('ADI',5)
plot.ou('W_speed',5)
plot.ou('W_dir')
plot.ou('home.team')
plot.ou('Gametime_ET',5)
plot.ou('away.money.line',10)
plot.ou('over.under',5)
plot.ou('strong_w_dir')

