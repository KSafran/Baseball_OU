### Baseball Data Cleaning Functions ---------------------------------------------------------------

# A lot of times we will have a vector of team identifiers, sometimes just the city, sometimes just 
# the team name, sometimes capitalized. We want to convert that to a 3 letter team identifier

team.lookup <- matrix(c('detroit|tigers', 'det',
                           'pitts|pirates', 'pit',
                           'kans|royals', 'kan',
                           'loui|cardinal', 'stl',
                           'ariz|diamondba', 'ari',
                           'cubs', 'chc',
                           'washing', 'was',
                           'colora|rockies', 'col',
                           'diego|padres', 'sdg',
                           'cinci|reds', 'cin',
                           'franci|giants', 'sfo',
                           'phil', 'phi',
                           'dodger', 'lad',
                           'houst|astros', 'hou',
                           'angels', 'laa',
                           'milwau|brewers', 'mil',
                           'mets', 'nym',
                           'miami', 'mia',
                           'oakland|athletics', 'oak',
                           'atlanta|braves', 'atl',
                           'toronto|blue jays', 'tor',
                           'tampa', 'tam',
                           'baltim|orioles', 'bal',
                           'boston|red sox', 'bos',
                           'seattle|mariners', 'sea',
                           'texas|rangers', 'tex',
                           'white sox', 'chw',
                           'minne|twins', 'min',
                           'yankees', 'nyy',
                           'cleveland|indians', 'cle'),
                         ncol = 2, byrow = T)

identify.team <- function(name.vector){
  z <- rep(NA,length(name.vector))
  for(i in seq_along(team.lookup[,1])){
    z[grepl(team.lookup[i,1], tolower(name.vector))] <- team.lookup[i,2]
  }
  z
}

remove.funky.string <- function(char.vector){
  char.vector %>%  str_replace_all('Â', '') %>%
    str_trim() %>%
    as.numeric()
}
