library(tidyverse)
library(NBAr)

season <- 2018
teams <- c(1610612760,1610612748,1610612759)

date_from = '2019-01-01'
date_to = '2019-01-01'


dataset <- map(teams, function(x) NBAr::get_on_off(season, team_id = x)) %>% 
          compact() %>% 
          bind_rows()

  
  
  colnames(dataset)
  
  
  
  