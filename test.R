library(tidyverse)
library(NBAr)

season <- 2018
teams <- c(1610612760,1610612748,1610612759)

date_from = '2019-01-01'
date_to = '2019-01-01'


# libraries needed 
library(tidyverse)
library(NBAr)
library(glue)

# get list of the games
game_list = c(21800001:21801230)

# download them all

# runs delay, downloads file and saves it to csv, returns only game_id of successful downloads
download_wrap = function(game_id, boxscore_type, delay)
{
  Sys.sleep(5)
  print(glue('downloading {game_id}'))
  df = get_boxscore(game_id, boxscore_type = 'playertrack')
  readr::write_csv(df, glue("playertrack_{game_id}.csv"))
  return(game_id)
}


downloaded_game_list = map(game_list, ~download_wrap(.,boxscore_type = 'playertrack', 5)) 

# bind files into one
file_list = list.files(pattern = ".csv")
playertrack = map(file_list, ~read_csv(.)) %>% compact %>% bind_rows()

# write full file to csv
readr::write_csv(playertrack, "playetrack_data.csv")

