

library(NBAr)
library(tidyverse)
#devtools::install_github("PatrickChodowski/NBAr")

season <- 2018
gamelist<- c(21800001:21800003)
game_id = 21800001

#########################
### Boxscores:
#########################

traditional <- map(gamelist, ~get_boxscore(.,boxscore_type = 'traditional')) %>% compact() %>%  bind_rows()

#########################
### Matchups
#########################

matchup <- get_matchups(game_id)

#########################
### Play by play:
#########################

pbp2 <- map(gamelist, ~get_playbyplay2(.)) %>% compact() %>%  bind_rows()

#########################
### Lineups:
#########################

lineups <- get_lineups(season,5,'Base')
bulls_lineups <- get_lineups(season,5,'Base', team_id = 1610612741)

#########################
### On/Off court:
#########################

onoff_bulls <- get_on_off(season,1610612741)

#########################
### Players list
#########################

players <- get_players(season)
player_bio <- get_playerbio(season)



#########################
### Shotcharts
#########################

shots_abrines <- get_shotchart(203518, season)

#########################
### Schedule
#########################

schedule <- get_schedule(season)


#########################
### Tracking data
#########################

tracking = get_tracking(season, 'Team', measure_type = 'Defense')


#########################
### Playtype data
#########################

playtype <- get_playtype(season, 'T',  'Postup')


#########################
### Defense data
#########################

defense <- get_defense(season, 'Team', 'Overall')

#########################
### General data
#########################

general <- get_general(season, type = 'Team', 'Base')


#########################
### Hustle data
#########################

hustle <- get_hustle(21800001)

#########################
### Shooting data
#########################

shooting <- get_shooting(season, 'Team', "By+Zone", "Base")
