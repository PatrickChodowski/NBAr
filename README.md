# NBAr

## Version 2.0!

Hey, welcome back after long break! I decided to face the horror of reviewing my old code and make some large refresh of the package. List of changes:

- NBA API now requires non-empty referrer header to provide data, so going from readr::read_lines to httr::GET was very much needed. This discovery was made by Pawel Kapuscinski
- 95% of dependent functions is based on tidyverse packages
- functions_and_argument_names_are_finally_in_snake_case !!!
- Followed best practices about package dependencies (no requires!)
- Added verbose argument to every function to print out the url
- Provided proper data types changes to columns and arguments (no more dreary "2017")
- game_id is now integer and doesn't require 00 before proper id
- get_hustle is old get_hustle_boxscore (I find boxscores much easier to use)
- playtypes and tracking are now working flawlessly due to referral trick


## Overview

How to track NBA data in R 

Download data from NBA.com API



## General

If you are NBA, R and data analytics junkie, this is a package for you! Might be not the most useful tool nor the most exciting, but NBAr contains set of wrapper functions for downloading and simple processing of data from http://stats.nba.com API.

Make your NBA analysis faster and more automated by downloading data straight to your machine !


## Installation

```
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("PatrickChodowski/NBAr")
```

## Functions

Functions below allow you do easily download data about games, players, teams, statistics and advanced measures from NBA.com. You will find boxscores, play-by-play, shotchart, schedule, player Bios, Sport-VU tracking data, playtypes and much much more!
 - get_all_tracking
 - get_boxscore
 - get_defense
 - get_general
 - get_hustle
 - get_lineups
 - get_matchups
 - get_news
 - get_on_off
 - get_playbyplay
 - get_playbyplay2
 - get_player_movement
 - get_playerbio
 - get_players
 - get_playtype
 - get_rotowire_status
 - get_schedule
 - get_shooting
 - get_shooting_dashboard
 - get_shotchart
 - get_team_boxscore
 - get_tracking


[You can read more about that package and my work on my website, Per48.co](https://www.per48.co)


## Example
```

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

 pbp1 <- map(gamelist, ~get_playbyplay(.)) %>% compact() %>%  bind_rows()
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

```
