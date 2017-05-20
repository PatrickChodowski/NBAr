library(NBAr)
library(RODBC)
library(plyr)
#setwd("./NBA")
#devtools::install_github("PatrickChodowski/NBAr")

dbname <- "nba15"
season <- "2015"
gamelist<- paste("00",as.character(c(21500001:21501230)),sep="")

#########################
### Boxscores:
#########################

  ##player tracking
  pbps <- lapply(gamelist, getBoxscore, Type='playertrack')
  pbps <- pbps[lapply(pbps,length)>0]
  ptracking <- do.call(rbind.data.frame,pbps)
  saveRDS(ptracking, file = paste(".//",dbname,"//ptracking.RDS",sep=""))

  ##advanced
  pbps <- lapply(gamelist, getBoxscore, Type='advanced')
  pbps <- pbps[lapply(pbps,length)>0]
  advanced <- do.call(rbind.data.frame,pbps)
  saveRDS(advanced, file = paste(".//",dbname,"//advanced.RDS",sep=""))

  ##misc
  pbps <- lapply(gamelist, getBoxscore, Type='misc')
  pbps <- pbps[lapply(pbps,length)>0]
  misc <- do.call(rbind.data.frame,pbps)
  saveRDS(misc, file = paste(".//",dbname,"//misc.RDS",sep=""))

  ##traditional
  pbps <- lapply(gamelist, getBoxscore, Type='traditional')
  pbps <- pbps[lapply(pbps,length)>0]
  traditional <- do.call(rbind.data.frame,pbps)
  saveRDS(traditional, file = paste(".//",dbname,"//traditional.RDS",sep=""))

  
#########################
### Play by play:
#########################
  
  pbps <- lapply(gamelist, getPlaybyplay)
  pbps <- pbps[lapply(pbps,length)>0]
  playbyplay <- do.call(rbind.data.frame,pbps)
  saveRDS(playbyplay, file = paste(".//",dbname,"//playbyplay.RDS",sep=""))

  
#########################
### Players list
######################### 
 
  players <- getPlayerlist(season)
  saveRDS(players, file = paste(".//",dbname,"//players.RDS",sep=""))
  playerBio <- getPlayerBio(season)   
  saveRDS(playerBio, file = paste(".//",dbname,"//playersBio.RDS",sep=""))

  playerslist <- unique(players$PERSON_ID)
  
#########################
### Shotcharts
#########################   
  
  pbps <- lapply(playerslist, getShotchart, season = season)
  pbps <- pbps[lapply(pbps,length)>0]
  shotchart <- do.call(rbind.data.frame,pbps)
  saveRDS(shotchart, file = paste(".//",dbname,"//shotchart.RDS",sep=""))

  
#########################
### Schedule
#########################  
  
  months <- c("october","november","december","january","february","march","april","may")
  ds.months <- lapply(months,getSchedule, season = season)
  schedule <- do.call(rbind.data.frame,ds.months)
  saveRDS(schedule, file = paste(".//",dbname,"//schedule.RDS",sep=""))
  
  
#########################
### Tracking data
#########################  
  
  Type <- "Player"
  MeasureType <- c("Drives","Defense","CatchShoot","Passing","Possessions","PullUpShot","Rebounding","Efficiency","SpeedDistance","ElbowTouch","PostTouch","PaintTouch")
  
  pbps <- lapply(MeasureType, getTracking, Season = season, Type = Type)
  pbps <- pbps[lapply(pbps,length)>0]
  tracking <- join_all(pbps, by=c('PLAYER_ID','PLAYER_NAME','TEAM_ID','TEAM_ABBREVIATION','GP','W','L','MIN'), type='left')
  saveRDS(tracking, file = paste(".//",dbname,"//tracking.RDS",sep=""))
  
  
#########################
### Playtype data
#########################  
  
  Playtype <- c("Postup","Transition","Isolation","PRBallHandler","PRRollman","Spotup","Handoff","Cut","OffScreen","OffRebound","Misc")

  pbps <- lapply(Playtype, getPlaytype, Season = season, Type = Type)
  pbps <- pbps[lapply(pbps,length)>0]
  playtype <- join_all(pbps, by=c('PlayerIDSID','PlayerFirstName','PlayerLastName','PlayerNumber','TeamIDSID','TeamName','TeamNameAbbreviation','TeamShortName','GP','name','season','seasonType'), type='left')
  saveRDS(playtype, file = paste(".//",dbname,"//playtype.RDS",sep=""))

  
#########################
### Defense data
#########################
  
  DefenseCategory <- c("Overall","3+Pointers","2+Pointers","Less+Than+6Ft","Less+Than+10Ft","Greater+Than+15Ft")
  
  pbps <- lapply(DefenseCategory, getDefense, Season = season, Type = Type)
  pbps <- pbps[lapply(pbps,length)>0]
  defense <- join_all(pbps, by=c('CLOSE_DEF_PERSON_ID','PLAYER_NAME','PLAYER_LAST_TEAM_ID','PLAYER_LAST_TEAM_ABBREVIATION','PLAYER_POSITION','AGE','GP','G'), type='left')
  saveRDS(defense, file = paste(".//",dbname,"//defense.RDS",sep=""))
  
  
#########################
### General data
#########################
  
  MeasureType <- c("Base","Advanced","Misc","Scoring","Usage","Opponent","Defense", "Four Factors")
  
  pbps <- lapply(MeasureType, getGeneral, Season = season, Type = Type)
  
  pbps <- pbps[lapply(pbps,length)>0]
  
  general <- join_all(pbps, by=c("PLAYER_ID","PLAYER_NAME","TEAM_ID","TEAM_ABBREVIATION","AGE","GP"), type='left')
  saveRDS(general, file = paste(".//",dbname,"//general.RDS",sep=""))
  
  
#########################
### Hustle data
#########################
  
  hustle <- getHustle(season, Type = Type)
  saveRDS(hustle, file = paste(".//",dbname,"//hustle.RDS",sep=""))
  
  
#########################
### Shooting data
#########################
  
  shooting <- getShooting(season, Type, "By Zone", "Base")
  saveRDS(shooting, file = paste(".//",dbname,"//shooting.RDS",sep=""))
  

  
      