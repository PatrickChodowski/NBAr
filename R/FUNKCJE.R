#' @import RPostgreSQL
#' @import XML
#' @import dplyr
#' @import jsonlite
#' @importFrom lubridate minutes
#' @importFrom lubridate hours
#' @importFrom lubridate seconds
#' @import stringr
#' @import zoo
#' @export writePG
#' @export getLastGame
#' @export getDoneGames
#' @export checkActual
#' @export getPlayers
#' @export getPgData
#' @export getShotchart
#' @export getBoxscoreAdv
#' @export doBoxscore
#' @export getBoxscoreMisc
#' @export getBoxscorePt
#' @export getBoxscoreTrad
#' @export cleanShotchart
#' @export GSISummary
#' @export getGSI
#' @export getGarbageTime
#' @export getPlaybyplay
#' @export getPlayerlist
#' @export getPlayerInfo
#' @export getPlaytypePlayer
#' @export getPlaytypeTeam
#' @export getSchedule
#' @export getNbaSchedule
#' @export getLineups
#'
#'


traditional <- NULL
calendar <- NULL
s5v <- NULL
s5h <- NULL
na.locf <- NULL
teamlist <- NULL
shotchart <- NULL
PLAYER_ID <- NULL
s <- NULL
playbyplay <- NULL
advanced <- NULL
misc <- NULL
ptracking <- NULL
eff_area <- NULL
eff_type <- NULL

writePG <- function(data,schemat ="rd"){
  on.exit(dbDisconnect(con))
  tryCatch({
    dane<- readRDS(data)
    name <- gsub(".//nba16//","",data)
    name <- gsub(".RDS","",name)
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = "NBA16", host = "localhost", port = 5432, user = "postgres", password = "postgres")
    if(name %in% c("traditional","advanced","misc","ptracking","playbyplay")){
      dbWriteTable(con, c(schemat, name), value = dane, overwrite = FALSE, row.names = FALSE, append = T)

    }else{
      dbWriteTable(con, c(schemat, name), value = dane, overwrite = T, row.names = FALSE, append = F)
    }
    if(name == "shotchart"){
      dbSendQuery(con, paste("CREATE UNIQUE INDEX uix_shc ON rd.shotchart
                             USING btree
                             (\"GAME_ID\" COLLATE pg_catalog.\"default\",
                             \"PLAYER_ID\" COLLATE pg_catalog.\"default\",
                             \"GAME_EVENT_ID\" COLLATE pg_catalog.\"default\");",sep=""))
    }
  },error = function(err){
    print(paste("No table", sep=" "))
  })
}

getLastGame <- function(date = Sys.Date()-1){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "NBA16", host = "localhost", port = 5432, user = "postgres", password = "postgres")
  query <- paste("select max(\"GAME_ID\") from rd.calendar where date = \'",date,"\'",sep="")
  return(as.character(dbGetQuery(con, query)))

}

getDoneGames <- function(date = Sys.Date()-1){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "NBA16", host = "localhost", port = 5432, user = "postgres", password = "postgres")
  query <- paste("select \"GAME_ID\" from rd.calendar where date <= \'",date,"\'",sep="")
  return(as.character(unlist(as.list(dbGetQuery(con, query)))))
}

checkActual <- function(table,date = Sys.Date()-1){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "NBA16", host = "localhost", port = 5432, user = "postgres", password = "postgres")
  query <- paste("select distinct \"GAME_ID\" from rd.",table,sep="")
  #return(as.character(unlist(as.list(dbGetQuery(con, query)))))
  gms <- as.character(unlist(as.list(dbGetQuery(con, query))))
  rez <- match(getDoneGames(),gms)
  if(any(is.na(rez) == T)){
    return(getDoneGames()[is.na(rez) == T])
  }else{
    return(NULL)
  }
}

getPlayers <- function(x){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = "NBA16", host = "localhost", port = 5432, user = "postgres", password = "postgres")
  query <- paste("select distinct \"PLAYER_ID\" from rd.playerlist",sep="")
  pls <- as.character(unlist(as.list(dbGetQuery(con, query))))
  return(pls)
}

getPgData <- function(db, schema){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = db, host = "localhost", port = 5432, user = "postgres", password = "postgres")
  query <-paste("select table_name FROM information_schema.tables where table_schema = \'",schema,"\'",sep="")
  tbs <- as.character(unlist(as.list(dbGetQuery(con, query))))
  #return(sapply(tbs,FUN = function(x) dbReadTable(con, c(schema, x))))
  #return(tbs)
  i <- 1
  for(i in 1:length(tbs))
  {
    assign(tbs[i],dbReadTable(con, c("rd",tbs[i])), pos =.GlobalEnv)
  }

}


getShotchart <- function(playerID, season = "2016-17"){
  url <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=",season,"&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlusMinus=N&PlayerPosition=&Rank=N&RookieYear=&Season=",season,"&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep="")
  web_page <- readLines(url,warn = F)
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", ",",x3, perl=TRUE)
  shot_chart<-read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE, row.names = NULL)
  shot_chart2 <- shot_chart[shot_chart$GRID_TYPE == 'Shot Chart Detail', -c(1,22)]
  return(shot_chart2)
}



getBoxscoreAdv <- function(gameID){
  require(lubridate)
  url <- paste("http://stats.nba.com/stats/boxscoreadvancedv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=2016-17&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")
  web_page <- readLines(url,warn = F)
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", "|",x3, perl=TRUE)
  bs<- read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE)
  bsb <- bs[ grepl("[[:digit:]]", bs$PLAYER_ID) == TRUE,  names(bs) != "X"]
  bsb[bsb[,] == "null"] = NA
  bsb[bsb[,] == ""] = NA
  bsb[,10:ncol(bsb)] <- sapply(bsb[,10:ncol(bsb)], as.numeric)
  bsb$MINS <- as.numeric(minute(ms(bsb[,9])))
  bsb$SECS <- as.numeric(second(ms(bsb[,9])))
  bsb <- bsb[,-9]
  return(bsb)
}

doBoxscore <- function(traditional=traditional, advanced = advanced, misc = misc, ptracking=ptracking){
  require(dplyr)
  boxscore_all <- left_join(traditional, advanced)
  boxscore_all <- left_join(boxscore_all, misc)
  boxscore_all <- left_join(boxscore_all, ptracking)
  return(boxscore_all)
}


getBoxscoreMisc <- function(gameID){
  require(lubridate)
  url <- paste("http://stats.nba.com/stats/boxscoremiscv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=2016-17&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")
  web_page <- readLines(url,warn = F)
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", "|",x3, perl=TRUE)
  bs<- read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE)
  bsb <- bs[ grepl("[[:digit:]]", bs$PLAYER_ID) == TRUE,  names(bs) != "X"]
  bsb[bsb[,] == "null"] = NA
  bsb[bsb[,] == ""] = NA
  bsb[,10:ncol(bsb)] <- sapply(bsb[,10:ncol(bsb)], as.numeric)
  bsb$MINS <- as.numeric(minute(ms(bsb[,9])))
  bsb$SECS <- as.numeric(second(ms(bsb[,9])))
  bsb <- bsb[,-9]
  return(bsb)
}



getBoxscorePt <- function(gameID){
  require(lubridate)
  url <- paste("http://stats.nba.com/stats/boxscoreplayertrackv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=2016-17&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")
  web_page <- readLines(url,warn = F)
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", "|",x3, perl=TRUE)
  bs<- read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE)
  bsb <- bs[ grepl("[[:digit:]]", bs$PLAYER_ID) == TRUE,  names(bs) != "X"]
  bsb[bsb[,] == "null"] = NA
  bsb[bsb[,] == ""] = NA
  bsb[,10:ncol(bsb)] <- sapply(bsb[,10:ncol(bsb)], as.numeric)
  bsb$MINS <- as.numeric(minute(ms(bsb[,9])))
  bsb$SECS <- as.numeric(second(ms(bsb[,9])))
  bsb <- bsb[,-9]
  return(bsb)
}


getBoxscoreTrad <- function(gameID){
  require(lubridate)
  url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=2016-17&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")
  web_page <- readLines(url,warn = F)
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", "|",x3, perl=TRUE)
  bs<- read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE)
  bsb <- bs[ grepl("[[:digit:]]", bs$PLAYER_ID) == TRUE,  names(bs) != "X"]
  bsb[bsb[,] == "null"] = NA
  bsb[bsb[,] == ""] = NA
  bsb$MINS <- as.numeric(minute(ms(bsb[,9])))
  bsb$SECS <- as.numeric(second(ms(bsb[,9])))
  bsb[,10:28] <- sapply(bsb[,10:28], as.numeric)
  bsb$MINS <- as.numeric(minute(ms(bsb[,9])))
  bsb$SECS <- as.numeric(second(ms(bsb[,9])))
  bsb <- bsb[,-9]
  return(bsb)
}




cleanShotchart <- function(s=shotchart){

  #my god.. czyszczenie
  s[s$ACTION_TYPE == "Hook Bank Shot",c("ACTION_TYPE")] = "Hook Shot"
  s[s$ACTION_TYPE == "Jump Bank Shot",c("ACTION_TYPE")] = "Jump Shot"
  s[s$ACTION_TYPE == "Cutting Dunk Shot",c("ACTION_TYPE")] = "Cutting Layup Shot"
  s[s$ACTION_TYPE %in% c("Cutting Finger Roll Layup Shot","Finger Roll Layup Shot"), c("ACTION_TYPE")] = "Cutting Layup Shot"
  s[s$ACTION_TYPE %in% c("Putback Dunk Shot","Tip Layup Shot","Putback Layup Shot","Tip Dunk Shot"), c("ACTION_TYPE")] = "Putback Shot"
  s[s$ACTION_TYPE %in% c("Driving Finger Roll Layup Shot","Driving Dunk Shot"), c("ACTION_TYPE")] = "Driving Layup Shot"
  s[s$ACTION_TYPE %in% c("Running Finger Roll Layup Shot","Running Dunk Shot","Running Layup Shot"), c("ACTION_TYPE")] = "Running Shot"
  s[s$ACTION_TYPE %in% c("Step Back Jump shot","Fadeaway Jump Shot","Turnaround Fadeaway shot"), c("ACTION_TYPE")] = "Forced jump Shot"
  s$SHOT_ZONE_BASIC[s$SHOT_ZONE_BASIC == 'In The Paint (Non-RA)' & s$SHOT_ZONE_RANGE == "Less Than 8 ft."] <- "Paint"
  s$SHOT_ZONE_BASIC[s$SHOT_ZONE_BASIC == 'In The Paint (Non-RA)' & s$SHOT_ZONE_RANGE == "8-16 ft."] <- "Mid-Range"
  s$SHOT_ZONE_BASIC[s$SHOT_ZONE_BASIC == 'Mid-Range' &  s$SHOT_ZONE_RANGE =="16-24 ft."] <- "Long-Range"
  s$SHOT_ZONE_BASIC[s$SHOT_TYPE == '3PT Field Goal'] <- "Three"
  s[s$ACTION_TYPE %in% c("Turnaround Jump Shot") & s$SHOT_ZONE_BASIC %in% c("Long-Range","Three"), c("ACTION_TYPE")] = "Forced jump Shot"
  s[s$SHOT_ZONE_BASIC %in% c("Right Corner 3", "Left Corner 3", "Above The Break 3","Above the Break 3"), c("SHOT_TYPE")] = "3PT Field Goal"
  s[s$SHOT_ZONE_BASIC %in% c("Right Corner 3", "Left Corner 3", "Above The Break 3","Above the Break 3"), c("SHOT_ZONE_BASIC")] = "Three"
  s[s$ACTION_TYPE %in% c("Alley Oop Dunk Shot","Alley Oop Layup Shot","Alley Oop Layup shot"), c("ACTION_TYPE")] = "Alley Oop"
  s[s$ACTION_TYPE %in% c("Alley Oop Dunk Shot","Alley Oop Layup Shot","Alley Oop Layup shot"), c("ACTION_TYPE")] = "Alley Oop"
  s[s$ACTION_TYPE %in% c("Pullup Bank shot"), c("ACTION_TYPE")] = "Pullup Jump shot"
  s[s$ACTION_TYPE %in% c("Turnaround Bank Hook Shot"), c("ACTION_TYPE")] = "Turnaround Hook Shot"
  assign("shotchart_cs",s, envir = .GlobalEnv)
}

GSISummary <- function(pid, col1 = "ACTION_TYPE", col2 = "EVENT_TYPE"
                       , col3 = "SHOT_ZONE_BASIC" , col4 = "SHOT_ZONE_AREA", s = shotchart_cs){
  require(dplyr)
  sp <- s[s$PLAYER_ID %in% pid,]
  if(col3 != "SHOT_ZONE_BASIC"){
    col3 = "EVENT_TYPE"
  }
  if(col4 != "SHOT_ZONE_AREA"){
    col4 = "EVENT_TYPE"
  }
  if(col1 != "ACTION_TYPE"){
    col1 = "EVENT_TYPE"
  }
  if(col2 != "EVENT_TYPE"){
    col2 = "EVENT_TYPE"
  }

  shots <- as.data.frame(sp %>%
                           group_by_(col1,col2,col3,col4) %>%
                           filter(PLAYER_ID %in% pid) %>%
                           summarise(n = n()))

  jv<- c(col1, col2, col3,col4)
  join_vars <- jv[jv != "EVENT_TYPE"]

  m <- split.data.frame(shots, shots$EVENT_TYPE)
  colnames(m$`Made Shot`)[ncol(m$`Made Shot`)] <- "MADE"
  colnames(m$`Missed Shot`)[ncol(m$`Missed Shot`)] <- "MISSED"
  shots <- full_join(m$`Made Shot`,m$`Missed Shot`, by = join_vars)
  shots <- shots[,-which(names(shots) %in% c("EVENT_TYPE.x","EVENT_TYPE.y"))]
  shots[is.na(shots) == T] =0
  shots$freq <- as.numeric((rowSums(shots[,c("MADE","MISSED")]) / (sum(shots$MADE) +  sum(shots$MISSED))))
  shots[nrow(shots)+1,c("MADE","MISSED")] <- colSums(shots[,c("MADE","MISSED")])
  shots[nrow(shots),1] <- "TOTALS"
  shots$Totals <- rowSums(shots[,c("MADE","MISSED")])
  shots$efficiency <- as.numeric(format(shots$MADE/shots$Totals,digits=3, decimal.mark="."))
  return(shots)
}


getGSI <- function(pid, s=shotchart_cs){
  #tryCatch({
    ##################GSI
    pid <<- pid
    shots <- GSISummary(pid = pid, s=shotchart_cs)
    gms <- length(unique(s[s$PLAYER_ID %in% pid,"GAME_ID"]))  #liczba spotkan
    cutoff <- quantile(shots$Totals, c(0.50)) #czestosc oddawania rzutu

    shots2 <- full_join(shots,eff_area[,c("SHOT_ZONE_AREA","SHOT_ZONE_BASIC","efficiency")], by = c("SHOT_ZONE_AREA","SHOT_ZONE_BASIC"))
    colnames(shots2)[c(8,9)] <- c("efficiency_player", "efficiency_overall")
    shots2 <-  shots2[is.na(shots2$ACTION_TYPE) == FALSE,]
    shots2$GOOD_FLAG <- ifelse(shots2$Totals >= cutoff & shots2$efficiency_player >= shots2$efficiency_overall,1,0)


    rzuty <- unique(shots2[shots2$Totals < cutoff & shots2$efficiency_player >= shots2$efficiency_overall,"ACTION_TYPE"])
    n <- 1
    if (length(rzuty) != 0)
    {
      for (n in 1:length(rzuty))
      {
        if (sum(shots2[shots2$ACTION_TYPE == rzuty[n],"Totals"]) >= cutoff){
          a <- sum(shots2[shots2$ACTION_TYPE == rzuty[n],"Made"], na.rm = TRUE)
          b <- sum(shots2[shots2$ACTION_TYPE == rzuty[n],"Totals"], na.rm = TRUE)
          c <- eff_type[eff_type$ACTION_TYPE == rzuty[n],"efficiency"]
          shots2$GOOD_FLAG <- ifelse(shots2$ACTION_TYPE == rzuty[n] & a/b >= c &
                                       shots2$Totals < cutoff & shots2$efficiency_player >= shots2$efficiency_overall
                                     , 1, shots2$GOOD_FLAG)

        }
      }}
    shots2$GOOD_FLAG <- ifelse(shots2$Totals >= cutoff & shots2$SHOT_ZONE_BASIC == 'Restricted Area' & shots2$efficiency_player >= 0.500,1,shots2$GOOD_FLAG)
    shots2$PLAYER_ID <- pid
    shots4 <- shots2[-nrow(shots2),c("PLAYER_ID","ACTION_TYPE","SHOT_ZONE_BASIC","SHOT_ZONE_AREA","GOOD_FLAG")]
    return(shots4)}#,error = function(err){
      #return(NULL)
    #})
#}



getGarbageTime <- function(p=playbyplay, s=shotchart){
  require(dplyr)

  p$EVENTNUM <- as.numeric(p$EVENTNUM)
  s$GAME_EVENT_ID <- as.numeric(s$GAME_EVENT_ID)


  gtime <- left_join(p[p$EVENTMSGTYPE %in% c(1,2),], s, by =c("PERIOD" = "PERIOD",
                                                              "GAME_ID"="GAME_ID","MINS"= "MINUTES_REMAINING", "SECS"="SECONDS_REMAINING",
                                                              "EVENTNUM"="GAME_EVENT_ID"))

  gtime$SCOREMARGIN <- as.numeric(ifelse(gtime$SCOREMARGIN =='TIE'
                                         | (gtime$PERIOD == 1 & gtime$MINS == 12),0,gtime$SCOREMARGIN))
  gtime$abscore <- abs(gtime$SCOREMARGIN)
  gtime$garbage <- ifelse((gtime$PERIOD == 3 & gtime$abscore >= 25) |
                            (gtime$PERIOD == 4 & gtime$abscore >= 25 & gtime$MINS <10)|
                            (gtime$PERIOD == 4 & gtime$abscore >= 22 & gtime$MINS <8)|
                            (gtime$PERIOD == 4 & gtime$abscore >= 20 & gtime$MINS <6)|
                            (gtime$PERIOD == 4 & gtime$abscore >= 15 & gtime$MINS <4),1,0)
  return(gtime)

}





getPlaybyplay <- function(gameID){
  require(lubridate)
  require(stringr)
  require(dplyr)
  require(zoo)

  url <- paste("http://stats.nba.com/stats/playbyplay?GameID=", gameID, "&StartPeriod=0&EndPeriod=14", sep="")
  web_page <- readLines(url, warn = F)
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", ",",x3, perl=TRUE)
  bs<-read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE)
  bsb <- bs[bs$GAME_ID == gameID,  names(bs) != "X"]
  bsb[bsb[,] == "null"] = NA
  bsb[bsb[,] == ""] = NA
  bsb$MINS <- as.numeric(minute(ms(bsb$PCTIMESTRING)))
  bsb$SECS <- as.numeric(second(ms(bsb$PCTIMESTRING)))
  bsb[1,c("SCORE","SCOREMARGIN")] <- c("0 - 0",0)
  bsb[,11:12] <- apply(bsb[,11:12],2,na.locf)
  return(bsb)
}



getPlayerlist <- function(season = "2016-17"){
  require(dplyr)
  url  <- paste("http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=",season,"&IsOnlyCurrentSeason=1%20", sep="")
  web_page <- readLines(url, warn = F)
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", ",",x3, perl=TRUE)

  playerlist<-read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE)
  playerlist<-playerlist[,c(1,3,4,5,6,8,10,11,13)]
  playerlist$PERSON_ID <- as.character(playerlist$PERSON_ID)
  playerlist$TEAM_ID <- as.character(playerlist$TEAM_ID)
  colnames(playerlist)[1] <- "PLAYER_ID"
  return(playerlist)
}



getPlayerInfo <- function(playerID){
  require(jsonlite)
  require(dplyr)
  tryCatch({
    url <- paste("http://stats.nba.com/stats/commonplayerinfo?LeagueID=00&PlayerID=",playerID,"&SeasonType=Regular+Season",sep="")
    web_page <- readLines(url, warn = F)
    x1 <- fromJSON(web_page)
    x2 <- x1[[3]]
    hx2 <-as.data.frame(unlist(x2$headers))
    hx2$v <- as.data.frame(unlist(x2$rowSet))
    cols <- c("DISPLAY_FIRST_LAST","BIRTHDATE", "SCHOOL"
              ,"COUNTRY","HEIGHT","WEIGHT","SEASON_EXP","POSITION","TEAM_ABBREVIATION","PLAYER_ID")
    colnames(hx2) <- c("attribute","value")
    x3 <- as.data.frame(t(hx2[hx2$attribute %in% cols, "value"]),stringsAsFactors = F)
    colnames(x3) <- cols
    x3$WEIGHT <- as.numeric(x3$WEIGHT)
    x3$HEIGHT <- round(
      (as.numeric(substr(x3$HEIGHT, 1, 1))*30.5+
         as.numeric(substr(x3$HEIGHT, 3, 5))*2.54
      ),digits =0)
    return(x3)},error = function(err){
      return(NULL)
    })
}



getPlaytypePlayer <- function(playtype, what = "offensive", season = "2016"){

  if(playtype %in% c("PostTouch","ElbowTouch","PaintTouch","SpeedDistance","CatchShoot",
                     "Defense","Drives","Passing","Possessions","PullUpShot","Rebounding","Efficiency")){
    url <- paste("http://stats.nba.com/stats/leaguedashptstats?College=&Conference=&%20Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=%20&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0%20&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=",playtype,"&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&StarterBench=%20&TeamID=0&VsConference=&VsDivision=&Weight=", sep = "")
    web_page <- readLines(url, warn = F)
    x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
    x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
    x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
    x4 <- gsub(";", ",",x3, perl=TRUE)
    nba<-read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE)
  }

  if(playtype %in% c("Isolation","Transition","Postup","PRBallHandler","PRRollman","Spotup",
                     "Handoff","Cut","OffScreen","OffRebound")){
    url <- paste("http://stats-prod.nba.com/wp-json/statscms/v1/synergy/player/?category=",playtype,"&limit=500&name=",what,"&q=2452161&season=",season,"&seasonType=Reg", sep ="")
    web_page <- readLines(url, warn = F)
    nba <- as.data.frame(fromJSON(web_page),stringsAsFactors = FALSE)
  }

  ###yeah I know, but I need specified columns for each table

  if(playtype == "CatchShoot"){getcols <- c("PLAYER_ID","PLAYER_NAME","CATCH_SHOOT_FGM","CATCH_SHOOT_FGA",
                                            "CATCH_SHOOT_FG_PCT",  "CATCH_SHOOT_PTS", "CATCH_SHOOT_FG3M", "CATCH_SHOOT_FG3A","CATCH_SHOOT_FG3_PCT", "CATCH_SHOOT_EFG_PCT")}
  if(playtype == "Drives"){getcols <- c("PLAYER_ID","PLAYER_NAME","DRIVE_FGM","DRIVE_FGA",
                                        "DRIVES","DRIVE_FTM","DRIVE_FTA","DRIVE_PTS","DRIVE_PASSES","DRIVE_AST","DRIVE_TOV","DRIVE_PF" )}
  if(playtype == "ElbowTouch"){getcols <- c("PLAYER_ID","PLAYER_NAME","TOUCHES","ELBOW_TOUCHES", "ELBOW_TOUCH_FGM","ELBOW_TOUCH_FGA" ,
                                            "ELBOW_TOUCH_FTM","ELBOW_TOUCH_FTA","ELBOW_TOUCH_PTS" ,"ELBOW_TOUCH_PASSES" ,"ELBOW_TOUCH_AST", "ELBOW_TOUCH_TOV" ,"ELBOW_TOUCH_FOULS" )}
  if(playtype == "PaintTouch"){getcols <- c("PLAYER_ID","PLAYER_NAME","PAINT_TOUCH_FGM","PAINT_TOUCH_FGA","TOUCHES"
                                            ,"PAINT_TOUCHES","PAINT_TOUCH_FTA","PAINT_TOUCH_FTM","PAINT_TOUCH_PTS","PAINT_TOUCH_PASSES","PAINT_TOUCH_AST","PAINT_TOUCH_TOV","PAINT_TOUCH_FOULS")}
  if(playtype == "PostTouch"){getcols <- c("PLAYER_ID","PLAYER_NAME","TOUCHES","POST_TOUCHES","POST_TOUCH_FGM","POST_TOUCH_FGA","POST_TOUCH_FTA","POST_TOUCH_FTM",
                                           "POST_TOUCH_PTS","POST_TOUCH_PASSES","POST_TOUCH_AST","POST_TOUCH_TOV","POST_TOUCH_FOULS")}
  if(playtype == "PullUpShot"){getcols <- c("PLAYER_ID","PLAYER_NAME","PULL_UP_FGM","PULL_UP_FGA","PULL_UP_FG3M","PULL_UP_FG3A","PULL_UP_PTS")}

  if(playtype %in% c("Cut","PRRollman","PRBallHandler","Transition",
                     "Spotup","OffScreen","Isolation","Handoff","OffRebound","Postup")){getcols <- c("results.PlayerIDSID", "results.Poss" ,"results.FGA",
                                                                                                     "results.FGM","results.Points" ,"results.PlusOne" ,"results.FT","results.TO","results.SF")
                     nba <- nba[,getcols]
                     cols <- c("POSS_", "FGA_","FGM_","POINTS_","PLUSONE_","FT_","TO_","SF_")
                     cols <- paste(cols, toupper(playtype), sep="")
                     cold <- append("PLAYER_ID", cols)
                     colnames(nba) <- cold
                     assign(paste("DF_",playtype,sep=""),nba)
                     return(get(paste("DF_",playtype,sep="")))
  }


  if(playtype %in% c("PostTouch","ElbowTouch","PaintTouch","SpeedDistance","CatchShoot",
                     "Defense","Drives","Passing","Possessions","PullUpShot","Rebounding","Efficiency")){
    nba <- nba[,getcols]
    assign(paste("DF_",playtype,sep=""),nba)
    return(get(paste("DF_",playtype,sep="")))
  }
}





getPlaytypeTeam <- function(playtype, what = "defensive", season = "2016"){

  if(playtype %in% c("Isolation","Transition","Postup","PRBallHandler","PRRollman","Spotup",
                     "Handoff","Cut","OffScreen","OffRebound")){
    url <- paste("http://stats-prod.nba.com/wp-json/statscms/v1/synergy/team/?category=",playtype,"&limit=500&name=",what,"&q=2452161&season=",season,"&seasonType=Reg",sep="")
    web_page <- readLines(url, warn = F)
    nba <- as.data.frame(fromJSON(web_page),stringsAsFactors = FALSE)

    nba <- nba[,c(2,4,7,9,10,11,23,24,26)]
    cols <- c("ABR_", "POSS_","POINTS_","FGA_","FGM_","FT_","TO_","PLUSONE_")
    cols <- paste(cols, toupper(playtype), sep="")
    cold <- append("TEAM_ID", cols)
    colnames(nba) <- cold
    assign(paste("TEAM_",playtype,sep=""),nba)
    return(get(paste("TEAM_",playtype,sep="")))

  }
}





mo2Num <- function(x) match(tolower(x), tolower(month.abb))

getSchedule <- function(month){
  require(XML)
  require(stringr)
  require(jsonlite)
  doc.html <- htmlTreeParse(
    paste('http://www.basketball-reference.com/leagues/NBA_2017_games-',month,'.html',sep=""),useInternal = TRUE)
  s <- as.data.frame(readHTMLTable(doc.html,stringsAsFactors= F),stringsAsFactors =F)
  s <- s[,c(1,3,4,5,6,8)]
  s$d <- substr(s$date, 6, length(s$date))
  s$d1 <- substr(s$d, 1, 3)
  s$d1 <- mo2Num(s$d1)
  s$d1 <- ifelse(nchar(s$d1) == 1, paste("0",s$d1,sep=""),s$d1)
  s$d2 <- substr(s$d, 5, 6)
  s$d2 <- ifelse(grepl(",",s$d2) == TRUE, paste("0",substr(s$d2,1,1),sep=""),s$d2)
  s$d3 <- str_sub(s$d, -4)
  s$date <- as.Date(paste(s$d3,s$d1, s$d2, sep="-"))
  s <- s[,1:6]
  s$vpts <- as.numeric(s$vpts)
  s$hpts <- as.numeric(s$hpts)
  return(s)
}

getNbaSchedule <- function(dt){
  require(XML)
  require(stringr)
  require(jsonlite)
  url <- paste("http://data.nba.com/data/10s/json/cms/noseason/scoreboard/",dt,"/games.json", sep ="")
  web_page <- readLines(url, warn = F)
  tb <- as.data.frame(fromJSON(web_page),stringsAsFactors = F)
  basic <- unique(tb[,c(18,15)])
  colnames(basic) <- c("dateid","GAME_ID")
  home <- tb$sports_content.games.game.home
  home$TEAM_NAME <- paste(home$city, home$nickname, sep=" ")
  visit <- tb$sports_content.games.game.visitor
  visit$TEAM_NAME <- paste(visit$city, visit$nickname, sep=" ")
  basic$home <- home$TEAM_NAME
  basic$visitor <- visit$TEAM_NAME
  return(basic)
}


getLineups <- function(gameID, pb = playbyplay, s = calendar, t = traditional, tl=teamlist){
  require(dplyr)
  require(zoo)
  require(jsonlite)
  pb<- pb[pb$GAME_ID == gameID,]
  t<- t[t$GAME_ID == gameID,]
  names<- unlist(lapply(unique(t[t$GAME_ID == gameID, "PLAYER_NAME"]),FUN = function(x) trimws(gsub("^\\S+","",x))))
  habr <- tl[tl$TEAM_NAME == s[s$GAME_ID == gameID, "home"],"abr"]
  vabr <- tl[tl$TEAM_NAME == s[s$GAME_ID == gameID, "visitor"],"abr"]
  s5v<- t[is.na(t$START_POSITION) == F & t$TEAM_ABBREVIATION == vabr, "PLAYER_NAME"]
  s5h<- t[is.na(t$START_POSITION) == F & t$TEAM_ABBREVIATION == habr, "PLAYER_NAME"]
  names<- unlist(lapply(unique(t[t$GAME_ID == gameID, "PLAYER_NAME"]),FUN = function(x) trimws(gsub("^\\S+","",x))))
  s5v <- unlist(lapply(s5v,FUN = function(x) trimws(gsub("^\\S+","",x))))
  s5h <- unlist(lapply(s5h,FUN = function(x) trimws(gsub("^\\S+","",x))))
  who <- "HOMEDESCRIPTION"
  test <- pb[pb$EVENTMSGTYPE %in% c(8,12) & pb$GAME_ID == gameID, append("HOMEDESCRIPTION",c("EVENTMSGTYPE","PERIOD","EVENTNUM"))]
  test <- test[is.na(test[,1]) == F | test$EVENTMSGTYPE == 12,]
  test[,5:9] <- NA
  if(who == "HOMEDESCRIPTION"){
    colnames(test)[5:9] <- c("A_home","B_home","C_home","D_home","E_home")
  }else{
    colnames(test)[5:9] <- c("A_visit","B_visit","C_visit","D_visit","E_visit")}
  if(colnames(test)[1] == "VISITORDESCRIPTION"){
    test[,5:9] <- as.data.frame(t(s5v),stringsAsFactors = F)
  }else{
    test[,5:9] <- as.data.frame(t(s5h),stringsAsFactors = F)}
  n <- 1
  for (n in 2:nrow(test))
  {
    schodzi <- unlist(strsplit(test[n,1]," "))[4]
    wchodzi <- unlist(strsplit(test[n,1]," "))[2]
    test[ n:nrow(test),which(test[n,] == schodzi)] <- wchodzi
    if(test[n,"EVENTMSGTYPE"] == 12){
      test[n,5:9] <- NA
      if (length(which(test[n+1:nrow(test),"EVENTMSGTYPE"] == 12)) == 0){
        nextp <- nrow(test)
      }else{
        nextp <- min(which(test[n+1:nrow(test),"EVENTMSGTYPE"] == 12 ) +n)}
      fors <- lapply(test[(n+1):nextp,1],FUN = function(x) unlist(strsplit(x," "))[4])
      subs <- lapply(test[(n+1):nextp,1],FUN = function(x) unlist(strsplit(x," "))[2])
      fors <- na.omit(unlist(fors))
      subs <- na.omit(unlist(subs))
      period <- test[n,"PERIOD"]
      if(length(setdiff(subs,setdiff(subs, fors))) != 5){
        known <- unique(append(fors, subs))
        pat <- setdiff(names,known)
        pattern <- paste(pat,collapse="|")
        unknown <- na.omit(pb[pb$PERIOD == period, who])
        aaa<- unlist(lapply(unknown,strsplit, " "))
        irving <- unique(gsub("\\(","",aaa[grepl(pattern, aaa)]))}
      test[n:nrow(test),5] <- unlist(fors[1])
      y <- 2
      while(is.na(test[n,6]) == TRUE)
      {test[n:nrow(test),6] <- ifelse(!unlist(fors[y]) %in% subs[1:y-1],unlist(fors[y]),NA)
      y <- y + 1
      if(y>50){test[n:nrow(test),6] <- setdiff(irving, test[n,5:9])[1]}
      }
      while(is.na(test[n,7]) == TRUE)
      {test[n:nrow(test),7] <- ifelse(!unlist(fors[y]) %in% subs[1:y-1],unlist(fors[y]),NA)
      y <- y + 1
      if(y>50){test[n:nrow(test),7] <- setdiff(irving, test[n,5:9])[1]}
      }
      while(is.na(test[n,8]) == TRUE)
      {test[n:nrow(test),8] <- ifelse(!unlist(fors[y]) %in% subs[1:y-1],unlist(fors[y]),NA)
      y <- y + 1
      if(y>50){test[n:nrow(test),8] <- setdiff(irving, test[n,5:9])[1]}
      }
      while(is.na(test[n,9]) == TRUE)
      {test[n:nrow(test),9] <- ifelse(!unlist(fors[y]) %in% subs[1:y-1],unlist(fors[y]),NA)
      y <- y + 1
      if(y>50){test[n:nrow(test),9] <- setdiff(irving, test[n,5:9])[1]}
      }#poczwórny while, dzieki nba#dzieki nba
    }#poczworny while w ifie, dzieki nba
  }#poczworny while w ifie w forze, dzieki nba
  home<-test
  who <- "VISITORDESCRIPTION"
  test <- pb[pb$EVENTMSGTYPE %in% c(8,12) & pb$GAME_ID == gameID, append("VISITORDESCRIPTION",c("EVENTMSGTYPE","PERIOD","EVENTNUM"))]
  test <- test[is.na(test[,1]) == F | test$EVENTMSGTYPE == 12,]
  test[,5:9] <- NA
  if(who == "HOMEDESCRIPTION"){
    colnames(test)[5:9] <- c("A_home","B_home","C_home","D_home","E_home")
  }else{
    colnames(test)[5:9] <- c("A_visit","B_visit","C_visit","D_visit","E_visit")}
  if(colnames(test)[1] == "VISITORDESCRIPTION"){
    test[,5:9] <- as.data.frame(t(s5v),stringsAsFactors = F)
  }else{
    test[,5:9] <- as.data.frame(t(s5h),stringsAsFactors = F)}
  n <- 1
  for (n in 2:nrow(test))
  {
    schodzi <- unlist(strsplit(test[n,1]," "))[4]
    wchodzi <- unlist(strsplit(test[n,1]," "))[2]
    test[ n:nrow(test),which(test[n,] == schodzi)] <- wchodzi
    if(test[n,"EVENTMSGTYPE"] == 12){
      test[n,5:9] <- NA
      if (length(which(test[n+1:nrow(test),"EVENTMSGTYPE"] == 12)) == 0){
        nextp <- nrow(test)
      }else{
        nextp <- min(which(test[n+1:nrow(test),"EVENTMSGTYPE"] == 12 ) +n)}
      fors <- lapply(test[(n+1):nextp,1],FUN = function(x) unlist(strsplit(x," "))[4])
      subs <- lapply(test[(n+1):nextp,1],FUN = function(x) unlist(strsplit(x," "))[2])
      fors <- na.omit(unlist(fors))
      subs <- na.omit(unlist(subs))
      period <- test[n,"PERIOD"]
      if(length(setdiff(subs,setdiff(subs, fors))) != 5){
        known <- unique(append(fors, subs))
        pat <- setdiff(names,known)
        pattern <- paste(pat,collapse="|")
        unknown <- na.omit(pb[pb$PERIOD == period, who])
        aaa<- unlist(lapply(unknown,strsplit, " "))
        irving <- unique(gsub("\\(","",aaa[grepl(pattern, aaa)]))}
      test[n:nrow(test),5] <- unlist(fors[1])
      y <- 2
      while(is.na(test[n,6]) == TRUE)
      {test[n:nrow(test),6] <- ifelse(!unlist(fors[y]) %in% subs[1:y-1],unlist(fors[y]),NA)
      y <- y + 1
      if(y>50){test[n:nrow(test),6] <- setdiff(irving, test[n,5:9])[1]}
      }
      while(is.na(test[n,7]) == TRUE)
      {test[n:nrow(test),7] <- ifelse(!unlist(fors[y]) %in% subs[1:y-1],unlist(fors[y]),NA)
      y <- y + 1
      if(y>50){test[n:nrow(test),7] <- setdiff(irving, test[n,5:9])[1]}
      }
      while(is.na(test[n,8]) == TRUE)
      {test[n:nrow(test),8] <- ifelse(!unlist(fors[y]) %in% subs[1:y-1],unlist(fors[y]),NA)
      y <- y + 1
      if(y>50){test[n:nrow(test),8] <- setdiff(irving, test[n,5:9])[1]}
      }
      while(is.na(test[n,9]) == TRUE)
      {test[n:nrow(test),9] <- ifelse(!unlist(fors[y]) %in% subs[1:y-1],unlist(fors[y]),NA)
      y <- y + 1
      if(y>50){test[n:nrow(test),9] <- setdiff(irving, test[n,5:9])[1]}
      }#poczwórny while, dzieki nba#dzieki nba
    }#poczworny while w ifie, dzieki nba
  }#poczworny while w if
  visit<- test
  ptest<- pb
  ptest <- left_join(ptest,home)
  ptest <- left_join(ptest,visit)
  ptest[,15:24] <- apply(ptest[,15:24],2,zoo::na.locf)
  if(is.na(ptest[1,"SCORE"]) == T){
    ptest[1,c("SCORE","SCOREMARGIN")] = c("0 - 0",0)
    ptest[,11:12] <- apply(ptest[,11:12],2,zoo::na.locf)
  }
  return(ptest)
}




