#' Download data from NBA.com/Stats/ and build your own database in local PostgreSQL instance
#'
#'
#' @import RPostgreSQL
#' @import XML
#' @import dplyr
#' @import jsonlite
#' @importFrom lubridate minutes
#' @importFrom lubridate hours
#' @importFrom lubridate seconds
#' @import stringr
#' @import zoo
#' @import sqldf
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
#' @export getPlaybyplay
#' @export getPlayerlist
#' @export getPlayerInfo
#' @export getPlaytypePlayer
#' @export getPlaytypeTeam
#' @export getSchedule
#' @export getNbaSchedule
#' @export mo2Num
#' @export getESPNpbp
#' @export createLineup


writePG <- function(data, dbname, schemat ="rd"){
  on.exit(dbDisconnect(con))
  tryCatch({
    dane<- readRDS(data)
    name <- gsub(".//nba16//","",data)
    name <- gsub(".RDS","",name)
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = dbname, host = "localhost", port = 5432, user = "postgres", password = "postgres")
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

getLastGame <- function(dbname, date = Sys.Date()-1){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = "localhost", port = 5432, user = "postgres", password = "postgres")
  query <- paste("select max(\"GAME_ID\") from rd.calendar where date = \'",date,"\'",sep="")
  return(as.character(dbGetQuery(con, query)))

}

getDoneGames <- function(dbname,date = Sys.Date()-1){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = "localhost", port = 5432, user = "postgres", password = "postgres")
  query <- paste("select \"GAME_ID\" from rd.calendar where date <= \'",date,"\'",sep="")
  return(as.character(unlist(as.list(dbGetQuery(con, query)))))
}

checkActual <- function(table, dbname, date = Sys.Date()-1){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = "localhost", port = 5432, user = "postgres", password = "postgres")
  query <- paste("select distinct \"GAME_ID\" from rd.",table,sep="")
  #return(as.character(unlist(as.list(dbGetQuery(con, query)))))
  gms <- as.character(unlist(as.list(dbGetQuery(con, query))))
  rez <- match(getDoneGames(dbname = dbname),gms)
  if(any(is.na(rez) == T)){
    return(getDoneGames(dbname = dbname)[is.na(rez) == T])
  }else{
    return(NULL)
  }
}

getPlayers <- function(x, dbname = "NBA16"){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = "localhost", port = 5432, user = "postgres", password = "postgres")
  query <- paste("select distinct \"PLAYER_ID\" from rd.playerlist",sep="")
  pls <- as.character(unlist(as.list(dbGetQuery(con, query))))
  return(pls)
}

getPgData <- function(dbname, schema){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = "localhost", port = 5432, user = "postgres", password = "postgres")
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

doBoxscore <- function(traditional.=traditional, advanced. = advanced, misc. = misc, ptracking.=ptracking){
  require(dplyr)
  boxscore_all <- left_join(traditional., advanced.)
  boxscore_all <- left_join(boxscore_all, misc.)
  boxscore_all <- left_join(boxscore_all, ptracking.)
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
  colnames(s) <- c("date","visitor","vpts","home","hpts","to")
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


getESPNpbp <- function(espnID) {
  link <-
    paste("http://www.espn.com/nba/playbyplay?gameId=", espnID, sep = "")
  docHtml <- htmlTreeParse(link, useInternal = TRUE)
  pbpESPN <- readHTMLTable(docHtml, stringsAsFactors = F)
  quarters <- unname(which(lapply(pbpESPN, nrow) > 20))
  qrts <- pbpESPN[quarters]
  q <- 1
  for (q in 1:length(qrts))
  {
    qrts[[q]]$V2 <- q
  }
  game <- do.call(rbind.data.frame, qrts)

  return(list(game, qrts))
}


createLineup <- function(gameID
                         , calendar. = calendar
                         , espnCalendar. = espnCalendar
                         , traditional. = traditional
                         , teamlist. = teamlist
                         , playbyplay. = playbyplay) {

  require(sqldf)
  require(XML)
  require(dplyr)
  require(zoo)
  joinedCalendar <- left_join(calendar., espnCalendar., by = c("visitor",
                                                               "home", "dateid"))
  habr <- teamlist.[teamlist.$TEAM_NAME == calendar.[calendar.$GAME_ID ==
                                                       gameID, "home"], "TEAM_ABBREVIATION"]
  vabr <- teamlist.[teamlist.$TEAM_NAME == calendar.[calendar.$GAME_ID ==
                                                       gameID, "visitor"], "TEAM_ABBREVIATION"]
  s5v <- traditional.[is.na(traditional$START_POSITION) ==
                        F & traditional.$GAME_ID == gameID & traditional$TEAM_ABBREVIATION ==
                        vabr, "PLAYER_NAME"]
  s5h <- traditional.[is.na(traditional.$START_POSITION) ==
                        F & traditional.$GAME_ID == gameID & traditional.$TEAM_ABBREVIATION ==
                        habr, "PLAYER_NAME"]
  fullv <- traditional.[traditional.$TEAM_ABBREVIATION == vabr &
                          traditional.$GAME_ID == gameID, "PLAYER_NAME"]
  fullh <- traditional.[traditional.$TEAM_ABBREVIATION == habr &
                          traditional.$GAME_ID == gameID, "PLAYER_NAME"]
  espnID <- trimws(joinedCalendar[joinedCalendar$GAME_ID ==
                                    gameID, "espn"])
  espnGame <- getESPNpbp(espnID)
  game <- espnGame[[1]]

  game$MINS <- as.numeric(gsub("\\:.*", "", game$V1))
  game$SECS <- as.numeric(gsub("^.*\\:", "", game$V1))
  game$PERIOD <- as.numeric(game$V2)

  haaa <- data.table::as.data.table(game[game$V1 != "time",])
  haaa[,playID:=data.table::frank(haaa, PERIOD, -MINS,-SECS, ties.method="min")]

  haaa <- haaa[with(haaa, order(playID)),]
  game <- left_join(game,haaa,by = c("V1", "V2", "V3", "V4", "V5", "MINS", "SECS", "PERIOD"))


  ######GET AND DIVIDE ESPN

  #qtrs <- espnGame[[2]]
  game$V3 <- gsub("Frank Kaminsky III","Frank Kaminsky",game$V3)
  subso <- game[grep("enters", game$V3), ]
  subsvisitor <- subso[grep(paste(fullv, collapse = "|"), subso$V3), ]
  subshome <- subso[grep(paste(fullh, collapse = "|"), subso$V3), ]

  #######PREPARE SUBSHOME/SUBSVISITOR
  subshome$fnames <- unlist(lapply(subshome$V3, FUN = function(x) gsub(" enters the game for ", "/", x)))
  subsvisitor$fnames <- unlist(lapply(subsvisitor$V3, FUN = function(x) gsub(" enters the game for ", "/", x)))


  gaaa <- data.table::as.data.table(subshome)
  gaaa[,timeID:=data.table::frank(gaaa, PERIOD, -MINS,-SECS, ties.method="min")]

  gaaa <- gaaa[with(gaaa, order(timeID)),]
  subshome <- left_join(subshome,gaaa,by = c("V1", "V2", "V3", "V4", "V5", "MINS", "SECS", "PERIOD","playID","fnames"))


  faaa <- data.table::as.data.table(subsvisitor)
  faaa[,timeID:=data.table::frank(faaa, PERIOD, -MINS,-SECS, ties.method="min")]

  faaa <- faaa[with(faaa, order(timeID)),]
  subsvisitor <- left_join(subsvisitor,faaa,by = c("V1", "V2", "V3", "V4", "V5", "MINS", "SECS", "PERIOD","playID","fnames"))




  ###### PREPARE PLAYBYPLAY
  who <- "HOMEDESCRIPTION"
  linueps <- playbyplay.[playbyplay.$EVENTMSGTYPE %in% c(8,9,
                                                       12) & playbyplay.$GAME_ID == gameID, append(who, c("EVENTMSGTYPE",
                                                                                                         "PERIOD", "EVENTNUM", "MINS", "SECS", "SCORE"))]
  linueps <- linueps[is.na(linueps[, 1]) == F | linueps$EVENTMSGTYPE == 12 | linueps$EVENTMSGTYPE == 9, ]

  linueps[, 8:12] <- NA
  colnames(linueps)[8:12] <- c("A_home", "B_home", "C_home",
                               "D_home", "E_home")
  linueps[, 8:12] <- as.data.frame(t(s5h), stringsAsFactors = F)
  linueps$snames <- unlist(lapply(linueps$HOMEDESCRIPTION,
                                  FUN = function(x) gsub("SUB: ", "", x)))
  linueps$snames <- unlist(lapply(linueps$snames, FUN = function(x) gsub(" FOR ",
                                                                         "|", x)))
  #####poki co same zmiany
  daaa <- data.table::as.data.table(linueps[linueps$EVENTMSGTYPE == 8,])
  daaa[,timeID:=data.table::frank(daaa, PERIOD, -MINS,-SECS, ties.method="min")]

  daaa <- daaa[with(daaa, order(timeID)),]
  linueps <- left_join(linueps,daaa,by = c("HOMEDESCRIPTION", "EVENTMSGTYPE", "PERIOD", "EVENTNUM", "MINS", "SECS", "SCORE", "A_home", "B_home", "C_home", "D_home", "E_home", "snames"))




  #########################JOIN SUBS AND LINEUPS

  testls <- sqldf("select * from linueps l left join subshome s on l.timeID=s.timeID",
                  stringsAsFactors = F, drv = "SQLite")

  testls <- testls[, -c(14, 20:22)]
  testls <- testls[with(testls, order(PERIOD, -MINS, -SECS)),
                   ]
  tls <- 1
  for (tls in 1:nrow(testls)) {
    testls[tls, "V5"] <- stri_count_regex(testls$fnames[tls],
                                          testls$snames[tls])
  }
  testnc <- testls[testls$V5 == 2 | is.na(testls$V5) == TRUE,
                   c(1:12, 16,19,20,21)]

  ########################LINEUPS IN MAKING
  n <- 1
  for (n in 2:nrow(testnc)) {
    schodzi <- unlist(strsplit(testnc[n, 15], "/"))[2]
    wchodzi <- unlist(strsplit(testnc[n, 15], "/"))[1]
    testnc[n:nrow(testnc), which(testnc[n, ] == schodzi)] <- wchodzi

    if (testnc[n, "EVENTMSGTYPE"] %in% c(12,9)) {
      testnc[n, 8:12] <- NA
      if (length(which(testnc[n + 1:nrow(testnc), "EVENTMSGTYPE"] %in% c(12,9))) == 0) {
        nextp <- nrow(testnc)
      } else {
        nextp <- min(which(testnc[n + 1:nrow(testnc),  "EVENTMSGTYPE"] %in% c(12,9)) + n)
      }
      fors <- lapply(testnc[(n + 1):nextp, 15], FUN = function(x) unlist(strsplit(x,
                                                                                  "/"))[2])
      subs <- lapply(testnc[(n + 1):nextp, 15], FUN = function(x) unlist(strsplit(x,
                                                                                  "/"))[1])
      fors <- na.omit(unlist(fors))
      subs <- na.omit(unlist(subs))

      NonNAindex <- which(!is.na(testnc[n:nrow(testnc),"playID"]))+n
      firstNonNA <- min(NonNAindex)

      playSTART <- testnc[firstNonNA,"playID"]
      NonNAindex <- which(!is.na(testnc[nextp:nrow(testnc),"playID"]))+nextp-1
      firstNonNA <- min(NonNAindex)

      playSTOP <- testnc[firstNonNA,"playID"]



      unknown <- na.omit(game[game$playID > playSTART & game$playID < playSTOP, "V3"])


      period <- testnc[n, "PERIOD"]
      known <- unique(append(fors, subs))
      pat <- setdiff(fullh, known)


      matches <- sapply(pat, grepl, unknown, ignore.case = TRUE)

      if (class(matches) != "list"){
        wholeQuarterPlayed <- names(which(apply(matches, 2, any) == TRUE))
        irving <- append(known, wholeQuarterPlayed)
      }else{
        wholeQuarterPlayed <- as.character()
        irving <- known
      }



      if(length(irving) < 5){
        previousRow <- unname(testnc[n-1,8:12])
        wholeQuarterPlayed <- setdiff(previousRow, fors)
      }


      if (length(wholeQuarterPlayed) > 0) {
        cq <- 1
        for (cq in 1:length(wholeQuarterPlayed)) {
          testnc[n:nrow(testnc), 8 + cq - 1] <- wholeQuarterPlayed[cq]
        }
      }
      y <- 1
      if (length(wholeQuarterPlayed) == 0) {
        while (is.na(testnc[n, 8]) == TRUE) {
          testnc[n:nrow(testnc), 8] <- ifelse(!unlist(fors[y]) %in%
                                                subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
        }
      }
      if (length(wholeQuarterPlayed) <= 1) {
        while (is.na(testnc[n, 9]) == TRUE) {
          testnc[n:nrow(testnc), 9] <- ifelse(!unlist(fors[y]) %in%
                                                subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
        }
      }
      if (length(wholeQuarterPlayed) <= 2) {
        while (is.na(testnc[n, 10]) == TRUE) {
          testnc[n:nrow(testnc), 10] <- ifelse(!unlist(fors[y]) %in%
                                                 subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
        }
      }
      if (length(wholeQuarterPlayed) <= 3) {
        while (is.na(testnc[n, 11]) == TRUE) {
          testnc[n:nrow(testnc), 11] <- ifelse(!unlist(fors[y]) %in%
                                                 subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
        }
      }
      if (length(wholeQuarterPlayed) <= 4) {
        while (is.na(testnc[n, 12]) == TRUE) {
          testnc[n:nrow(testnc), 12] <- ifelse(!unlist(fors[y]) %in%
                                                 subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
          if (y > 15){

            testnc[n:nrow(testnc), 12] <- setdiff(previousRow,testnc[n,8:11])
          }
        }
      }
    }
  }
  home <- testnc


  who <- "VISITORDESCRIPTION"
  linueps <- playbyplay.[playbyplay.$EVENTMSGTYPE %in% c(8,9,
                                                       12) & playbyplay.$GAME_ID == gameID, append(who, c("EVENTMSGTYPE",
                                                                                                         "PERIOD", "EVENTNUM", "MINS", "SECS", "SCORE"))]
  linueps <- linueps[is.na(linueps[, 1]) == F | linueps$EVENTMSGTYPE == 12 | linueps$EVENTMSGTYPE == 9, ]

  linueps[, 8:12] <- NA
  colnames(linueps)[8:12] <- c("A_visit", "B_visit", "C_visit",
                               "D_visit", "E_visit")
  linueps[, 8:12] <- as.data.frame(t(s5v), stringsAsFactors = F)
  linueps$snames <- unlist(lapply(linueps$VISITORDESCRIPTION,
                                  FUN = function(x) gsub("SUB: ", "", x)))
  linueps$snames <- unlist(lapply(linueps$snames, FUN = function(x) gsub(" FOR ",
                                                                         "|", x)))
  #####poki co same zmiany
  daaa <- data.table::as.data.table(linueps[linueps$EVENTMSGTYPE == 8,])
  daaa[,timeID:=data.table::frank(daaa, PERIOD, -MINS,-SECS, ties.method="min")]

  daaa <- daaa[with(daaa, order(timeID)),]
  linueps <- left_join(linueps,daaa,by = c("VISITORDESCRIPTION", "EVENTMSGTYPE", "PERIOD", "EVENTNUM", "MINS", "SECS", "SCORE", "A_visit", "B_visit", "C_visit","D_visit", "E_visit", "snames"))




  #########################JOIN SUBS AND LINEUPS

  testls <- sqldf("select * from linueps l left join subsvisitor s on l.timeID=s.timeID",
                  stringsAsFactors = F, drv = "SQLite")

  testls <- testls[, -c(14, 20:22)]
  testls <- testls[with(testls, order(PERIOD, -MINS, -SECS)),
                   ]
  tls <- 1
  for (tls in 1:nrow(testls)) {
    testls[tls, "V5"] <- stri_count_regex(testls$fnames[tls],
                                          testls$snames[tls])
  }
  testnc <- testls[testls$V5 == 2 | is.na(testls$V5) == TRUE,
                   c(1:12, 16,19,20,21)]

  ########################LINEUPS IN MAKING
  n <- 1
  for (n in 2:nrow(testnc)) {
    schodzi <- unlist(strsplit(testnc[n, 15], "/"))[2]
    wchodzi <- unlist(strsplit(testnc[n, 15], "/"))[1]
    testnc[n:nrow(testnc), which(testnc[n, ] == schodzi)] <- wchodzi

    if (testnc[n, "EVENTMSGTYPE"] %in% c(12,9)) {
      testnc[n, 8:12] <- NA
      if (length(which(testnc[n + 1:nrow(testnc), "EVENTMSGTYPE"] %in% c(12,9))) == 0) {
        nextp <- nrow(testnc)
      } else {
        nextp <- min(which(testnc[n + 1:nrow(testnc),  "EVENTMSGTYPE"] %in% c(12,9)) + n)
      }
      fors <- lapply(testnc[(n + 1):nextp, 15], FUN = function(x) unlist(strsplit(x,
                                                                                  "/"))[2])
      subs <- lapply(testnc[(n + 1):nextp, 15], FUN = function(x) unlist(strsplit(x,
                                                                                  "/"))[1])
      fors <- na.omit(unlist(fors))
      subs <- na.omit(unlist(subs))

      NonNAindex <- which(!is.na(testnc[n:nrow(testnc),"playID"]))+n
      firstNonNA <- min(NonNAindex)

      playSTART <- testnc[firstNonNA,"playID"]
      NonNAindex <- which(!is.na(testnc[nextp:nrow(testnc),"playID"]))+nextp-1
      firstNonNA <- min(NonNAindex)

      playSTOP <- testnc[firstNonNA,"playID"]



      unknown <- na.omit(game[game$playID > playSTART & game$playID < playSTOP, "V3"])


      period <- testnc[n, "PERIOD"]
      known <- unique(append(fors, subs))
      pat <- setdiff(fullv, known)


      matches <- sapply(pat, grepl, unknown, ignore.case = TRUE)

      if (class(matches) != "list"){
        wholeQuarterPlayed <- names(which(apply(matches, 2, any) == TRUE))
        irving <- append(known, wholeQuarterPlayed)
      }else{
        wholeQuarterPlayed <- as.character()
        irving <- known
      }



      if(length(irving) < 5){
        previousRow <- unname(testnc[n-1,8:12])
        wholeQuarterPlayed <- setdiff(previousRow, fors)
      }


      if (length(wholeQuarterPlayed) > 0) {
        cq <- 1
        for (cq in 1:length(wholeQuarterPlayed)) {
          testnc[n:nrow(testnc), 8 + cq - 1] <- wholeQuarterPlayed[cq]
        }
      }
      y <- 1
      if (length(wholeQuarterPlayed) == 0) {
        while (is.na(testnc[n, 8]) == TRUE) {
          testnc[n:nrow(testnc), 8] <- ifelse(!unlist(fors[y]) %in%
                                                subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
        }
      }
      if (length(wholeQuarterPlayed) <= 1) {
        while (is.na(testnc[n, 9]) == TRUE) {
          testnc[n:nrow(testnc), 9] <- ifelse(!unlist(fors[y]) %in%
                                                subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
        }
      }
      if (length(wholeQuarterPlayed) <= 2) {
        while (is.na(testnc[n, 10]) == TRUE) {
          testnc[n:nrow(testnc), 10] <- ifelse(!unlist(fors[y]) %in%
                                                 subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
        }
      }
      if (length(wholeQuarterPlayed) <= 3) {
        while (is.na(testnc[n, 11]) == TRUE) {
          testnc[n:nrow(testnc), 11] <- ifelse(!unlist(fors[y]) %in%
                                                 subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
        }
      }
      if (length(wholeQuarterPlayed) <= 4) {
        while (is.na(testnc[n, 12]) == TRUE) {
          testnc[n:nrow(testnc), 12] <- ifelse(!unlist(fors[y]) %in%
                                                 subs[1:y - 1], unlist(fors[y]), NA)
          y <- y + 1
          if (y > 15){

            testnc[n:nrow(testnc), 12] <- setdiff(previousRow,testnc[n,8:11])
          }
        }
      }
    }
  }

  visit <- testnc
  ptest <- playbyplay.[playbyplay.$GAME_ID == gameID, ]
  ptest <- left_join(ptest, home[, c(1, 4, 8:12)], by = c("EVENTNUM",
                                                          "HOMEDESCRIPTION"))
  ptest <- left_join(ptest, visit[, c(1, 4, 8:12)], by = c("EVENTNUM",
                                                           "VISITORDESCRIPTION"))
  ptest[, 15:24] <- apply(ptest[, 15:24], 2, na.locf)
  if (is.na(ptest[1, "SCORE"]) == TRUE) {
    ptest[1, c("SCORE", "SCOREMARGIN")] = c("0 - 0", 0)
    ptest[, 11:12] <- apply(ptest[, 11:12], 2, na.locf)
  }
  return(ptest)

}




