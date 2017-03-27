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
#' @importFrom data.table frank
#' @importFrom data.table as.data.table
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



writePG <- function(dataset, dbname, schema ="rd", pghost= "localhost",pgport= 5432, pguser = "postgres", pgpassword="postgres"){
  on.exit(dbDisconnect(con))

  tryCatch({

    if(grepl(".RDS",dataset, ignore.case = T) == T){

    data<- readRDS(dataset)
    name <- gsub(".RDS","",dataset)

    } else {

      data <- dataset
      name <- get(dataset)

    }
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = dbname, host = pghost, port = pgport, user = pguser, password = pgpassword)

    if(name %in% c("traditional","advanced","misc","ptracking","playbyplay")){
      dbWriteTable(con, c(schema, dbname), value = data, overwrite = FALSE, row.names = FALSE, append = T)
    }else{
      dbWriteTable(con, c(schema, dbname), value = data, overwrite = T, row.names = FALSE, append = F)
    }

  },error = function(err){
    print(paste("No table", sep=" "))
  })
}



getLastGame <- function(dbname, date = Sys.Date()-1, schema ="rd", pghost= "localhost",pgport= 5432, pguser = "postgres", pgpassword="postgres", calendartbl = "calendar",gamecol = "GAME_ID"){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = pghost, port = pgport, user = pguser, password = pgpassword)
  query <- paste("select max( \'",gamecol,"\') from ",schema,".",calendartbl," where date = \'",date,"\'",sep="")
  return(as.character(dbGetQuery(con, query)))
}



getDoneGames <- function(dbname,date = Sys.Date()-1, schema ="rd", pghost= "localhost",pgport= 5432, pguser = "postgres", pgpassword="postgres", calendartbl = "calendar",gamecol = "GAME_ID"){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = pghost, port = pgport, user = pguser, password = pgpassword)
  query <- paste("select \'",gamecol,"\' from ",schema,".",calendartbl," where date = \'",date,"\'",sep="")
  return(as.character(unlist(as.list(dbGetQuery(con, query)))))
}



checkActual <- function(table, dbname, date = Sys.Date()-1, schema ="rd", pghost= "localhost",pgport= 5432, pguser = "postgres", pgpassword="postgres", calendartbl = "calendar",gamecol = "GAME_ID"){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = pghost, port = pgport, user = pguser, password = pgpassword)
  query <- paste("select distinct \'",gamecol,"\' from ",schema,".",table,sep="")

  gms <- as.character(unlist(as.list(dbGetQuery(con, query))))
  rez <- match(getDoneGames(dbname = dbname),gms)
  if(any(is.na(rez) == T)){
    return(getDoneGames(dbname = dbname, date = date, host = pghost, port = pgport, user = pguser, password = pgpassword, calendartbl = calendartbl)[is.na(rez) == T])
  }else{
    return(NULL)
  }
}



getPlayers <- function(dbname, schema ="rd", pghost= "localhost",pgport= 5432, pguser = "postgres", pgpassword="postgres", playertbl = "playerlist",playercol = "PLAYER_ID"){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = pghost, port = pgport, user = pguser, password = pgpassword)
  query <- paste("select distinct \'",playercol,"\' from ",schema,".",playertbl,sep="")
  players <- as.character(unlist(as.list(dbGetQuery(con, query))))
  return(players)
}


getPgData <- function(dbname, schema, pghost= "localhost",pgport= 5432, pguser = "postgres", pgpassword="postgres"){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = pghost, port = pgport, user = pguser, password = pgpassword)
  query <-paste("select table_name FROM information_schema.tables where table_schema = \'",schema,"\'",sep="")
  tbs <- as.character(unlist(as.list(dbGetQuery(con, query))))

  i <- 1
  for(i in 1:length(tbs))
  {
    assign(tbs[i],dbReadTable(con, c(schema,tbs[i])), pos =.GlobalEnv)
  }

}




getShotchart <- function(playerID, season = "2016-17"){

  tryCatch({

  url <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=",season,"&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&RookieYear=&SeasonSegment=&PlayerPosition=", sep="")
  web_page <- readLines(url,warn = F)
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", ",",x3, perl=TRUE)
  shot_chart<-read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE, row.names = NULL)
  shot_chart2 <- shot_chart[shot_chart$GRID_TYPE == 'Shot Chart Detail', -c(1,22)]
  shot_chart2 <- shot_chart2[,which(colnames(shot_chart2) != "X")]

  return(shot_chart2)}, error=function(e) NULL)
}



getBoxscoreAdv <- function(gameID, season = "2016-17"){
  require(lubridate)
  tryCatch({
  url <- paste("http://stats.nba.com/stats/boxscoreadvancedv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=",season,"&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")
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
  return(bsb)}, error=function(e) NULL)
}



doBoxscore <- function(traditional.=traditional, advanced. = advanced, misc. = misc, ptracking.=ptracking){
  require(dplyr)
  boxscore_all <- left_join(traditional., advanced.)
  boxscore_all <- left_join(boxscore_all, misc.)
  boxscore_all <- left_join(boxscore_all, ptracking.)
  return(boxscore_all)
}


getBoxscoreMisc <- function(gameID, season = "2016-17"){
  require(lubridate)
  tryCatch({
  url <- paste("http://stats.nba.com/stats/boxscoremiscv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=",season,"&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")
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
  return(bsb)}, error=function(e) NULL)
}


getBoxscorePt <- function(gameID, season = "2016-17"){
  require(lubridate)
  tryCatch({
  url <- paste("http://stats.nba.com/stats/boxscoreplayertrackv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=",season,"&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")
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
  return(bsb)}, error=function(e) NULL)
}



getBoxscoreTrad <- function(gameID, season = "2016-17"){
  require(lubridate)
  tryCatch({
  url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=",season,"&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")
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
  return(bsb)}, error=function(e) NULL)
}



getPlaybyplay <- function(gameID){
  require(lubridate)
  require(stringr)
  require(dplyr)
  require(zoo)
  tryCatch({
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
  return(bsb)}, error=function(e) NULL)
}


getPlayerlist <- function(season = "2016-17"){
  require(dplyr)
  tryCatch({
  url  <- paste("http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=",season,"&IsOnlyCurrentSeason=1%20", sep="")
  web_page <- readLines(url, warn = F)
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", ",",x3, perl=TRUE)

  #http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2016-17&IsOnlyCurrentSeason=0

  playerlist<-read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE)
  playerlist<-playerlist[,c(1,3,4,5,6,8,10,11,13)]
  playerlist$PERSON_ID <- as.character(playerlist$PERSON_ID)
  playerlist$TEAM_ID <- as.character(playerlist$TEAM_ID)
  colnames(playerlist)[1] <- "PLAYER_ID"
  return(playerlist)}, error=function(e) NULL)
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



getPlaytypePlayer <- function(playtype, type = "offensive", season = "2016"){
  tryCatch({
  if(playtype %in% c("PostTouch","ElbowTouch","PaintTouch","SpeedDistance","CatchShoot",
                     "Defense","Drives","Passing","Possessions","PullUpShot","Rebounding","Efficiency")){

    seasonid <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")

    url <- paste("http://stats.nba.com/stats/leaguedashptstats?College=&Conference=&%20Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=%20&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0%20&PerMode=PerGame&PlayerExperience=&PlayerOrTeam=Player&PlayerPosition=&PtMeasureType=",playtype,"&Season=",seasonid,"&SeasonSegment=&SeasonType=Regular+Season&StarterBench=%20&TeamID=0&VsConference=&VsDivision=&Weight=", sep = "")
    web_page <- readLines(url, warn = F)
    x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
    x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
    x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
    x4 <- gsub(";", ",",x3, perl=TRUE)
    nba<-read.table(textConnection(x4), header=T, sep=",", skip=2, stringsAsFactors=FALSE, fill = TRUE)
  }

  if(playtype %in% c("Isolation","Transition","Postup","PRBallHandler","PRRollman","Spotup",
                     "Handoff","Cut","OffScreen","OffRebound")){
    url <- paste("http://stats-prod.nba.com/wp-json/statscms/v1/synergy/player/?category=",playtype,"&limit=500&name=",type,"&q=2452161&season=",season,"&seasonType=Reg", sep ="")
    web_page <- readLines(url, warn = F)
    nba <- as.data.frame(fromJSON(web_page),stringsAsFactors = FALSE)
  }


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
},error = function(err){
  return(NULL)
})
}





getPlaytypeTeam <- function(playtype, type = "defensive", season = "2016"){
  tryCatch({
  if(playtype %in% c("Isolation","Transition","Postup","PRBallHandler","PRRollman","Spotup",
                     "Handoff","Cut","OffScreen","OffRebound")){
    url <- paste("http://stats-prod.nba.com/wp-json/statscms/v1/synergy/team/?category=",playtype,"&limit=500&name=",type,"&q=2452161&season=",season,"&seasonType=Reg",sep="")
    web_page <- readLines(url, warn = F)
    nba <- as.data.frame(fromJSON(web_page),stringsAsFactors = FALSE)

    nba <- nba[,c(2,4,7,9,10,11,23,24,26)]
    cols <- c("ABR_", "POSS_","POINTS_","FGA_","FGM_","FT_","TO_","PLUSONE_")
    cols <- paste(cols, toupper(playtype), sep="")
    cold <- append("TEAM_ID", cols)
    colnames(nba) <- cold
    assign(paste("TEAM_",playtype,sep=""),nba)
    return(get(paste("TEAM_",playtype,sep="")))

  }}
  ,error = function(err){
    return(NULL)
  })
}







getSchedule <- function(month){
  require(XML)
  require(stringr)
  require(jsonlite)

  mo2Num <- function(x) match(tolower(x), tolower(month.abb))
  tryCatch({
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
  ,error = function(err){
    return(NULL)
  })
}

getNbaSchedule <- function(dt){
  require(XML)
  require(stringr)
  require(jsonlite)
  mo2Num <- function(x) match(tolower(x), tolower(month.abb))
  tryCatch({
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
  return(basic)}
  ,error = function(err){
    return(NULL)
  })
}


