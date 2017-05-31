#' Download shotchart data for player
#'
#' Downloads and process NBA.com shotchart data for given player and season.
#' @param playerID Player's ID in NBA.com DB
#' @param season Number of the year in which season started
#'
#' @return Dataset containing shot information such as location on the floor, type of shot and result per player and game.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, Shotchart, players
#'
#' @examples
#'
#' #' ContextMeasure <- c('PTS','FGA','FGM','FG_PCT','FG3M','FG3A','FG3_PCT','PF','EFG_PCT','TS_PCT','PTS_FB','PTS_OFF_TOV','PTS_2ND_CHANCE')
#' GameSegment <- c('','First Half','Overtime','Second Half')
#' OpponentTeamID <- c("0")
#' PerMode <- c("PerGame","Totals")[1]
#' Period <- as.character(c(0:4))[1]
#' DateFrom <- "01/01/2017"
#' DateTo <- "04/30/2017"
#' SeasonType <- c("Regular Season","Playoffs","Pre Season","All Star")[1]
#' SeasonSegment <- c("","Post All-Star","Pre All-Star")[1]
#' Outcome <- c("","W","L")[1]
#'
#' getShotchart("2853","2014")
#'
#' shotcharts<- lapply(players, getShotchart, season = "2014")
#' shotchart <- do.call(rbind.data.frame,shotcharts)
#'
#' @export getShotchart
#' @importFrom jsonlite fromJSON
#'


getShotchart <- function( PlayerID,
                          Season,
                          ContextMeasure = "FGA",
                          DateFrom = "",
                          DateTo = "",
                          GameSegment = "",
                          Period  = "0",
                          PerMode = "PerGame",
                          Outcome = "",
                          SeasonType = "Regular Season",
                          SeasonSegment= "",
                          OpponentTeamID= "0"
){
  require(jsonlite, quietly = T)
  tryCatch({
    seasonid <- paste(Season, as.numeric(substring(Season,3,4))+1,sep="-")
url <- gsub("\n","",
paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=",seasonid,"&ContextFilter=
&ContextMeasure=",ContextMeasure,"
&DateFrom=",DateFrom,"
&DateTo=",DateTo,"
&GameID=
&GameSegment=",GameSegment,"
&LastNGames=0
&LeagueID=00
&Location=
&MeasureType=Base
&Month=0
&OpponentTeamID=",OpponentTeamID,"
&Outcome=",Outcome,"
&PaceAdjust=N
&PerMode=",PerMode,"
&Period=",Period,"
&PlayerID=",PlayerID,"
&SeasonType=",SeasonType,"
&TeamID=0
&VsConference=
&VsDivision=
&mode=Advanced
&showDetails=0
&showShots=1
&showZones=0
&RookieYear=
&SeasonSegment=",SeasonSegment,"
&PlayerPosition=", sep=""))

    n1 <- readLines(url,warn = F)
    n2 <- fromJSON(n1)
    index <- which(n2$resultSets$name == "Shot_Chart_Detail")
    cols <- unlist(n2$resultSets$headers[index])
    vls <- n2$resultSets$rowSet[index]
    vls2 <- as.data.frame(vls,stringsAsFactors = F)
    colnames(vls2) <- cols
    return(vls2)}, error=function(e) NULL)
}
