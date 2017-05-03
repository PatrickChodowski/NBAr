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
#' getShotchart("2853","2014")
#'
#' shotcharts<- lapply(players, getShotchart, season = "2014")
#' shotchart <- do.call(rbind.data.frame,shotcharts)
#'
#' @export getShotchart
#' @importFrom jsonlite fromJSON

getShotchart <- function(playerID, season){
  require(jsonlite, quietly = T)
  tryCatch({
    seasonid <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    url <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=",seasonid,"&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&RookieYear=&SeasonSegment=&PlayerPosition=", sep="")
    n1 <- readLines(url,warn = F)
    n2 <- fromJSON(n1)
    index <- which(n2$resultSets$name == "Shot_Chart_Detail")
    cols <- unlist(n2$resultSets$headers[index])
    vls <- n2$resultSets$rowSet[index]
    vls2 <- as.data.frame(vls,stringsAsFactors = F)
    colnames(vls2) <- cols
    return(vls2)}, error=function(e) NULL)
}
