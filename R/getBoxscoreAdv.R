#' @name getBoxscore
#' @rdname getBoxscore
#' @title Download Boxscore data for NBA game
#'
#' @description  Download and process NBA.com boxscore data for given game and season.
#'
#' @param gameID Game's ID in NBA.com DB
#' @param season Number of the year in which season started
#'
#' @return Dataset containing data from one of the boxscore pages from NBA.com. Functions enable downloading Traditional, Advanced, Miscallenous or Player Tracking data.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, Boxscores, players, Game, Misc, Advanced, Traditional, Player Tracking
#'
#' @examples
#' getBoxscoreTrad("0021400001","2014")
#' getBoxscoreMisc("0021400001","2014")
#' getBoxscoreAdv("0021400001","2014")
#' getBoxscorePt("0021400001","2014")
#'
#' gamelist<- paste("00",as.character(c(21400001:21401230)),sep="")
#' boxscores <- lapply(gamelist, getBoxscorePt, season="2014")
#' player.tracking <- do.call(rbind.data.frame,boxscores)
#'
#' @importFrom lubridate minutes
#' @importFrom lubridate hours
#' @importFrom lubridate seconds
#' @importFrom jsonlite fromJSON

NULL

#' @rdname getBoxscore
#' @export getBoxscoreAdv
getBoxscoreAdv <- function(gameID, season){
  require(lubridate,quietly = T)
  require(jsonlite, quietly = T)
  tryCatch({

    seasonid <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    url <- paste("http://stats.nba.com/stats/boxscoreadvancedv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=",seasonid,"&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")

    n1 <- readLines(url,warn = F)
    n2 <- fromJSON(n1)
    index <- which(n2$resultSets$name == "PlayerStats")
    cols <- unlist(n2$resultSets$headers[index])
    vls <- n2$resultSets$rowSet[index]
    vls2 <- as.data.frame(vls,stringsAsFactors = F)
    colnames(vls2) <- cols

    vls2[,10:ncol(vls2)] <- sapply(vls2[,10:ncol(vls2)], as.numeric)

    vls2$MINS <- as.numeric(minute(ms(vls2[,9])))
    vls2$SECS <- as.numeric(second(ms(vls2[,9])))

    vls2 <- vls2[,-9]
    return(vls2)}, error=function(e) NULL)
}



#' @rdname getBoxscore
#' @export getBoxscoreMisc
getBoxscoreMisc <- function(gameID, season){
  require(lubridate,quietly = T)
  require(jsonlite, quietly = T)
  tryCatch({
    seasonid <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")

    seasonid <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    url <- paste("http://stats.nba.com/stats/boxscoremiscv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=",seasonid,"&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")

    n1 <- readLines(url,warn = F)
    n2 <- fromJSON(n1)
    index <- which(n2$resultSets$name == "sqlPlayersMisc")
    cols <- unlist(n2$resultSets$headers[index])
    vls <- n2$resultSets$rowSet[index]
    vls2 <- as.data.frame(vls,stringsAsFactors = F)
    colnames(vls2) <- cols

    vls2[,10:ncol(vls2)] <- sapply(vls2[,10:ncol(vls2)], as.numeric)

    vls2$MINS <- as.numeric(minute(ms(vls2[,9])))
    vls2$SECS <- as.numeric(second(ms(vls2[,9])))

    vls2 <- vls2[,-9]
    return(vls2)}, error=function(e) NULL)
}




#' @rdname getBoxscore
#' @export getBoxscorePt
getBoxscorePt <- function(gameID, season){
  require(lubridate,quietly = T)
  require(jsonlite, quietly = T)
  tryCatch({

    seasonid <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    url <- paste("http://stats.nba.com/stats/boxscoreplayertrackv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=",seasonid,"&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")

    n1 <- readLines(url,warn = F)
    n2 <- fromJSON(n1)
    index <- which(n2$resultSets$name == "PlayerTrack")
    cols <- unlist(n2$resultSets$headers[index])
    vls <- n2$resultSets$rowSet[index]
    vls2 <- as.data.frame(vls,stringsAsFactors = F)
    colnames(vls2) <- cols

    vls2[,10:ncol(vls2)] <- sapply(vls2[,10:ncol(vls2)], as.numeric)

    vls2$MINS <- as.numeric(minute(ms(vls2[,9])))
    vls2$SECS <- as.numeric(second(ms(vls2[,9])))

    vls2 <- vls2[,-9]

    return(vls2)}, error=function(e) NULL)
}




#' @rdname getBoxscore
#' @export getBoxscoreTrad
getBoxscoreTrad <- function(gameID, season){
  require(lubridate,quietly = T)
  require(jsonlite, quietly = T)
  tryCatch({
    seasonid <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    url <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&Season=",seasonid,"&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep = "")

    n1 <- readLines(url,warn = F)
    n2 <- fromJSON(n1)
    index <- which(n2$resultSets$name == "PlayerStats")
    cols <- unlist(n2$resultSets$headers[index])
    vls <- n2$resultSets$rowSet[index]
    vls2 <- as.data.frame(vls,stringsAsFactors = F)
    colnames(vls2) <- cols

    vls2[,10:ncol(vls2)] <- sapply(vls2[,10:ncol(vls2)], as.numeric)

    vls2$MINS <- as.numeric(minute(ms(vls2[,9])))
    vls2$SECS <- as.numeric(second(ms(vls2[,9])))

    vls2 <- vls2[,-9]

    return(vls2)}, error=function(e) NULL)
}

