#' @name getBoxscore
#' @rdname getBoxscore
#' @title Download Boxscore data for NBA game
#'
#' @description  Download and process NBA.com boxscore data for given game and season.
#'
#' @param gameID Game's ID in NBA.com DB
#' @param Type  Type of boxscore, one from the list: c("advanced","misc","playertrack","traditional")
#'
#' @return Dataset containing data from one of the boxscore pages from NBA.com. Functions enable downloading Traditional, Advanced, Miscallenous or Player Tracking data.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, Boxscores, players, Game, Misc, Advanced, Traditional, Player Tracking
#'
#' @examples
#'
#' Type <- c("advanced","misc","playertrack","traditional")[1]
#' getBoxscore("0021400001",Type)
#'
#' #playoffs:
#' getBoxscore("0041600101",Type)
#'
#' gamelist<- paste("00",as.character(c(21400001:21401230)),sep="")
#' boxscores <- lapply(gamelist, getBoxscore, Type = "traditional")
#' player.tracking <- do.call(rbind.data.frame,boxscores)
#'
#' @importFrom lubridate minutes
#' @importFrom lubridate hours
#' @importFrom lubridate seconds
#' @importFrom jsonlite fromJSON
#' @export getBoxscore
#'
#'
#'
getBoxscore <- function(gameID, Type){
  require(lubridate,quietly = T)
  require(jsonlite, quietly = T)
  tryCatch({

    url <- paste("http://stats.nba.com/stats/boxscore",Type,"v2?EndPeriod=10&EndRange=28800&GameID=",gameID,"&RangeType=0&StartPeriod=1&StartRange=0", sep = "")

    n1 <- readLines(url,warn = F)
    n2 <- fromJSON(n1)
    index <- which(n2$resultSets$name %in% c("PlayerStats","sqlPlayersMisc","PlayerStats","PlayerTrack"))
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




