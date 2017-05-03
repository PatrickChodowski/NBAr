#' Download Playbyplay data for NBA game
#'
#' Download and process NBA.com play-by-play data for given game and season.
#' @param gameID Game's ID in NBA.com DB
#'
#' @return Dataset containing data from Playbyplay pages. Play-by-play provides information about every play from NBA game, one action per row.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, play-by-play, game
#'
#' @examples
#' getPlaybyplay("0021400001")
#'
#' gamelist<- paste("00",as.character(c(21400001:21401230)),sep="")
#' pbps <- lapply(gamelist, getPlaybyplay)
#' playbyplay <- do.call(rbind.data.frame,pbps)
#'
#' @importFrom lubridate minutes
#' @importFrom lubridate hours
#' @importFrom lubridate seconds
#' @importFrom zoo na.locf

#'
#' @export getPlaybyplay


getPlaybyplay <- function(gameID){
  require(lubridate,quietly = T)
  require(zoo,quietly = T)
  tryCatch({
    url <- paste("http://stats.nba.com/stats/playbyplay?GameID=", gameID, "&StartPeriod=0&EndPeriod=14", sep="")
    n1 <- readLines(url,warn = F)
    n2 <- fromJSON(n1)
    index <- which(n2$resultSets$name == "PlayByPlay")
    cols <- unlist(n2$resultSets$headers[index])
    vls <- n2$resultSets$rowSet[index]
    vls2 <- as.data.frame(vls,stringsAsFactors = F)
    colnames(vls2) <- cols

    vls2$MINS <- as.numeric(minute(ms(vls2$PCTIMESTRING)))
    vls2$SECS <- as.numeric(second(ms(vls2$PCTIMESTRING)))
    vls2[1,c("SCORE","SCOREMARGIN")] <- c("0 - 0",0)
    vls2[,11:12] <- apply(vls2[,11:12],2,na.locf)
    return(vls2)}, error=function(e) NULL)
}

