#' Download NBA player list for NBA season
#'
#' Download and process list of NBA players for a given season
#'
#' @param season Number of the year in which season started
#'
#' @return Dataset containing data about NBA players who played in specified season.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, players
#'
#' @examples
#' getPlayerlist("2014")
#'
#'
#' @export getPlayerlist

getPlayerlist <- function(season){

  tryCatch({
    seasonid <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    url  <- paste("http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=",seasonid,"&IsOnlyCurrentSeason=1%20", sep="")
    n1 <- readLines(url,warn = F)
    n2 <- fromJSON(n1)
    index <- which(n2$resultSets$name == "CommonAllPlayers")
    cols <- unlist(n2$resultSets$headers[index])
    vls <- n2$resultSets$rowSet[index]
    vls2 <- as.data.frame(vls,stringsAsFactors = F)
    colnames(vls2) <- cols
    return(vls2)}, error=function(e) NULL)
}
