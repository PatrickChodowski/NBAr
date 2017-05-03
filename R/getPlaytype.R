#' @name getPlaytype
#' @rdname getPlaytype
#' @title Download Playtype data for Players and Teams
#'
#' @description  Download and process NBA.com Playtype data from http://stats.nba.com/players/playtypeexample/ or http://stats.nba.com/teams/playtypeexample/
#'
#' @param Season Number of the year in which season started
#' @param Type Specify if data is for Team or Player   c("Player,"Team")
#' @param Category Pick one of the playtypes provided by stats.nba.com. These are Isolation, Transition, Postup, PRBallHandler, PRRollman, Spotup, Handoff,Postup, Cut, OffScreen, Putbacks and Misc.
#' c("Isolation","Transition","PRBallHandler","PRRollman","Postup","Spotup","Handoff","Cut","OffScreen","OffRebound","Misc")
#' @param Names Specify if data is for offensive or defensive side of the floor. c("offensive","defensive")
#' @param SeasonType Choose data for preseason, regular season or postseason c("Reg","Post","Pre")
#'
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, Playtypes, players, teams,
#'
#' @examples
#' getPlaytype("2016","Team","Handoff","offensive","Pre")
#' getPlaytype("2015","Player","Isolation","defensive","Post")
#'
#' Season <- "2016"
#' Names <- c("offensive","defensive")[1]
#' Type <- c("Player","Team")[1]
#'
#' Playtype <- c("PostTouch","ElbowTouch","PaintTouch"
#'               ,"CatchShoot","Drives","PullUpShot","Isolation",
#'               "PRBallHandler","PRRollman","Spotup","Handoff","Cut","OffScreen",
#'               "OffRebound")[1]
#'
#' SeasonType <- c("Reg","Post","Pre")[1]
#'
#' playtypes.dataset <- getPlaytype(Season,Type,Category,Names,SeasonType)
#'
#' @importFrom jsonlite fromJSON
#' @export getPlaytype




getPlaytype <- function(Season = "",
                        Type = "",
                        Category = "",
                        Names = "",
                        SeasonType = ""

){
  require(jsonlite, quietly = T)
  tryCatch({


url <- gsub("\n","",
paste("http://stats-prod.nba.com/wp-json/statscms/v1/synergy/",Type,"
/?category=",Category,"
&limit=500
&names=",Names,"
&q=2488157
&season=",Season,"
&seasonType=",SeasonType
,sep=""))
n1 <- readLines(url,warn = F)
n2 <- fromJSON(n1)
vls <- as.data.frame(n2$results,stringsAsFactors = F)
return(vls2)}, error=function(e) NULL)
}
