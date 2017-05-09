#' @name getGeneral
#' @rdname getGeneral
#' @title Download general data for Players and Teams
#'
#' @description  Download and process NBA.com general data from http://stats.nba.com/players/traditional/, http://stats.nba.com/teams/advanced/ etc.
#'
#' @param Season Number of the year in which season started
#' @param Type Specify if data is for Team or Player   c("Player,"Team")
#' @param MeasureType Specify which statistics group you want to download. c("Base","Advanced","Misc","Scoring","Usage","Opponent","Defense", "Four Factors")
#' @param PerMode Specify if you want data divided per game or totals. Default parameter is "PerGame". c("PerGame","Totals")
#' @param SeasonType Choose data for preseason, regular season or postseason. Default parameter is "Regular Season". c("Regular Season","Playoffs","Pre Season","All Star")
#' @param SeasonSegment Choose season half for the data. Empty string means whole season and it is set by default. c("","Post All-Star","Pre All-Star")
#' @param GameSegment Choose game half for the data. Empty string means whole game and it is set by default. c("","First Half","Overtime","Second Half")
#' @param Period Choose game period for the data. 0 means whole game and it is set by default. as.character(c(0:4))
#' @param DateFrom Day from which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param DateTo Day to which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param Outcome Filter by game result. It can be a loss (L) or a win (W). By default parameter is an empty string, so both are taken into account. c("","W","L")
#' @param OpponentTeamID Filter by opponent's team id from nba.com database. Default "0" means all teams.
#'
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, general, traditional, advanced players, teams,
#'
#' @examples
#'
#' Season <- "2016"
#' Type <- c("Player","Team")[1]
#'
#' MeasureType <- c("Base",
#'                  "Advanced",
#'                  "Misc",
#'                  "Scoring",
#'                  "Usage",
#'                  "Opponent",
#'                  "Defense",
#'                  "Four Factors"
#'                  )[1]
#'
#' PerMode <- c("PerGame","Totals")[1]
#' SeasonType <- c("Regular Season","Playoffs","Pre Season","All Star")[1]
#' SeasonSegment <- c("","Post All-Star","Pre All-Star")[1]
#' GameSegment <- c("","First Half","Overtime","Second Half")[1]
#' Period <- as.character(c(0:4))[1]
#' DateFrom <- "01/01/2017"
#' DateTo <- "04/30/2017"
#' Outcome <- c("","W","L")[1]
#' OpponentTeamID <- c("0")
#'
#' general.dataset <- getGeneral(
#'                               Season,
#'                               Type,
#'                               MeasureType,
#'                               PerMode,
#'                               SeasonType,
#'                               SeasonSegment,
#'                               GameSegment,
#'                               DateFrom,
#'                               DateTo,
#'                               Outcome,
#'                               Period,
#'                               OpponentTeamID
#'                               )
#'
#' @importFrom jsonlite fromJSON
#' @export getGeneral




getGeneral <- function( Season ,
                        Type,
                        MeasureType,
                        PerMode = "PerGame",
                        SeasonType = "Regular Season",
                        SeasonSegment = "",
                        GameSegment ="",
                        DateFrom = "",
                        DateTo = "",
                        Outcome = "",
                        Period = "0",
                        OpponentTeamID = "0"
){
  require(jsonlite, quietly = T)
  tryCatch({
    seasonid <- paste(Season, as.numeric(substring(Season,3,4))+1,sep="-")
url <- gsub("\n","",
paste("http://stats.nba.com/stats/leaguedash",Type,"stats?
College=
&Conference=
&Country=
&DateFrom=",DateFrom,"
&DateTo=",DateTo,"
&Division=
&DraftPick=
&DraftYear=
&GameScope=
&GameSegment=",GameSegment,"
&Height=
&LastNGames=0
&LeagueID=00
&Location=
&MeasureType=",MeasureType,"
&Month=0
&OpponentTeamID=",OpponentTeamID,"
&Outcome=
&PORound=0
&PaceAdjust=N
&PerMode=",PerMode,"
&Period=",Period,"
&PlayerExperience=
&PlayerPosition=
&PlusMinus=N
&Rank=N
&Season=",seasonid,"
&SeasonSegment=",SeasonSegment,"
&SeasonType=",SeasonType,"
&ShotClockRange=
&StarterBench=
&TeamID=0
&VsConference=
&VsDivision=
&Weight="
,sep=""))
n1 <- readLines(url,warn = F)
n2 <- fromJSON(n1)
index <- which(n2$resultSets$name == paste("LeagueDash",Type,"Stats",sep=""))
cols <- unlist(n2$resultSets$headers[index])
vls <- n2$resultSets$rowSet[index]
vls2 <- as.data.frame(vls,stringsAsFactors = F)
colnames(vls2) <- cols
s <- ifelse(Type=="Player",5,3)
vls2[,c(s:(ncol(vls2)-2))] <- sapply(vls2[,c(s:(ncol(vls2)-2))], as.numeric)
return(vls2)}, error=function(e) NULL)
}





