#' Download shotchart data for player
#'
#' Downloads and process NBA.com shotchart data for given player and season.
#' @param player_id Player's ID in NBA.com DB
#' @param season Number of the year in which season started
#' @param context_measure Specify which value group you want to download. c('PTS','FGA','FGM','FG_PCT','FG3M','FG3A','FG3_PCT','PF',
#' 'EFG_PCT','TS_PCT','PTS_FB','PTS_OFF_TOV','PTS_2ND_CHANCE')
#' @param per_mode Specify if you want data divided per game or totals. Default parameter is "PerGame". c("PerGame","Totals")
#' @param season_type Choose data for preseason, regular season or postseason. Default parameter is "Regular Season". c("Regular Season","Playoffs","Pre Season","All Star")
#' @param season_segment Choose season half for the data. Empty string means whole season and it is set by default. c("","Post All-Star","Pre All-Star")
#' @param game_segment Choose game half for the data. Empty string means whole game and it is set by default. c("","First Half","Overtime","Second Half")
#' @param period Choose game period for the data. 0 means whole game and it is set by default. as.character(c(0:4))
#' @param date_from Day from which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param date_to Day to which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param outcome Filter by game result. It can be a loss (L) or a win (W). By default parameter is an empty string, so both are taken into account. c("","W","L")
#' @param opponent_team_id Filter by opponent's team id from nba.com database. Default "0" means all teams.
#' @param verbose Defalt TRUE - prints additional information
#'
#'
#' @return Dataset containing shot information such as location on the floor, type of shot and result per player and game.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, Shotchart, players
#'
#' @examples
#'
#'
#' context_measure <- c('PTS','FGA','FGM','FG_PCT','FG3M','FG3A','FG3_PCT','PF',
#' 'EFG_PCT','TS_PCT','PTS_FB','PTS_OFF_TOV','PTS_2ND_CHANCE')
#' game_segment <- c('','First+Half','Overtime','Second+Half')
#' opponent_team_id <- 0
#' per_mode <- c("PerGame","Totals")[1]
#' period <- as.character(c(0:4))[1]
#' date_from <- "01/01/2017"
#' date_to <- "04/30/2017"
#' season_type <- c("Regular+Season","Playoffs","Pre+Season","All+Star")[1]
#' season_segment <- c("","Post+All-Star","Pre+All-Star")[1]
#' outcome <- c("","W","L")[1]
#'
#' get_shotchart(2853,2014)
#'
#'
#' @export get_shotchart
#' @importFrom  lubridate second minute
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#'


get_shotchart <- function( player_id,
                          season,
                          context_measure = "FGA",
                          date_from = "",
                          date_to = "",
                          game_segment = "",
                          period  = "0",
                          per_mode = "PerGame",
                          outcome = "",
                          season_type = "Regular+Season",
                          season_segment= "",
                          opponent_team_id= "0",
                          verbose=TRUE
){

  tryCatch({
    season_id <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
link <- glue("https://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS={season_id}&ContextFilter=",
"&ContextMeasure={context_measure}",
"&DateFrom={date_from}",
"&DateTo={date_to}",
"&GameID=&GameSegment={game_segment}",
"&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID={opponent_team_id}",
"&Outcome={outcome}",
"&PaceAdjust=N&PerMode={per_mode}",
"&Period={period}",
"&PlayerID={player_id}",
"&SeasonType={season_type}",
"&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0&RookieYear=",
"&SeasonSegment={season_segment}",
"&PlayerPosition=")


verbose_print(verbose, link)
result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 
                                                               'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                               'connection' = 'keep-alive',
                                                               'Accept' = 'application/json',
                                                               'Host' = 'stats.nba.com',
                                                               'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()
index <- which(result_sets_df$resultSets$name == "Shot_Chart_Detail")
dataset <- result_sets_df$resultSets$rowSet[index][1] %>%
  as.data.frame(stringsAsFactors=F) %>%
  as_tibble() %>%
  mutate_if(check_if_numeric, as.numeric) %>%
  set_names(tolower(unlist(result_sets_df$resultSets$headers[index]))) 

verbose_dataset(verbose, dataset)

    return(dataset)}, error=function(e) print(e$message))
}
