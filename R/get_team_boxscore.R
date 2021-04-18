
#' @name get_team_boxscore
#' @rdname get_team_boxscore
#' @title Download boxscores data for Teams
#'
#' @description  Download and process NBA.com team boxscores data from https://stats.nba.com/teams/boxscores-traditional/
#'
#' @param season Number of the year in which season started
#' @param measure_type Specify type of data   c('Base','Advanced','Four+Factors','Misc','Scoring')
#' @param season_type Choose data for preseason, regular season or postseason. Default parameter is "Regular+Season". c("Regular+Season","Playoffs","Pre+Season","All+Star")
#' @param game_segment Choose game half for the data. Empty string means whole game and it is set by default. c("","First+Half","Overtime","Second+Half")
#' @param period Choose game period for the data. 0 means whole game and it is set by default. as.character(c(0:4))
#' @param date_from Day from which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param date_to Day to which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param outcome Filter by game result. It can be a loss (L) or a win (W). By default parameter is an empty string, so both are taken into account. c("","W","L")
#' @param opponent_team_id Filter by opponent's team id from nba.com database. Default "0" means all teams.
#' @param verbose Defalt TRUE - prints additional information
#'
#'
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, teams, boxscores
#'
#' @examples
#'
# date_from = '10/01/2018'
# date_to =  '11/03/2018'
# measure_type = c('Base','Advanced','Four+Factors','Misc','Scoring')[1]
# outcome = c('','W','L')[1]
# opponent_team_id = 0
# period = 0
# game_segment = ''
# season_type = 'Regular+Season'
#'
#' get_team_boxscore(season = 2018, measure_type = 'Advanced')
#'
#' @importFrom  lubridate second minute
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @export get_team_boxscore


get_team_boxscore <- function(season, date_from = "", date_to = "",
                              measure_type = "", outcome = "",
                             season_type = "Regular+Season",
                             opponent_team_id = "", period = "", game_segment = "", verbose=TRUE){


  tryCatch({
    season_id = paste(season, as.numeric(substring(season, 3, 4)) + 1, sep = "-")

    link <- glue("https://stats.nba.com/stats/teamgamelogs?DateFrom={date_from}",
    "&DateTo={date_to}",
    "&GameSegment={game_segment}",
    "&LastNGames=&LeagueID=00&Location=&MeasureType={measure_type}",
    "&Month=&OpponentTeamID={opponent_team_id}",
    "&Outcome={outcome}",
    "&PORound=0&PaceAdjust=N&PerMode=Totals&Period={period}",
    "&PlusMinus=N&Rank=N&Season={season_id}",
    "&SeasonSegment=&SeasonType={season_type}&ShotClockRange=&VsConference=&VsDivision=")

    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 
                                                                   'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                   'connection' = 'keep-alive',
                                                                   'Accept' = 'application/json',
                                                                   'Host' = 'stats.nba.com',
                                                                   'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()

    index <- which(result_sets_df$resultSets$name %in% c("TeamGameLogs"))

    dataset <- result_sets_df$resultSets$rowSet %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric) %>%
      set_names(tolower(unlist(result_sets_df$resultSets$headers[index])))

    verbose_dataset(verbose, dataset)
    return(dataset)}, error=function(e)  print(e$message))

}
