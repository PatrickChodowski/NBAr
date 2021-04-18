#' @name get_tracking
#' @rdname get_tracking
#' @title Download SportVu player tracking data for Players and Teams
#'
#' @description  Download and process NBA.com player tracking data from http://stats.nba.com/players/drives/, http://stats.nba.com/teams/passing/ etc.
#'
#' @param season Number of the year in which season started
#' @param type Specify if data is for Team or Player   c("Player,"Team")
#' @param measure_type Specify which statistics group you want to download. c("Drives","Defense","CatchShoot","Passing","Possessions","PullUpShot","Rebounding","Efficiency","SpeedDistance","ElbowTouch","PostTouch","PaintTouch")[1]
#' @param per_mode Specify if you want data divided per game or totals. Default parameter is "PerGame". c("PerGame","Totals")
#' @param season_type Choose data for preseason, regular season or postseason. Default parameter is "Regular+Season". c("Regular+Season","Playoffs","Pre+Season","All+Star")
#' @param season_segment Choose season half for the data. Empty string means whole season and it is set by default. c("","Post+All-Star","Pre+All-Star")
#' @param game_segment Choose game half for the data. Empty string means whole game and it is set by default. c("","First+Half","Overtime","Second+Half")
#' @param period Choose game period for the data. 0 means whole game and it is set by default. as.character(c(0:4))
#' @param date_from Day from which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param date_to Day to which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param outcome Filter by game result. It can be a loss (L) or a win (W). By default parameter is an empty string, so both are taken into account. c("","W","L")
#' @param opponent_team_id Filter by opponent's team id from nba.com database. Default "0" means all teams.
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, tracking, sportvu, players, teams,
#'
#' @examples
#'
#' season <- 2016
#' type <- c("Player","Team")[1]
#'
#' measure_type <- c("Drives",
#'                  "Defense",
#'                  "CatchShoot",
#'                  "Passing",
#'                  "Possessions",
#'                  "PullUpShot",
#'                  "Rebounding",
#'                  "Efficiency",
#'                  "SpeedDistance",
#'                  "ElbowTouch",
#'                  "PostTouch",
#'                  "PaintTouch"
#'                  )[1]
#'
#' per_mode <- c("PerGame","Totals")[1]
#' season_type <- c("Regular+Season","Playoffs","Pre+Season","All+Star")[1]
#' season_segment <- c("","Post+All-Star","Pre All-Star")[1]
#' game_segment <- c("","First+Half","Overtime","Second+Half")[1]
#' period <- 0
#' date_from <- "01/01/2017"
#' date_to <- "04/30/2017"
#' outcome <- c("","W","L")[1]
#' opponent_team_id <- 0
#'
#' df <- get_tracking(
#'                                 season,
#'                                 type,
#'                                 measure_type,
#'                                 per_mode,
#'                                 season_type,
#'                                 season_segment,
#'                                 game_segment,
#'                                 date_from,
#'                                 date_to,
#'                                 outcome,
#'                                 period,
#'                                 opponent_team_id
#'                                 )
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
#' @export get_tracking



get_tracking <- function(season,
                        type,
                        measure_type,
                        per_mode = "PerGame",
                        season_type = "Regular+Season",
                        season_segment = "",
                        game_segment ="",
                        date_from = "",
                        date_to = "",
                        outcome = "",
                        period = 0,
                        opponent_team_id = 0,
                        verbose=TRUE
){


  tryCatch({
    season_id <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    link <- glue("http://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom={date_from}",
                "&DateTo={date_to}",
                "&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment={game_segment}",
                "&Height=&LastNGames=0&LeagueID=00&Location=&PtMeasureType={measure_type}",
                "&Month=0&OpponentTeamID={opponent_team_id}",
                "&Outcome={outcome}",
                "&PORound=0&PaceAdjust=N&PerMode={per_mode}",
                "&Period={period}",
                "&PlayerExperience=&PlayerOrTeam={type}",
                "&PlayerPosition=&PlusMinus=N&Rank=N&Season={season_id}",
                "&SeasonSegment={season_segment}",
                "&SeasonType={season_type}",
                "&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")

    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36",
                                                                   'connection' = 'keep-alive',
                                                                   'Accept' = 'application/json',
                                                                   'Host' = 'stats.nba.com',
                                                                   'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()
    index <- which(result_sets_df$resultSets$name == "LeagueDashPtStats")

    dataset <- result_sets_df$resultSets$rowSet %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric) %>%
      set_names(tolower(unlist(result_sets_df$resultSets$headers[index]))) 

    verbose_dataset(verbose, dataset)
    return(dataset)}, error=function(e) print(e$message))
}
