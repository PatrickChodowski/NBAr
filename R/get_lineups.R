#' @name get_lineups
#' @rdname get_lineups
#' @title Download Lineups data
#'
#' @description  Download and process NBA.com lineups data from http://stats.nba.com/lineups/traditional/
#'
#' @param season Number of the year in which season started
#' @param group_quantity Number of players in a lineup
#' @param measure_type Type of statistics
#' @param per_mode Specify if you want data divided per game or totals. Default parameter is "PerGame". c("PerGame","Totals")
#' @param season_type Choose data for preseason, regular season or postseason. Default parameter is "Regular Season". c("Regular Season","Playoffs","Pre Season","All Star")
#' @param season_segment Choose season half for the data. Empty string means whole season and it is set by default. c("","Post All-Star","Pre All-Star")
#' @param game_segment Choose game half for the data. Empty string means whole game and it is set by default. c("","First Half","Overtime","Second Half")
#' @param date_from Day from which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param date_to Day to which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param outcome Filter by game result. It can be a loss (L) or a win (W). By default parameter is an empty string, so both are taken into account. c("","W","L")
#' @param opponent_team_id Filter by opponent's team id from nba.com database. Default "0" means all teams.
#' @param team_id Specify team id from nba.com database. Default "0" means all teams.
#' @param verbose Defalt TRUE - prints additional information
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, lineups, players, Thibodeau minutes
#'
#' @examples
#'
#' season = '2017'
#' group_quantity = 2
#' measure_type = c("Base","Advanced","Misc","Four+Factors","Scoring","Opponent")[1]
#' team_id = 0
#' date_from = '01/01/2018'
#' date_to = '01/25/2018'
#' opponent_team_id = 0
#' outcome = c("","W","L")[1]
#' per_mode =  c("Totals","PerGame","MinutesPer","Per48","Per40","Per36")[1]
#' season_segment = c("","Post+All-Star","Pre+All-Star")[1]
#' game_segment = c("","First+Half","Overtime","Second+Half")[1]
#' season_type = c("Regular+Season","Playoffs","PreSeason","All+Star")[1]
#'
#' get_lineups(season = '2017',
#'             group_quantity = 5,
#'             measure_type = "Base",
#'             team_id="0",
#'             date_from="",
#'             date_to="",
#'             opponent_team_id="0",
#'             outcome="",
#'             per_mode="Totals",
#'             season_segment="",
#'             game_segment="",
#'             season_type="Regular+Season")
#'
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
#' @export get_lineups


get_lineups <- function(season,
                       group_quantity,
                       measure_type,
                       team_id="0",
                       date_from="",
                       date_to="",
                       opponent_team_id="0",
                       outcome="",
                       per_mode="Totals",
                       season_segment="",
                       game_segment="",
                       season_type="Regular+Season",
                       verbose=TRUE)
{
  tryCatch({
    season_id <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    link <- glue(
      "http://stats.nba.com/stats/leaguedashlineups?Conference=",
      "&DateFrom={date_from}",
      "&DateTo={date_to}",
      "&Division=","&GameSegment={game_segment}",
      "&GroupQuantity={group_quantity}",
      "&LastNGames=0&LeagueID=00&Location=",
      "&MeasureType={measure_type}",
      "&Month=0",
      "&OpponentTeamID={opponent_team_id}",
      "&Outcome={outcome}",
      "&PORound=0&PaceAdjust=N",
      "&PerMode={per_mode}",
      "&Period=0&PlusMinus=N&Rank=N",
      "&Season={season_id}",
      "&SeasonSegment={season_segment}",
      "&SeasonType={season_type}",
      "&ShotClockRange=",
      "&TeamID={team_id}",
      "&VsConference=&VsDivision="
    )

    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                   'connection' = 'keep-alive',
                                                                   'Accept' = 'application/json',
                                                                   'Host' = 'stats.nba.com',
                                                                   'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()
    index <- which(result_sets_df$resultSets$name == paste("Lineups",sep=""))

    dataset <- result_sets_df$resultSets$rowSet[index][1] %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      set_names(tolower(unlist(result_sets_df$resultSets$headers[index]))) %>%
      select(-contains("_rank")) %>%
      mutate(group_id = str_replace_all(group_id,'^-|-$','')) %>%
      separate(group_id, c("player_id_1", "player_id_2","player_id_3","player_id_4","player_id_5"), "-") %>%
      mutate(n = group_quantity) %>%
      select(- c(group_set)) %>%
      mutate_if(check_if_numeric, as.numeric) 
    verbose_dataset(verbose, dataset)
    return(dataset)}, error=function(e) print(e$message))
}


