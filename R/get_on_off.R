#' @name get_on_off
#' @rdname get_on_off
#' @title Download On/Off team data
#'
#' @description  Download and process NBA.com on/off data from http://stats.nba.com/team/1610612743/onoffcourt-summary/
#' @param season Number of the year in which season started
#' @param per_mode Specify if you want data divided per game or totals. Default parameter is "PerGame". c("PerGame","Totals")
#' @param period Choose game period for the data. 0 means whole game and it is set by default. as.character(c(0:4))
#' @param season_type Choose data for preseason, regular season or postseason. Default parameter is "Regular Season". c("Regular Season","Playoffs","Pre Season","All Star")
#' @param season_segment Choose season half for the data. Empty string means whole season and it is set by default. c("","Post All-Star","Pre All-Star")
#' @param game_segment Choose game half for the data. Empty string means whole game and it is set by default. c("","First Half","Overtime","Second Half")
#' @param date_from Day from which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param date_to Day to which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param outcome Filter by game result. It can be a loss (L) or a win (W). By default parameter is an empty string, so both are taken into account. c("","W","L")
#' @param opponent_team_id Filter by opponent's team id from nba.com database. Default "0" means all teams.
#' @param team_id Specify team id from nba.com database. Default "0" means all teams.
#' @param verbose Defalt TRUE - prints additional information

#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, lineups, players, mason plumlee should not be paired with Jokic
#'
#' @examples
#'
#' season = '2017'
#' team_id = "1610612739"
#' date_from = '01/01/2018'
#' period = 0
#' date_to = '01/25/2018'
#' outcome = c("","W","L")[1]
#' per_mode =  c("Totals","PerGame","MinutesPer","Per48","Per40","Per36")[1]
#' season_segment = c("","Post+All-Star","Pre+All-Star")[1]
#' game_segment = c("","First+Half","Overtime","Second+Half")[1]
#' season_type = c("Regular+Season","Playoffs","PreSeason","All+Star")[1]
#' opponent_team_id = "0"
#'
#' df =     get_on_off(season = 2017,
#'                 team_id = 1610612739,
#'                 measure_type,
#'                 date_from="",
#'                 date_to="",
#'                 opponent_team_id="0",
#'                 outcome="",
#'                 period = 0,
#'                 per_mode=per_mode,
#'                 season_segment="",
#'                 game_segment="",
#'                 season_type="Regular+Season")
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
#' @export get_on_off
#'

get_on_off <- function(season,
                     team_id,
                     date_from="",
                     date_to="",
                     opponent_team_id="0",
                     outcome="",
                     per_mode="Totals",
                     period=0,
                     season_segment="",
                     game_segment="",
                     season_type="Regular+Season",
                     verbose=TRUE)
{
  tryCatch({
    season_id <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")

    link <- glue(
      "http://stats.nba.com/stats/teamplayeronoffsummary?",
      "DateFrom={date_from}",
      "&DateTo={date_to}",
      "&GameSegment={game_segment}",
      "&LastNGames=0&LeagueID=00&Location=&MeasureType=Base",
      "&Month=0&OpponentTeamID={opponent_team_id}",
      "&Outcome={outcome}",
      "&PaceAdjust=N&PerMode={per_mode}",
      "&Period={period}&PlusMinus=N&Rank=N&Season={season_id}",
      "&SeasonSegment={season_segment}",
      "&SeasonType={season_type}",
      "&TeamID={team_id}",
      "&VsConference=&VsDivision="
    )

    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                   'connection' = 'keep-alive',
                                                                   'Accept' = 'application/json',
                                                                   'Host' = 'stats.nba.com',
                                                                   'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()

    index <- which(result_sets_df$resultSets$name %in% c("PlayersOnCourtTeamPlayerOnOffSummary",
                                             "PlayersOffCourtTeamPlayerOnOffSummary"))

    cols <- tolower(unlist(result_sets_df$resultSets$headers[index[1]]))

    dataset_on <- result_sets_df$resultSets$rowSet[index[1]][1] %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric)

    colnames(dataset_on)[1:8] <- cols[1:8]
    colnames(dataset_on)[9:13] <- c("min_on","plus_minus_on","off_rating_on","def_rating_on","net_rating_on")

    dataset_off <- result_sets_df$resultSets$rowSet[index[2]][1] %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric) 
    colnames(dataset_off)[1:8] <- cols[1:8]
    colnames(dataset_off)[9:13] <- c("min_off","plus_minus_off","off_rating_off","def_rating_off","net_rating_off")


    dataset <- dataset_on %>%
      left_join(.,dataset_off, by = c("vs_player_id","team_abbreviation","team_name",
                                 "team_id","vs_player_name")) %>%
      select(-c(group_set.x, court_status.x, group_set.y,court_status.y,gp.y),
             player_id = vs_player_id,
             player_name = vs_player_name,
             gp = gp.x) 

    verbose_dataset(verbose, dataset)

    return(dataset)}, error=function(e) print(e$message))
}
