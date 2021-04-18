
#' @name get_shooting
#' @rdname get_shooting
#' @title Download shooting data for Players and Teams
#'
#' @description  Download and process NBA.com shooting data from http://stats.nba.com/players/opponent-shooting/, http://stats.nba.com/teams/shooting/ etc.
#'
#' @param season Number of the year in which season started
#' @param type Specify if data is for Team or Player   c("Player,"Team")
#' @param distance_range Specify how do you want to split the basketball court c("5ft Range","8ft Range","By Zone")
#' @param measure_type Specify if you want data for offensive (base) or defensive(opponent) shooting per player or per team. c("Base","Opponent")
#' @param per_mode Specify if you want data divided per game or totals. Default parameter is "PerGame". c("PerGame","Totals")
#' @param season_type Choose data for preseason, regular season or postseason. Default parameter is "Regular Season". c("Regular Season","Playoffs","Pre Season","All Star")
#' @param season_segment Choose season half for the data. Empty string means whole season and it is set by default. c("","Post All-Star","Pre All-Star")
#' @param game_segment Choose game half for the data. Empty string means whole game and it is set by default. c("","First Half","Overtime","Second Half")
#' @param period Choose game period for the data. 0 means whole game and it is set by default. as.character(c(0:4))
#' @param date_from Day from which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param date_to Day to which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param outcome Filter by game result. It can be a loss (L) or a win (W). By default parameter is an empty string, so both are taken into account. c("","W","L")
#' @param opponent_team_id Filter by opponent's team id from nba.com database. Default "0" means all teams.
#' @param team_id Specify team id from nba.com database. Default "0" means all teams.
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, shooting, players, teams,
#'
#' @examples
#'
#' season <- 2016
#' type <- c("Player","Team")[1]
#' distance_range <- c("5ft Range","8ft Range","By Zone")[1]
#' measure_type <- c("Base","Opponent")[1]
#' game_segment <- c('','First+Half','Overtime','Second+Half')
#' opponent_team_id <- 0
#' team_id <- 0
#' per_mode <- c("PerGame","Totals")[1]
#' period <- as.character(c(0:4))[1]
#' date_from <- "01/01/2017"
#' date_to <- "04/30/2017"
#' season_type <- c("Regular+Season","Playoffs","Pre+Season","All+Star")[1]
#' season_segment <- c("","Post+All-Star","Pre+All-Star")[1]
#' outcome <- c("","W","L")[1]
#'
#' df <- get_shooting(
#'                                 season,
#'                                 type,
#'                                 distance_range,
#'                                 measure_type,
#'                                 per_mode,
#'                                 season_type,
#'                                 season_segment,
#'                                 game_segment,
#'                                 date_from,
#'                                 date_to,
#'                                 outcome,
#'                                 period,
#'                                 opponent_team_id,
#'                                 team_id
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
#' @export get_shooting



get_shooting <- function(season,
                         type,
                         distance_range,
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
                         team_id = 0,
                         verbose = TRUE
){

  tryCatch({
    season_id <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    distance_range <- gsub(' ','%20',distance_range)
    link <- glue("https://stats.nba.com/stats/leaguedash{type}shotlocations",
                 "?College=&Conference=&Country=&DateFrom={date_from}",
                 "&DateTo={date_to}",
                 "&DistanceRange={distance_range}",
                 "&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment={game_segment}",
                 "&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType={measure_type}",
                 "&Month=0&OpponentTeamID={opponent_team_id}",
                 "&Period=0&Outcome={outcome}",
                 "&PORound=0&PaceAdjust=N&PerMode={per_mode}",
                 "&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season_id}",
                 "&SeasonSegment={season_segment}",
                 "&SeasonType={season_type}",
                 "&ShotClockRange=&StarterBench=&TeamID={team_id}",
                 "&VsConference=&VsDivision=&Weight="
    )

    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                   'connection' = 'keep-alive',
                                                                   'Accept' = 'application/json',
                                                                   'Host' = 'stats.nba.com',
                                                                   'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()

    xcols = tolower(unlist(result_sets_df$resultSets$headers$columnNames))
    if(measure_type == 'Base'){
      fg_index = min(which(xcols == 'fgm'))
    } else if(measure_type == 'Opponent'){
      fg_index = min(which(xcols == 'opp_fgm'))
    }

    if(type == 'Team'){
      id_index = min(which(xcols == 'team_id'))
    } else if(type == 'Player'){
      id_index = min(which(xcols == 'player_id'))
    }


    cols_base = xcols[id_index:length(xcols)]
    cols_dist = xcols[1:(id_index-1)]
    cols_mtr = xcols[fg_index:length(xcols)]%>% unique
    cols_info = xcols[id_index:(fg_index-1)]

    cols_dist =
      str_replace_all(
    str_replace_all(
      str_replace_all(
        str_replace_all(
          cols_dist,'less than ',''),' ft.','_ft'),'\\+|\\-| ','_'),'\\_\\(non_ra\\)','')

    new_cols = expand.grid(cols_mtr, cols_dist) %>%
      mutate(new_col = paste(Var1,Var2, sep='_')) %>%
      pull(new_col)

    all_cols = c(cols_info, new_cols)

    dataset <- result_sets_df$resultSets$rowSet %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric) %>%
      set_names(all_cols) %>%
      mutate(date_from = date_from,
             date_to = date_to)

    verbose_dataset(verbose, dataset)

    return(dataset) }, error=function(e) print(e$message))
}
