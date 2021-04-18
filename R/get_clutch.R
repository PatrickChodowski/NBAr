#' @name get_clutch
#' @rdname get_clutch
#' @title Download clutch data for Players and Teams
#'
#' @description  Download and process NBA.com defense data from http://stats.nba.com/players/defense-dash-overall/, http://stats.nba.com/teams/defense-dash-overall/ etc.
#'
#' @param season Number of the year in which season started
#' @param ahead_behind Specify clutch situation c('Ahead+or+Behind','Behind+or+Tied','Ahead+or+Tied')
#' @param clutch_time Specify clutch time c('Last+5+Minutes','Last+4+Minutes','Last+3+Minutes','Last+2+Minutes','Last+1+Minute','Last+30+Seconds','Last+10+Seconds')
#' @param measure_type Specify metrics type c('Base','Advanced','Misc','Scoring','Usage')
#' @param per_mode Specify if you want data divided per game or totals. Default parameter is "PerGame". c("PerGame","Totals")
#' @param season_type Choose data for preseason, regular season or postseason. Default parameter is "Regular Season". c("Regular Season","Playoffs","Pre Season","All Star")
#' @param date_from Day from which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param date_to Day to which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.
#' @param verbose Defalt TRUE - prints additional information
#' 
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, clutch, players, teams,
#'
#' @examples
#'
#' season <- 2019
#' ahead_behind = c('Ahead+or+Behind','Behind+or+Tied','Ahead+or+Tied')[1]
#' clutch_time = c('Last+5+Minutes','Last+4+Minutes','Last+3+Minutes',
#' 'Last+2+Minutes','Last+1+Minute','Last+30+Seconds','Last+10+Seconds')[1]
#' measure_type = c('Base','Advanced','Misc','Scoring','Usage')[1]
#' per_game = c('PerGame','Totals')[1]
#'
#' season_type <- c("Regular+Season","Playoffs","Pre+Season","All+Star")[1]
#' date_from <- "2020-01-01"
#' date_to <- "2020-03-01"
#' season <- 2019
#'
#' clutch.dataset <- get_clutch(2019,'Ahead+or+Behind','Last+5+Minutes')
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
#' @export get_clutch

get_clutch <- function(season,
                        ahead_behind,
                        clutch_time,
                        measure_type = "Base",
                        date_from = "",
                        date_to = "",
                        per_mode = "PerGame",
                        season_type = "Regular+Season",
                        verbose = TRUE
){
  tryCatch({
season_id <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
  
link <- glue("https://stats.nba.com/stats/leaguedashplayerclutch?",
     "AheadBehind={ahead_behind}",
     "&ClutchTime={clutch_time}",
     "&College=&Conference=&Country=",
     "&DateFrom={date_from}",
     "&DateTo={date_to}",
     "&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&",
     "&MeasureType={measure_type}",
     "&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode={per_game}",
     "&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&PointDiff=5&Rank=N&Season={season_id}",
     "&SeasonSegment=&SeasonType={season_type}",
     "&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")
  
  
verbose_print(verbose, link)
result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                               'connection' = 'keep-alive',
                                                               'Accept' = 'application/json',
                                                               'Host' = 'stats.nba.com',
                                                               'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()

index <- which(result_sets_df$resultSets$name == 'LeagueDashPlayerClutch')

dataset <- result_sets_df$resultSets$rowSet[index][1] %>%
  as.data.frame(stringsAsFactors=F) %>%
  as_tibble() %>%
  mutate_if(check_if_numeric, as.numeric) %>%
  set_names(tolower(unlist(result_sets_df$resultSets$headers[index]))) 
verbose_dataset(verbose, dataset)

return(dataset)}, error=function(e) print(e$message))
}



