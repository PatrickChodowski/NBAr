#' @name get_boxscore
#' @rdname get_boxscore
#' @title Download Boxscore data for NBA game
#'
#' @description  Download and process NBA.com boxscore data for given game and season.
#'
#' @param game_id Game's ID in NBA.com DB
#' @param boxscore_type  Type of boxscore, one from the list: c("advanced","misc","playertrack","traditional")
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset containing data from one of the boxscore pages from NBA.com. Functions enable downloading Traditional, Advanced, Miscallenous or Player Tracking data.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, Boxscores, players, Game, Misc, Advanced, Traditional, Player Tracking
#'
#' @examples
#'
#' type <- c("advanced","misc","playertrack","traditional")[1]
#' get_boxscore(21400001,type)
#'
#'

#' @importFrom  lubridate second minute ms
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @export get_boxscore
#'
#'
#'
get_boxscore <- function(game_id, boxscore_type, verbose = TRUE){
    tryCatch({

      season <- get_season_id(game_id)
      season_type <- get_season_type(game_id)
      

      link = glue("https://stats.nba.com/stats/boxscore{boxscore_type}v2",
                  "?EndPeriod=10",
                  "&EndRange=28800",
                  "&GameID=00{game_id}",
                  "&RangeType=0",
                  "&Season={season}",
                  "&SeasonType={season_type}",
                  "&StartPeriod=1",
                  "&StartRange=0")

      verbose_print(verbose, link)

      result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 
                                                                     'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                     'connection' = 'keep-alive',
                                                                     'Accept' = 'application/json',
                                                                     'Host' = 'stats.nba.com',
                                                                     'x-nba-stats-origin'= 'stats')))$content) %>% jsonlite::fromJSON()
      index <- which(result_sets_df$resultSets$name %in% c("PlayerStats","sqlPlayersMisc","PlayerStats","PlayerTrack"))

      dataset <- result_sets_df$resultSets$rowSet[index][1] %>%
        as.data.frame(stringsAsFactors=F) %>%
        as_tibble() %>%
        mutate_if(check_if_numeric, as.numeric) %>%
        set_names(tolower(unlist(result_sets_df$resultSets$headers[index]))) %>%
        mutate(mins = as.numeric(minute(ms(min))),
               secs = as.numeric(second(ms(min)))) %>%
        select(-min) 
      verbose_dataset(verbose, dataset)

      return(dataset)}, error=function(e) print(e$message))
  }




