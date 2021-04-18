#' @name get_hustle
#' @rdname get_hustle
#' @title Download hustle data for NBA game
#'
#' @description  Download and process NBA.com hustle data for given game
#'
#' @param game_id Game's ID in NBA.com DB
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset containing data about specified game hustle plays
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, Boxscores, players, Game, hustle
#'
#' @examples
#'
#' get_hustle(21701181)
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
#' @export get_defense
#' @export get_hustle
#'
#'
#'
get_hustle <- function(game_id, verbose=TRUE){

  tryCatch({

   link<- glue("http://stats.nba.com/stats/hustlestatsboxscore?GameID=00{game_id}")

   verbose_print(verbose, link)
   result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                  'connection' = 'keep-alive',
                                                                  'Accept' = 'application/json',
                                                                  'Host' = 'stats.nba.com',
                                                                  'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()

    index <- which(result_sets_df$resultSets$name == "PlayerStats")
    dataset <- result_sets_df$resultSets$rowSet[index][1] %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric) %>%
      set_names(tolower(unlist(result_sets_df$resultSets$headers[index]))) %>%
      mutate(mins = as.numeric(minute(ms(minutes))),
             secs = as.numeric(second(ms(minutes)))) %>%
      select(-minutes)
    verbose_dataset(verbose, dataset)
    return(dataset)}, error=function(e) print(e$message))
}

