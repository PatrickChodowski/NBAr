#' Download Playbyplay data for NBA game
#'
#' Download and process NBA.com play-by-play data for given game and season.
#' @param game_id Game's ID in NBA.com DB
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset containing data from Playbyplay pages. Play-by-play provides information about every play from NBA game, one action per row.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, play-by-play, game
#'
#' @examples
#' get_playbyplay(21400001)
#'
#' @importFrom  lubridate second minute ms
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import zoo
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @export get_playbyplay


get_playbyplay <- function(game_id, verbose=TRUE){


  tryCatch({
    link <- glue("https://stats.nba.com/stats/playbyplay?GameID=00{game_id}&StartPeriod=0&EndPeriod=14")

    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                   'connection' = 'keep-alive',
                                                                   'Accept' = 'application/json',
                                                                   'Host' = 'data.nba.com',
                                                                   'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()
    index <- which(result_sets_df$resultSets$name == "PlayByPlay")

    dataset <- result_sets_df$resultSets$rowSet[index][1] %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      set_names(tolower(unlist(result_sets_df$resultSets$headers[index]))) %>%
      mutate(mins = as.numeric(minute(ms(pctimestring))),
             secs = as.numeric(second(ms(pctimestring)))) %>%
      mutate(scoremargin = ifelse(scoremargin == 'TIE',0, scoremargin)) %>%
      mutate_if(check_if_numeric, as.numeric) 
    dataset[1,c("score","scoremargin")] <- c("0 - 0", 0)
    dataset[,c("score","scoremargin")] <- apply(dataset[,c("score","scoremargin")], 2, na.locf)

    verbose_dataset(verbose, dataset)

    return(dataset)}, error=function(e) print(e$message))
}

