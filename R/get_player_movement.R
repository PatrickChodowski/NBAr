#' @name get_player_movement
#' @rdname get_player_movement
#' @title Download NBA players roster transactions since 2015
#'
#' @description  Download NBA players roster transactions since 2015
#'
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset containing data about roster changes
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, players, roster, trades, signs, contracts
#'
#' @examples
#' df = get_player_movement()
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @export get_player_movement
#'
#'

get_player_movement <- function(verbose = TRUE){
  tryCatch({
    
    link = glue("https://stats.nba.com/js/data/playermovement/NBA_Player_Movement.json")
    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                   'connection' = 'keep-alive',
                                                                   'Accept' = 'application/json',
                                                                   'Host' = 'stats.nba.com',
                                                                   'x-nba-stats-origin'= 'stats')))$content) %>% jsonlite::fromJSON()
    
    dataset = result_sets_df$NBA_Player_Movement$rows %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric)
    
    colnames(dataset) = tolower(colnames(dataset))
    verbose_dataset(verbose, dataset)
    
    return(dataset)}, error=function(e) print(e$message))
}