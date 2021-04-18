#' @name get_rotowire_status
#' @rdname get_rotowire_status
#' @title Download NBA players rotowire news and updates
#'
#' @description  Download NBA players rotowire news and updates
#'
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset rotowire news and updates
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, players, injuries, status, rotowire
#'
#' @examples
#' df = get_rotowire_status()
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @export get_rotowire_status
#'
#'
get_rotowire_status <- function(verbose=TRUE){
  tryCatch({
    link = glue("https://stats-prod.nba.com/wp-json/statscms/v1/rotowire/player/")
    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                   'connection' = 'keep-alive',
                                                                   'Accept' = 'application/json',
                                                                   'Host' = 'stats.nba.com',
                                                                   'x-nba-stats-origin'= 'stats')))$content) %>% jsonlite::fromJSON()
    
    dataset = result_sets_df$ListItems %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric) 
    colnames(dataset) = tolower(names(result_sets_df$ListItems))
    verbose_dataset(verbose, dataset)
    
    return(dataset)}, error=function(e) print(e$message))
}