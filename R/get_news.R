#' @name get_news
#' @rdname get_news
#' @title Download NBA weekly news 
#'
#' @description  Download NBA weekly news 
#' @param limit Default 100, choose how many recent news you want
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset with weekly news
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, players, injuries, status, rotowire
#'
#' @examples
#' df = get_news()
#'
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @export get_news
#'
#'


get_news <- function(limit=100, verbose=TRUE){
  tryCatch({
    link = glue("https://stats-prod.nba.com/wp-json/statscms/v1/type/spotlight/?limit={limit}")
    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                   'connection' = 'keep-alive',
                                                                   'Accept' = 'application/json',
                                                                   'Host' = 'stats.nba.com',
                                                                   'x-nba-stats-origin'= 'stats')))$content) %>% jsonlite::fromJSON()
    
    dataset = result_sets_df$posts %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric) 
    colnames(dataset) = tolower(colnames(dataset))
    verbose_dataset(verbose, dataset)
    
    return(dataset)}, error=function(e) print(e$message))
}





