#' @name get_all_on_off
#' @rdname get_all_on_off
#' @title Download all on_off combinations and merge into one table
#'
#' @description  Wrapper for processing NBA.com player on_off data 
#' 
#' @param season Number of the year in which season started
#' @param teams Specify team_ids
#' @param ... Additional arguments for get_on_off
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, on_off, players, teams,
#'
#' @examples
#'
#' season <- 2019
#' teams <- c(1610612760,1610612748,1610612759)
#' df <- get_all_on_off(season, teams)
#'
#' @importFrom  lubridate second minute
#' @import dplyr
#' @import tidyr
#' @import httr
#' @import purrr
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @export get_all_on_off
#'


get_all_on_off <- function(season, 
                           teams=c(1610612760,1610612748,1610612759,1610612740,1610612763,1610612751,1610612752,1610612753,1610612749,1610612747,1610612757,1610612761,1610612741,1610612758,1610612756,1610612766,1610612742,1610612743,1610612750,1610612764,1610612737,1610612746,1610612754,1610612762,1610612755,1610612765,1610612744,1610612738,1610612745,1610612739),
                            ...){
  tryCatch({
    
    dataset <- map(teams, function(x) NBAr::get_on_off(season, team_id = x)) %>% 
      compact() %>% 
      bind_rows()
    
    argz <- list(...)
    if(length(argz) > 0){
      dataset <- merge(dataset, data.frame(argz, stringsAsFactors = F) )
    }
    return(dataset)}, error=function(e) NULL)
}



