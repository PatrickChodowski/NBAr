#' @name get_all_lineups
#' @rdname get_all_lineups
#' @title Download all lineups combinations and merge into one table
#'
#' @description  Wrapper for processing NBA.com player lineups data 
#' 
#' @param season Number of the year in which season started
#' @param group_quantities Specify number of players in a lineup c(2:5)
#' @param measure_types Specify which statistics group you want to download. c('Base','Advanced','Misc','Scoring','Opponent')
#' @param ... Additional arguments for get_lineups
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, lineups, sportvu, players, teams,
#'
#' @examples
#'
#' season <- 2019
#' group_quantities <- c(2:3)
#' measure_types <- c('Base','Advanced')
#' df <- get_all_lineups(season, group_quantities, measure_types)
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
#' @export get_all_lineups
#'


get_all_lineups <- function(season, 
                            group_quantities=c(2:5), 
                            measure_types=c('Base','Advanced','Misc','Scoring','Opponent'),
                            ...){
  tryCatch({
    
    
  all_lineups <- map(measure_types, 
      function(x) map(group_quantities, 
                      function(y) NBAr::get_lineups(season, group_quantity = y, measure_type = x, ...))
      
      )
  
  
  all_lineups_per_type = map(all_lineups, bind_rows)
  
  if(length(all_lineups_per_type) == 1 & nrow(all_lineups_per_type[[1]]) == 0){
    return(NULL)
  } else {
    
    basedf = map_df(all_lineups_per_type, 
                    function(x) x %>% select(player_id_1, player_id_2, player_id_3, player_id_4, player_id_5 )) %>% unique
    full_all_lineups = map(all_lineups_per_type, function(x) left_join(basedf, x, by = c("player_id_1", "player_id_2", "player_id_3", "player_id_4", "player_id_5")))
    
    dataset <- bind_cols(full_all_lineups) %>%
      select(- matches('player_id_1(\\d)|player_id_2(\\d)|player_id_3(\\d)|player_id_4(\\d)|player_id_5(\\d)|group_name(\\d)|team_abbreviation(\\d)|team_id(\\d)|gp(\\d)|w(\\d)|l(\\d)|min(\\d)|w_pct(\\d)|n(\\d)')) 
    
  }
  argz <- list(...)
  if(length(argz) > 0){
    dataset <- merge(dataset, data.frame(argz, stringsAsFactors = F) )
  }
  return(dataset)}, error=function(e) NULL)
}



