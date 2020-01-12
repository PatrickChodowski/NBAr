#' @name get_all_on_off
#' @rdname get_all_on_off
#' @title Download all on_off combinations and merge into one table
#'
#' @description  Wrapper for processing NBA.com player on_off data 
#' 
#' @param season Number of the year in which season started
#' @param teams Specify team_ids
#' @param measure_types Specify which statistics group you want to download. c('Base','Advanced','Misc','Scoring','Opponent')
#' @param ... Additional arguments for get_lineups
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, on_off, players, teams,
#'
#' @examples
#'
#' season <- 2019
#' teams <- c(1610612760,1610612748,1610612759)
#' measure_types <- c('Base','Advanced'')
#' df <- get_all_on_off(season, teams, measure_types)
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
                           measure_types=c('Base','Advanced','Misc','Scoring','Opponent'),
                            ...){
  tryCatch({
    
    all_types <- map(measure_types, 
                       function(x) map(teams, 
                                       function(y) NBAr::get_on_off(season, team_id = y, measure_type = x, ...))
                       
    )
    
    
    all_onoffs_per_type = map(all_types, bind_rows)
    
    if(length(all_onoffs_per_type) == 1 & nrow(all_onoffs_per_type[[1]]) == 0){
      return(NULL)
    } else {
      
      basedf = map_df(all_onoffs_per_type, 
                      function(x) x %>% select(player_id_1, player_id_2, player_id_3, player_id_4, player_id_5 )) %>% unique
      full_all_lineups = map(all_onoffs_per_type, function(x) left_join(basedf, x, by = c("player_id_1", "player_id_2", "player_id_3", "player_id_4", "player_id_5")))
      
      dataset <- bind_cols(full_all_lineups) %>%
        select(- matches('player_id_1(\\d)|player_id_2(\\d)|player_id_3(\\d)|player_id_4(\\d)|player_id_5(\\d)|group_name(\\d)|team_abbreviation(\\d)|team_id(\\d)|gp(\\d)|w(\\d)|l(\\d)|min(\\d)|w_pct(\\d)|n(\\d)')) 
      
    }
    argz <- list(...)
    if(length(argz) > 0){
      dataset <- merge(dataset, data.frame(argz, stringsAsFactors = F) )
    }
    return(dataset)}, error=function(e) NULL)
}



