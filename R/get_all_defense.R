#' @name get_all_defense
#' @rdname get_all_defense
#' @title Download all SportVu player defense data at once for Players and Teams.
#'
#' @description  Wrapper for processing NBA.com player defense data 
#'
#' @param season Number of the year in which season started
#' @param type Specify if data is for Team or Player   c("Player,"Team")
#' @param defense_categories Specify which defense categories group you want to download. c("Overall","3+Pointers","2+Pointers","Less+Than+6Ft","Less+Than+10Ft","Greater+Than+15Ft")
#' @param ... Additional arguments for get_defense
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, tracking, sportvu, players, teams, defense
#'
#' @examples
#'
#' season <- 2019
#' type <- c("Player","Team")[1]
#'
#' defense_cats <- c("Overall",
#'                  "3+Pointers"
#'                  )
#'
#' df <- get_all_defense(season, type, defense_cats)
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
#' @export get_all_defense
#'

get_all_defense <- function(season,
                             type,
                             defense_categories = c("Overall","3+Pointers","2+Pointers","Less+Than+6Ft","Less+Than+10Ft","Greater+Than+15Ft"),
                             ...){
  
  tryCatch({
    
    
    if(type  == 'Player'){
      all_def <- map(defense_categories, function(x) NBAr::get_defense(season,type,defense_category = x, ...) %>% arrange(close_def_person_id))
    } else {
      all_def <- map(defense_categories,  function(x) NBAr::get_defense(season,type,defense_category = x, ...) %>% arrange(team_id))
    }
    
    if(type  == 'Player'){
      basedf = map_df(all_def, function(x) x %>% select(close_def_person_id)) %>% unique
      full_all_def = map(all_def, function(x) left_join(basedf, x, by = "close_def_person_id"))
    } else {
      basedf = map_df(all_def, function(x) x %>% select(team_id)) %>% unique
      full_all_def = map(all_def, function(x) left_join(basedf, x, by = "team_id"))
    }
    
    dataset <- bind_cols(full_all_def) %>%
      select(- matches('team_id(\\d)|team_name(\\d)|gp(\\d)|freq(\\d)|team_abbreviation(\\d)|plusminus(\\d)')) %>%
      select(- matches('close_def_person_id(\\d)|player_name(\\d)|player_position(\\d)|age(\\d)'))
    
    
    argz <- list(...)
    if(length(argz) > 0){
      dataset <- merge(dataset, data.frame(argz, stringsAsFactors = F) )
    }
    
    
    return(dataset)}, error=function(e)  
      print(e$message))
}




























