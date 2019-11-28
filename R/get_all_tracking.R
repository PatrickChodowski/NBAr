#' @name get_all_tracking
#' @rdname get_all_tracking
#' @title Download all SportVu player tracking data at once for Players and Teams.
#'
#' @description  Wrapper for processing NBA.com player tracking data from http://stats.nba.com/players/drives/, http://stats.nba.com/teams/passing/ etc.
#'
#' @param season Number of the year in which season started
#' @param type Specify if data is for Team or Player   c("Player,"Team")
#' @param measure_types Specify which statistics group you want to download. c("Drives","Defense","CatchShoot","Passing","Possessions","PullUpShot","Rebounding","Efficiency","SpeedDistance","ElbowTouch","PostTouch","PaintTouch")
#' @param ... Additional arguments for get_tracking
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, tracking, sportvu, players, teams,
#'
#' @examples
#'
#' season <- 2019
#' type <- c("Player","Team")[1]
#'
#' measure_types <- c("Drives",
#'                  "Defense"
#'                  )
#'
#' df <- get_all_tracking(season, type, measure_types)
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
#' @export get_all_tracking
#'

get_all_tracking <- function(season,
                           type,
                           measure_types = c('Drives','Defense','Passing','Possessions','PullUpShot',
                                            'Rebounding','Efficiency','SpeedDistance','ElbowTouch','PostTouch','PaintTouch'),
                           ...){

  tryCatch({
    
    if(type  == 'Player'){
      all_track <- map(measure_types, function(x) NBAr::get_tracking(season,type,measure_type = x, ...) %>% arrange(player_id))
    } else {
      all_track <- map(measure_types,  function(x) NBAr::get_tracking(season,type,measure_type = x, ...) %>% arrange(team_id))
    }
    
    if(type  == 'Player'){
      basedf = map_df(all_track, function(x) x %>% select(player_id)) %>% unique
      full_all_track = map(all_track, function(x) left_join(basedf, x, by = "player_id"))
    } else {
      basedf = map_df(all_track, function(x) x %>% select(team_id)) %>% unique
      full_all_track = map(all_track, function(x) left_join(basedf, x, by = "team_id"))
    }
    
    dataset <- bind_cols(full_all_track) %>%
      select(- matches('player_id(\\d)|team_abbreviation(\\d)|team_id(\\d)|player_name(\\d)|gp(\\d)|w(\\d)|l(\\d)|min(\\d)|touches(\\d)')) %>%
      select(- matches('paint_touch_fg_pct(\\d)|paint_touch_pts(\\d)|post_touch_fg_pct(\\d)|post_touch_pts(\\d)|elbow_touch_fg_pct(\\d)|elbow_touch_pts(\\d)')) %>%
      select(- matches('dreb(\\d)|points(\\d)|drive_fg_pct(\\d)|drive_pts(\\d)|pull_up_pts(\\d)|pull_up_fg_pct(\\d)|team_name(\\d)'))
    
    return(dataset)}, error=function(e) NULL)
}




























