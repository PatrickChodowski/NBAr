#' Download improved Playbyplay data for NBA game
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
#' get_playbyplay2(21800001)
#'
#' @importFrom  lubridate second minute ms
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names map_df is_character
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#'
#' @export get_playbyplay2


get_playbyplay2 <- function(game_id, verbose=TRUE){

  tryCatch({

  if(is_character(game_id)){
    game_id <- as.numeric(game_id)
  }

  season_id <- glue('20{str_sub(game_id, 2,3)}')
  if(season_id < 2016){
    warning('Playbyplay 2 not available for seasons older than 2016-17. Returns NULL',call. = FALSE)
    return(NULL)
  }

  link <- glue("https://data.nba.com/data/10s/v2015/json/mobile_teams/nba/{season_id}/scores/pbp/00{game_id}_full_pbp.json")
  verbose_print(verbose, link)
  result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com')))$content) %>% fromJSON()

  cols <- c(ord = 0)
  dataset <- map_df(.x = result_sets_df$g$pd$p, .f = function(.x) result_sets_df$g$pd$pla[[.x]] %>% mutate(period = .x)) %>%
    mutate(season_id = as.numeric(season_id),
           game_id = as.numeric(result_sets_df$g$gid),
           game_date_id = as.numeric(str_replace(str_extract(result_sets_df$g$gcode, '.*/'),'/','')),
           visit_team = str_sub(str_replace(str_extract(result_sets_df$g$gcode, '/.*'),'/',''),1,3),
           home_team = str_sub(str_replace(str_extract(result_sets_df$g$gcode, '/.*'),'/',''),4,6),
           opid = as.numeric(opid),
           epid = as.numeric(epid)) %>%
    add_column(!!!cols[!names(cols) %in% names(.)]) %>%
    select(
      season_id,
      game_id,
      game_date_id,
      visit_team,
      home_team,
      period,
      event_id = evt,
      clock = cl,
      description = de,
      loc_x = locX,
      loc_y = locY,
      opt1,
      opt2,
      message_type = mtype,
      event_type = etype,
      player_id = pid,
      opponent_player_id = opid,
      secondary_player_id = epid,
      team_id = tid,
      offensive_team_id = oftid,
      home_score = hs,
      visit_score = vs,
      order_no = ord) %>% as_tibble() %>%
    mutate(mins = as.numeric(minute(ms(clock))),
           secs = as.numeric(second(ms(clock)))) 

  verbose_dataset(verbose, dataset)

  return(dataset)}, error=function(e) NULL)
}






