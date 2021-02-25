
#' @importFrom  lubridate second minute
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @import stringr
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON



verbose_print <- function(v_true, print_object){
  if(v_true == TRUE){
    print(print_object)
  }
}

verbose_dataset <- function(v_true, dataset){
  if(v_true == TRUE){
    glimpse(dataset)
  }
}


check_if_numeric <- function(x){
  suppressWarnings(all(!is.na(as.numeric(as.character(replace_na(x,0))))))
}


c_to_int <- function(x){
  if(is.numeric(x)){
    x <- as.integer(x)
  }
  return(x)
}



get_season_id <- function(game_id){
  y1 = as.numeric(gsub('^(.{1})(.*)$', '\\10\\2', str_sub(game_id,1,3)))
  return(glue('{y1}-{str_sub(y1+1,3,4)}'))

}


get_season_type <- function(game_id){
  d1 = as.numeric(stringr::str_sub(game_id,1,1))

  y = case_when(d1 == 1 ~ 'Pre+season',
                d1 == 2 ~ 'Regular+Season',
                d1 == 3 ~ 'All+star',
                d1 == 4 ~ 'Playoffs')

  return(y)

}



utils::globalVariables(c('opid',
                         'epid',
                         '.',
                         'game_date_id',
                         'visit_team',
                         'home_team',
                         'period',
                         'evt',
                         'cl',
                         'de',
                         'locX',
                         'locY',
                         'opt1',
                         'opt2',
                         'mtype',
                         'etype',
                         'pid',
                         'tid',
                         'oftid',
                         'hs',
                         'vs',
                         'ord',
                         'minutes',
                         'group_id',
                         'group_set',
                         'group_set.x',
                         'court_status.x',
                         'group_set.y',
                         'court_status.y',
                         'gp.y',
                         'vs_player_id',
                         'vs_player_name',
                         'gp.x',
                         'pctimestring',
                         'scoremargin',
                         'game_id',
                         'Var1',
                         'Var2',
                         'new_col',
                         'player_id', 
                         'team_id',
                         'clock',
                         'close_def_person_id',
                         'player_id_1',
                         'player_id_2',
                         'player_id_3',
                         'player_id_4',
                         'player_id_5',
                         'full_link',
                         'acquired',
                         'relinquished'))







