#' Download NBA season games schedule
#'
#' Downloads and process NBA season games schedule from \url{http://NBA.com}. Works for seasons 2015-16 onward.
#' @param season Number of the year in which season started
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset Game ID, Game Date and Time, Arena, Home, Visitor team, series score for playoffs game.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @references \url{http://www.NBA.com}
#' @keywords NBAr, Schedule, Calendar
#'
#' @examples
#' get_schedule(2015)
#'
#' @export get_schedule
#' @importFrom  lubridate second minute
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON

get_schedule <- function(season, verbose=TRUE){

  tryCatch({
    link <- glue("https://data.nba.com/data/10s/v2015/json/mobile_teams/nba/{season}/league/00_full_schedule.json")
    verbose_print(verbose, link)
    result_sets_df <- rawToChar(httr::GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                                         'connection' = 'keep-alive',
                                                                         'Accept' = 'application/json',
                                                                         'Host' = 'data.nba.com',
                                                                         'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()

    list.months <- lapply(1:nrow(result_sets_df$lscd$mscd), FUN =
                            function(x) as.data.frame(result_sets_df$lscd[x,1][1,2], rownames= F)
                          [,c('gid','gcode','seri','gdte','an','gdtutc','utctm')]
    )

    schedule <- bind_rows(list.months) %>% as_tibble()
    schedule$visitor <-  substr(gsub("^.*/","",schedule$gcode),1,3)
    schedule$home <- substr(gsub("^.*/","",schedule$gcode),4,6)
    colnames(schedule) <- c('game_id','gcode','series','game_date','arena','game_date_utc','game_time_utc','visitor','home')

    dataset <- schedule %>%
      mutate(season_type = case_when(str_sub(game_id,3,3) == 1 ~ 'PRE',
                                     str_sub(game_id,3,3) == 2 ~ 'REG',
                                     str_sub(game_id,3,3) == 3 ~ 'ASW',
                                     str_sub(game_id,3,3) == 4 ~ 'POST')
             , season_type_id = str_sub(game_id,3,3)
             ,season = season) %>%
      mutate_if(check_if_numeric, as.numeric) 

    verbose_dataset(verbose, dataset)

    return(dataset)
  }, error = function(err) {
    return(print(err$message))
  })
}
