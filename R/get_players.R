#' @name get_players
#' @rdname get_players
#' @title Download NBA player list for NBA season
#'
#' Download NBA player list for NBA season
#'
#' Download and process list of NBA players for a given season
#'
#' @param season Number of the year in which season started
#' @param only_current 0 To show historical list of all players 1 to show only current season
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset containing data about NBA players who played in specified season.
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, players
#'
#' @examples
#' get_players(2017)
#'
#' @importFrom  lubridate second minute
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#'
#'@export get_players


get_players <- function(season, only_current = 1, verbose=TRUE){


  #tryCatch({
    seasonid <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")
    link  <- glue("https://stats.nba.com/stats/commonallplayers?LeagueID=00&Season={seasonid}&IsOnlyCurrentSeason={only_current}%20")

    verbose_print(verbose, link)
    result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com')))$content) %>% fromJSON()

    index <- which(result_sets_df$resultSets$name == "CommonAllPlayers")
    dataset <- result_sets_df$resultSets$rowSet[index][1] %>%
      as.data.frame(stringsAsFactors=F) %>%
      as_tibble() %>%
      mutate_if(check_if_numeric, as.numeric) %>%
      set_names(tolower(unlist(result_sets_df$resultSets$headers[index])))

    colnames(dataset)[1] <- 'player_id'

    verbose_dataset(verbose, dataset)
    return(dataset)}#, error=function(e) NULL)}

