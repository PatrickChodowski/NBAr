#' @name get_playtype
#' @rdname get_playtype
#' @title Download Playtype data for Players and Teams
#'
#' @description  Download and process NBA.com Playtype data from http://stats.nba.com/players/playtype/isolation/
#'
#' @param season Number of the year in which season started
#' @param type Specify if data is for Team or Player   c("P,"T")
#' @param per_mode Specify PerGame or Totals c("PerGame","Totals")
#' @param playtype Pick one of the playtypes provided by stats.nba.com. c("Postup","Transition","Isolation","PRBallHandler","PRRollman","Spotup","Handoff","Cut","OffScreen","OffRebound","Misc")
#' @param type_grouping Specify if data is for offensive or defensive side of the floor. c("offensive","defensive")
#' Warning! 'Transition','Cut','OffRebound', 'Misc' do not work for defensive!
#' @param season_type Choose data for preseason, regular season or postseason ("Regular+Season","Playoffs")
#' @param verbose Defalt TRUE - prints additional information
#'
#' @return Dataset from stats.nba.com
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, Playtypes, players, teams,
#'
#' @examples
#' get_playtype(2018,"P","Isolation")
#'
#' season <- 2016
#' type_grouping <- c("offensive","defensive")[1]
#' type <- c("P","T")[1]
#' per_mode <- c("PerGame")[1]
#'
#' playtype <- c("Postup",
#'               "Transition",
#'               "Isolation",
#'               "PRBallHandler",
#'               "PRRollman",
#'               "Spotup",
#'               "Handoff",
#'               "Cut",
#'               "OffScreen",
#'               "OffRebound",
#'               "Misc"
#'               )[1]
#'
#' season_type <- c("Regular+Season","Playoffs")[1]
#'
#' df <- get_playtype(Season,Type,Playtype,TypeGrouping,SeasonType,PerMode)
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
#' @export get_playtype


get_playtype <- function(season,
                        type,
                        playtype,
                        type_grouping = "offensive",
                        season_type = "Regular+Season",
                        per_mode = 'PerGame',
                        verbose=TRUE

){

  tryCatch({

    season_id <- paste(season, as.numeric(substring(season,3,4))+1,sep="-")

link <- glue("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&",
            "&PerMode={per_mode}",
            "&PlayType={playtype}",
            "&PlayerOrTeam={type}",
            "&SeasonType={season_type}",
            "&SeasonYear={season_id}",
            "&TypeGrouping={type_grouping}")
verbose_print(verbose, link)

result_sets_df <- rawToChar(GET(link, add_headers(.headers = c('Referer' = 'http://google.com', 'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36',
                                                               'connection' = 'keep-alive',
                                                               'Accept' = 'application/json',
                                                               'Host' = 'stats.nba.com',
                                                               'x-nba-stats-origin'= 'stats')))$content) %>% fromJSON()


dataset <- result_sets_df$resultSets$rowSet %>%
  as.data.frame(stringsAsFactors=F) %>%
  as_tibble() %>%
  mutate_if(check_if_numeric, as.numeric) %>%
  set_names(tolower(unlist(result_sets_df$resultSets$headers))) 

verbose_dataset(verbose, dataset)



return(dataset)}, error=function(e) print(e$message))
}
