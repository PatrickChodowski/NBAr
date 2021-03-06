% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_shooting_dashboard.R
\name{get_shooting_dashboard}
\alias{get_shooting_dashboard}
\title{Download shooting dashboard data for Players and Teams}
\usage{
get_shooting_dashboard(
  season,
  type,
  general_range = "",
  shot_clock_range = "",
  dribble_range = "",
  touch_time_range = "",
  close_def_dist_range = "",
  shot_dist_range = "",
  per_mode = "PerGame",
  season_type = "Regular+Season",
  season_segment = "",
  game_segment = "",
  date_from = "",
  date_to = "",
  outcome = "",
  period = 0,
  opponent_team_id = 0,
  team_id = 0,
  verbose = TRUE
)
}
\arguments{
\item{season}{Number of the year in which season started}

\item{type}{Specify if data is for Team or Player   c("Player,"Team")}

\item{general_range}{= c("Overall","Catch+And+Shoot","Pullups","Less+Than+10+ft")}

\item{shot_clock_range}{= c("24-22","22-18+Very+Early","18-15+Early","15-7+Average","7-4+Late","4-0+Very+Late","ShotClock+Off")}

\item{dribble_range}{= c("0+Dribbles","1+Dribble","2+Dribbles","3-6+Dribbles","7++Dribbles")}

\item{touch_time_range}{= c("Touch+<+2+Seconds","Touch+2-6+Seconds","Touch+6++Seconds")}

\item{close_def_dist_range}{= c("0-2+Feet+-+Very+Tight","2-4+Feet+-+Tight","4-6+Feet+-+Open","6++Feet+-+Wide+Open")}

\item{shot_dist_range}{= c("","> 3D10.0")}

\item{per_mode}{Specify if you want data divided per game or totals. Default parameter is "PerGame". c("PerGame","Totals")}

\item{season_type}{Choose data for preseason, regular season or postseason. Default parameter is "Regular+Season". c("Regular+Season","Playoffs","Pre+Season","All+Star")}

\item{season_segment}{Choose season half for the data. Empty string means whole season and it is set by default. c("","Post+All-Star","Pre+All-Star")}

\item{game_segment}{Choose game half for the data. Empty string means whole game and it is set by default. c("","First+Half","Overtime","Second+Half")}

\item{date_from}{Day from which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.}

\item{date_to}{Day to which data will be collected. It is set in MM/DD/YYYY format and by default is not specified, so data is calculated for whole season.}

\item{outcome}{Filter by game result. It can be a loss (L) or a win (W). By default parameter is an empty string, so both are taken into account. c("","W","L")}

\item{period}{Choose game period for the data. 0 means whole game and it is set by default. as.character(c(0:4))}

\item{opponent_team_id}{Filter by opponent's team id from nba.com database. Default "0" means all teams.}

\item{team_id}{Specifyteam id from nba.com database. Default "0" means all teams.}

\item{verbose}{Defalt TRUE - prints additional information}
}
\value{
Dataset from stats.nba.com
}
\description{
Download and process NBA.com shooting dashboard data from http://stats.nba.com/players/shots-general/, http://stats.nba.com/teams/shots-shotclock/ etc.
}
\examples{

season <- 2016
type <- c("Player","Team")[1]
general_range <- c("Overall","Catch+And+Shoot","Pullups","Less+Than+10+ft")[1]

shot_clock_range <- c("24-22",
"22-18+Very+Early","18-15+Early","15-7+Average","7-4+Late","4-0+Very+Late","ShotClock+Off")[1]

dribble_range <- c("0+Dribbles",
                  "1+Dribble",
                  "2+Dribbles",
                  "3-6+Dribbles",
                  "7++Dribbles"
                  )[1]

touch_time_range <- c("Touch+<+2+Seconds",
                    "Touch+2-6+Seconds",
                    "Touch+6++Seconds"
                    )[1]

close_def_dist_range <- c("0-2+Feet+-+Very+Tight",
                       "2-4+Feet+-+Tight",
                       "4-6+Feet+-+Open",
                       "6++Feet+-+Wide+Open"
                       )[1]

shot_dist_range <- c("",">3D10.0")[1]
per_mode <- c("PerGame","Totals")[1]
season_type <- c("Regular+Season","Playoffs","Pre+Season","All+Star")[1]
season_segment <- c("","Post+All-Star","Pre+All-Star")[1]
game_segment <- c("","First+Half","Overtime","Second+Half")[1]
period <- 0
date_from <- "01/01/2017"
date_to <- "04/30/2017"
outcomde <- c("","W","L")[1]
opponent_team_id <- 0
team_id <- 0

df <- get_shooting_dashboard(
                                                season,
                                                type,
                                                general_range,
                                                shot_clock_range,
                                                dribble_range,
                                                touch_time_range,
                                                close_def_dist_range,
                                                shot_dist_range,
                                                per_mode,
                                                season_type,
                                                season_segment,
                                                game_segment,
                                                date_from,
                                                date_to,
                                                outcome,
                                                period,
                                                opponent_team_id,
                                                team_id
                                                )

}
\author{
Patrick Chodowski, \email{Chodowski.Patrick@gmail.com}
}
\keyword{NBAr,}
\keyword{dashboard,}
\keyword{players,}
\keyword{shooting,}
\keyword{shotclock,}
\keyword{teams}
