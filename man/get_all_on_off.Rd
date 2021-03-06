% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_all_on_off.R
\name{get_all_on_off}
\alias{get_all_on_off}
\title{Download all on_off combinations and merge into one table}
\usage{
get_all_on_off(
  season,
  teams = c(1610612760, 1610612748, 1610612759, 1610612740, 1610612763, 1610612751,
    1610612752, 1610612753, 1610612749, 1610612747, 1610612757, 1610612761, 1610612741,
    1610612758, 1610612756, 1610612766, 1610612742, 1610612743, 1610612750, 1610612764,
    1610612737, 1610612746, 1610612754, 1610612762, 1610612755, 1610612765, 1610612744,
    1610612738, 1610612745, 1610612739),
  ...
)
}
\arguments{
\item{season}{Number of the year in which season started}

\item{teams}{Specify team_ids}

\item{...}{Additional arguments for get_on_off}
}
\value{
Dataset from stats.nba.com
}
\description{
Wrapper for processing NBA.com player on_off data
}
\examples{

season <- 2019
teams <- c(1610612760,1610612748,1610612759)
df <- get_all_on_off(season, teams)

}
\author{
Patrick Chodowski, \email{Chodowski.Patrick@gmail.com}
}
\keyword{NBAr,}
\keyword{on_off,}
\keyword{players,}
\keyword{teams,}
