#' Download NBA season games schedule
#'
#' Downloads and process NBA season games schedule from \url{http://www.basketball-reference.com}
#' @param month Character name of the month
#' @param season Number of the year in which season started
#'
#' @return None
#'
#' @author Patrick Chodowski, \email{kbroman@@biostat.wisc.edu}
#' @references \url{http://www.basketball-reference.com}
#' @keywords NBAr, Schedule, Calendar
#'
#' @examples
#' getSchedule("january","2016")
#'
#' @export getSchedule
#'
#' @import XML
#' @import stringr
#' @import jsonlite
#'

getSchedule <-
  function (month, season)
{
  require(XML)
  require(stringr)
  require(jsonlite)
  mo2Num <- function(x) match(tolower(x), tolower(month.abb))
  seas <- as.numeric(season) + 1
  tryCatch({
    doc.html <- htmlTreeParse(paste("http://www.basketball-reference.com/leagues/NBA_",
                                    seas, "_games-", month, ".html", sep = ""), useInternal = TRUE)
    s <- as.data.frame(readHTMLTable(doc.html, stringsAsFactors = F),
                       stringsAsFactors = F)
    s <- s[, c(1, 3, 4, 5, 6, 8)]
    colnames(s) <- c("GAME_DATE", "VISITOR", "VPTS", "HOME", "HPTS", "OT")
    s$d <- substr(s$GAME_DATE, 6, length(s$GAME_DATE))
    s$d1 <- substr(s$d, 1, 3)
    s$d1 <- mo2Num(s$d1)
    s$d1 <- ifelse(nchar(s$d1) == 1, paste("0", s$d1, sep = ""),
                   s$d1)
    s$d2 <- substr(s$d, 5, 6)
    s$d2 <- ifelse(grepl(",", s$d2) == TRUE, paste("0", substr(s$d2,
                                                               1, 1), sep = ""), s$d2)
    s$d3 <- str_sub(s$d, -4)
    s$DATE <- as.Date(paste(s$d3, s$d1, s$d2, sep = "-"))
    s <- s[, 1:6]
    s$VPTS <- as.numeric(s$VPTS)
    s$HPTS <- as.numeric(s$HPTS)
    return(s)
  }, error = function(err) {
    return(NULL)
  })
}
