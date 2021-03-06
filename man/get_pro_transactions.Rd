% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pro_transactions.R
\name{get_pro_transactions}
\alias{get_pro_transactions}
\title{Get transactions from https://www.prosportstransactions.com/basketball/}
\usage{
get_pro_transactions(date_from, date_to, verbose = TRUE)
}
\arguments{
\item{date_from}{Transactions from}

\item{date_to}{Transactions to}

\item{verbose}{Defalt TRUE - prints additional information}
}
\value{
Dataset containing  pro transaction data
}
\description{
Get transactions from https://www.prosportstransactions.com/basketball/
}
\examples{
df = get_pro_transactions('2020-01-01','2020-01-03')

}
\author{
Patrick Chodowski, \email{Chodowski.Patrick@gmail.com}
}
\keyword{NBAr,}
\keyword{injuries,}
\keyword{pro}
\keyword{trades}
\keyword{transactions,}
