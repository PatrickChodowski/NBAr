#' Function Wrapper for getting all NBA datasets into R
#'
#' Connect to PostgreSQL database to read all tables from schema into  R environment
#'
#' @param dbname Name of the PostgreSQL database
#' @param schema Name of the PostgreSQL schema in the database
#' @param pghost Name or IP address of DB host. By default it is localhost.
#' @param pgport Port number of PostgreSQL server. By default it is 5432.
#' @param pguser Database user login. By default it is "postgres"
#' @param pgpassword Database user password By default it is "postgres"
#'
#' @return R datasets
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, PostgreSQL
#'
#' @examples
#' getNBADB("NBA","nba16","localhost",5432,"postgres","postgres")
#'
#' @export getNBADB
#'
#' @import RPostgreSQL
#'



getNBADB <- function(dbname, schema, pghost= "localhost",pgport= 5432, pguser = "postgres", pgpassword="postgres"){
  on.exit(dbDisconnect(con))
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname = dbname, host = pghost, port = pgport, user = pguser, password = pgpassword)
  query <-paste("select table_name FROM information_schema.tables where table_schema = \'",schema,"\'",sep="")
  tbs <- as.character(unlist(as.list(dbGetQuery(con, query))))

  i <- 1
  for(i in 1:length(tbs))
  {
    assign(tbs[i],dbReadTable(con, c(schema,tbs[i])), pos =.GlobalEnv)
  }

}

