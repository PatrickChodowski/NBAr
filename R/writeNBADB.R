#' Function Wrapper for writing NBA data to PostgreSQL database
#'
#' Provide a dataset name or .RDS file name and write the data dircetly and easy to PostgreSQL database
#'
#' @param dataset Character name of the dataset. It maybe a R dataset or .RDS file
#' @param dbname Name of the PostgreSQL database
#' @param schema Name of the PostgreSQL schema in the database
#' @param pghost Name or IP address of DB host. By default it is localhost.
#' @param pgport Port number of PostgreSQL server. By default it is 5432.
#' @param pguser Database user login. By default it is "postgres"
#' @param pgpassword Database user password By default it is "postgres"
#' @param owrite TRUE/FALSE. Decides if database table should by overwritten or appended to existing table. By default new rows will be appended.
#'
#' @return PostgreSQL table
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, PostgreSQL
#'
#' @examples
#' writeNBADB("GameCalendar2016","NBA","nba16","localhost",5432,"postgres","postgres",overwrite = TRUE)
#'
#' @export writeNBADB
#'
#' @import RPostgreSQL
#'




writeNBADB <- function(dataset, dbname, schema, pghost= "localhost",pgport= 5432, pguser = "postgres", pgpassword="postgres",owrite = FALSE){
  on.exit(dbDisconnect(con))

  tryCatch({

    if(grepl(".RDS",dataset, ignore.case = T) == T){

      data<- readRDS(dataset)
      name <- gsub(".RDS","",dataset)

    } else {

      data <- get(dataset)
      name <- dataset

    }
    require(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = dbname, host = pghost, port = pgport, user = pguser, password = pgpassword)

    if(owrite  == FALSE){
      dbWriteTable(con, c(schema, name), value = data, overwrite = FALSE, row.names = FALSE, append = T)
    }else{
      dbWriteTable(con, c(schema, name), value = data, overwrite = T, row.names = FALSE, append = F)
    }

  },error = function(err){
    print(paste("No table", name ,sep=" "))
  })
}


