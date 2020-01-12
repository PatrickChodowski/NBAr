#' @name get_pro_transactions
#' @rdname get_pro_transactions
#' @title Get transactions from https://www.prosportstransactions.com/basketball/
#'
#' @description  Get transactions from https://www.prosportstransactions.com/basketball/
#'
#' @param date_from Transactions from
#' @param date_to  Transactions to
#' @param verbose Defalt TRUE - prints additional information 
#'
#' @return Dataset containing  pro transaction data
#'
#' @author Patrick Chodowski, \email{Chodowski.Patrick@@gmail.com}
#' @keywords NBAr, pro transactions, injuries, trades
#'
#' @examples
#' df = get_pro_transactions('2020-01-01','2020-01-03')
#'
#' @importFrom  lubridate second minute ms
#' @import dplyr
#' @import tidyr
#' @import httr
#' @importFrom purrr set_names
#' @import tibble
#' @importFrom stringr str_detect str_replace_all
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom jsonlite fromJSON
#' @importFrom rvest html_nodes html_attr html_table
#' @importFrom xml2 read_html
#' @export get_pro_transactions
#'


get_pro_transactions <- function(date_from, date_to, verbose = TRUE){
  tryCatch({

    link <- glue("https://www.prosportstransactions.com/basketball/Search/SearchResults.php?",
                 "Player=&Team=&BeginDate={date_from}&EndDate={date_to}&PlayerMovementChkBx=yes",
                 "&ILChkBx=yes&NBADLChkBx=yes&InjuriesChkBx=yes&PersonalChkBx=yes&DisciplinaryChkBx=yes",
                 "&LegalChkBx=yes&Submit=Search")
    verbose_print(verbose, link)
    wbs<-read_html(link) 
    
    link_list = wbs %>% html_nodes("a") %>% html_attr( "href") %>% tibble() %>%
      filter(str_detect(., 'SearchResult')) %>% 
      mutate(full_link = glue('https://www.prosportstransactions.com/basketball/Search/{.}')) %>% 
      pull(full_link)
    
    all_pro_tr = map(link_list, function(x) html_table(read_html(x))[[1]] %>% slice(-1))
    
    dataset <- bind_rows(all_pro_tr)
    colnames(dataset) <- c('transaction_date','team_name','acquired','relinquished','notes')
    
    ###stringi::stri_escape_unicode("•")
    
    dataset <- dataset %>% 
      mutate(acquired = str_replace_all(acquired,'\\u2022 ',''),
             relinquished = str_replace_all(relinquished,'\\u2022 ',''))
    
    verbose_dataset(verbose, dataset)
    
    return(dataset)}, error=function(e) NULL)
}




