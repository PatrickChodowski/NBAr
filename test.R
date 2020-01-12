library(tidyverse)
library(NBAr)



all_lineups <- map(c('Base'), 
                   function(x) map(c(2:3), 
                                   function(y) NBAr::get_lineups(2018, 
                                                                 group_quantity = y, 
                                                                 measure_type = x, 
                                                                 date_from='2018-09-01',
                                                                 date_to = '2018-09-01'))
                   
)

all_lineups_per_type = map(all_lineups, bind_rows)

if(length(all_lineups_per_type) == 1 & nrow(all_lineups_per_type[[1]]) == 0){
  return(NULL)
} else {
  
  basedf = map_df(all_lineups_per_type, 
                  function(x) x %>% select(player_id_1, player_id_2, player_id_3, player_id_4, player_id_5 )) %>% unique
  full_all_lineups = map(all_lineups_per_type, function(x) left_join(basedf, x, by = c("player_id_1", "player_id_2", "player_id_3", "player_id_4", "player_id_5")))
  
  dataset <- bind_cols(full_all_lineups) %>%
    select(- matches('player_id_1(\\d)|player_id_2(\\d)|player_id_3(\\d)|player_id_4(\\d)|player_id_5(\\d)|group_name(\\d)|team_abbreviation(\\d)|team_id(\\d)|gp(\\d)|w(\\d)|l(\\d)|min(\\d)|w_pct(\\d)|n(\\d)')) 
  
}
