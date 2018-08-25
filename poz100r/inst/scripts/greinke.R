
library(readr)
library(dplyr)

YEAR_LIM = 7

summarise_war_data = function(dfX) {
 
  ww = dfX %>% filter(WAR>=5)
  ww2 = dfX %>% filter(IPouts >= 180 * 3, ERA_plus>=150)
  
  
  war1 = best_in_war(ww)
  era1 = best_in_era_plus(ww2)
  
  ra1 = best_in_ra9(ww2)
  list(ra1=ra1, era1=era1, war1=war1)
}

get_data = function() {
  df = read_csv("https://www.baseball-reference.com/data/war_daily_pitch.txt")
  dfX = df %>% 
    mutate(WAR=as.numeric(WAR), ERA_plus=as.numeric(ERA_plus)) %>% 
    group_by(name_common, age, player_ID, year_ID) %>% 
    summarise(WAR=sum(WAR, na.rm=TRUE), 
              ERA_plus = sum(ERA_plus * IPouts, na.rm=TRUE)/sum(IPouts, na.rm=TRUE), 
              IPouts= sum(IPouts, na.rm=TRUE), RA9=27*sum(RA)/sum(IPouts))

}

best_in_n = function(dfX, y, dy, metric, asc=FALSE) {
  k = enquo(metric)
  tmp = dfX %>%
    filter(year_ID>=y, year_ID<y+dy)

  if (!asc) {
    tmp %>% arrange(desc(!!k)) %>% head(1)
  } else {
    tmp %>% arrange(!!k) %>% head(1)
  }


}

best_in_war = function(dfX) {
  dplyr::bind_rows(
    lapply(1908:2012, function(y) {
      message(y)
      tmp = best_in_n(dfX, y, YEAR_LIM, WAR)
      tmp$start_year = y
      tmp
    })
  )
}

best_in_era_plus = function(dfX) {
  dplyr::bind_rows(
    lapply(1908:2012, function(y) {
      message(y)
      tmp = best_in_n(dfX, y, YEAR_LIM, ERA_plus, asc = FALSE)
      tmp$start_year = y
      tmp
    })
  )
}

best_in_ra9 = function(dfX) {
  dplyr::bind_rows(
    lapply(1908:2012, function(y) {
      message(y)
      tmp = best_in_n(dfX, y, YEAR_LIM, RA9, asc = TRUE)
      tmp$start_year = y
      tmp
    })
  )
}
