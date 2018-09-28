
library(dplyr)
library(ggplot2)
library(tidyr)
library(RPostgres)
library(DBI)

get_data = function(conn = NULL, dfX = NULL){

  if (is.null(conn)) {
    conn <- dbConnect(RPostgres::Postgres(),
                      password=Sys.getenv("PSQL_PASS"),
                      user=Sys.getenv("PSQL_USER"),
                      port=Sys.getenv("PSQL_PORT"),
                      dbname='retrosheet')
  }

  dfX = dbGetQuery(conn,
                   paste0("select * from daybyday_playing_primary ",
                          " ")
                   )


}

do_lag = function(dfX, k, ngame) {
  dfX = dfX %>% filter(person_key == pk)

}

pool_hits = function(dfX, ngame=40) {
  tmp = dfX %>%
    filter(b_pa>0) %>%
    arrange(person_key, game_date, game_number) %>%
    group_by(person_key) %>%
    mutate(g = row_number()) %>%
    mutate(h=cumsum(b_h), ab=cumsum(b_ab)) %>%
    ungroup()

  tmp_lag = tmp %>% dplyr::select(person_key, g:ab) %>% mutate(g = g+ngame)

  ans = dplyr::inner_join(tmp, tmp_lag, by=c("person_key", "g")) %>%
    mutate(h=h.x-h.y, ab=ab.x-ab.y, z=h/ab) %>%
    dplyr::select(game_key:person_key, g:z)

  ans

}
