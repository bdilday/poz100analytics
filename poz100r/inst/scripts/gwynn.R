
library(dplyr)
library(ggplot2)
library(tidyr)
library(RPostgres)
library(DBI)
library(ggalt)

pl_lkup = Lahman::Master %>%
  select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv =
  paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')


frac400 = function(ans, first_date = '1900-01-01') {
  w = ans %>%
    filter(game_date >= first_date) %>%
    mutate(i=z>=0.4) %>%
    group_by(person_key) %>%
    summarise(ff = mean(i), n=n()) %>%
    filter(n >= 1000) %>%
    arrange(-ff) %>%
    merge(pl_lkup, by.x="person_key", by.y="retroID") %>%
    select(name=nameAbbv, frac400 = ff, N = n) %>%
    arrange(-frac400) %>% filter(N>=1000) %>% head(20)

  w$frac400 = sprintf("%.4f", w$frac400)
  w
}

dilone = function() {
  b = Lahman::battingStats()

  b %>%
    filter(SB >= 60, BA>=0.33) %>%
    arrange(-yearID) %>%
    select(playerID, yearID, SB, BA) %>%
    merge(pl_lkup %>%
            select(-playerID), by.x="playerID", by.y="bbrefID") %>%
    select(name=nameAbbv, season=yearID, yearID:BA) %>%
    arrange(-season) %>%
    filter(season>=1900)
}

excursions = function(a) {
  persons = c("gwynt001", 'cobbt101', 'carer001',
              'bretg001', 'boggw001',
              'musis101'
              )
  plot_df = a %>%
    filter(person_key %in% persons) %>%
    group_by(person_key) %>%
    arrange(game_date, game_number) %>%
    mutate(g=row_number()) %>%
    ungroup() %>%
    merge(pl_lkup, by.x="person_key", by.y="retroID")

  plot_df %>%
    ggplot() +
    stat_run(aes(x=b_ab, y=b_h, t=g), run_length=40)
}


bestNg = function(ans, ng=40) {
  w = ans %>%
    filter(ab>=ng*3) %>%
    group_by(person_key) %>%
    filter(z == max(z)) %>%
    arrange(-z) %>%
    mutate(r=row_number()) %>%
    ungroup() %>%
    arrange(-z) %>%
    mutate(idx= row_number()) %>%
    filter(r==1)

  w %>% arrange(-z) %>%
    head(20) %>%
    select(person_key, h:z, game_date) %>%
    merge(pl_lkup, by.x="person_key", by.y="retroID") %>%
    select(name=nameAbbv, end_date=game_date, h, ab, ba=z)  %>%
    arrange(-ba) %>%
    mutate(ba=sprintf("%.4f", ba))
}

frac400_lolli = function() {
  w = frac400(ans, first_date = '1973-01-01')
  w$frac400 = as.numeric(w$frac400)
  w %>%
    ggplot(aes(x=fct_reorder(name, frac400), y=frac400)) +
    geom_lollipop() +
    coord_flip() +
    theme_minimal(base_size = 16) +
    labs(x="player", y="fraction",
         title="fraction of 40-game spans BA>=0.400")
}



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

  nn = tolower(names(dfX))
  names(dfX) = nn
  dfX %>% filter(b_ab > 0, season_phase=='R')

}

do_lag = function(dfX, k, ngame) {
  dfX = dfX %>% filter(person_key == pk)

}

gwynn_running_ba = function(ans) {
  p = ans %>%
    filter(grepl('^gwynt001', person_key)) %>%
    ggplot(aes(x=game_date, y=z)) +
    geom_point() +
    geom_hline(yintercept = 0.419) +
    theme_minimal(base_size = 16) +
    labs(title='T. Gwynn 40-game running BA', x="Date", y="BA")

}

pool_hits = function(dfX, ngame=40, first_date = '1900-01-01') {
  tmp = dfX %>%
    filter(game_date >= first_date)  %>%
    mutate(b_pa = b_ab + b_bb) %>%
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

gwynn_rank = function(dfX, game_stretch = 10, first_date = '1900-01-01') {
 ans = pool_hits(dfX, first_date = first_date, ngame = game_stretch)

 w = ans %>%
   filter(ab>=game_stretch*3) %>%
   group_by(person_key) %>%
   filter(z == max(z)) %>%
   arrange(-z) %>%
   mutate(r=row_number()) %>%
   ungroup() %>%
   arrange(-z) %>%
   mutate(idx= row_number()) %>%
   filter(r==1)

 w1 = w %>% filter(person_key == 'gwynt001') %>% mutate(ng=game_stretch)
 w2 = w %>% filter(idx<=3) %>% mutate(ng = game_stretch)

 dplyr::bind_rows(w1, w2)
}
