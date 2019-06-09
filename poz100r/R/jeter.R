
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggrepel)
library(DBI)
library(RPostgres)

pl_lkup = Lahman::Master %>%
  dplyr::select(playerID, bbrefID, retroID, nameLast, nameFirst, birthYear)

nameAbbv = paste(
  stringr::str_sub(pl_lkup$nameFirst, 1, 1),
  pl_lkup$nameLast, sep=". ")

pl_lkup$nameAbbv = nameAbbv

get_data = function(conn = NULL){

  if (is.null(conn)) {
    conn <- dbConnect(RPostgres::Postgres(),
                      password=Sys.getenv("PSQL_PASS"),
                      user=Sys.getenv("PSQL_USER"),
                      port=Sys.getenv("PSQL_PORT"),
                      dbname='retrosheet')
  }

  df = dbGetQuery(conn, "select * from daybyday_playing_primary")

  pl_df = df %>%
    filter(season_phase=='R') %>%
    group_by(person_key) %>%
    mutate(h=sum(b_h), ab=sum(b_ab)) %>%
    group_by(person_key, b_h) %>%
    summarise(n=n(), h=mean(h), ab=mean(ab)) %>%
    group_by(person_key) %>%
    mutate(nt=sum(n)) %>%
    ungroup() %>%
    mutate(bavg=h/ab, z=n/nt)

  a = pl_df %>%
    group_by(person_key) %>%
    mutate(m=max(b_h)) %>%
    ungroup() %>%
    filter(m >= 4)
}

do_euclid = function(p1, p2, a) {
  x1 = a %>%
    filter(person_key == p1) %$% z
  x2 = a %>%
    filter(person_key == p2) %$% z
  if (length(x1) != length(x2)) {
    return(NULL)
  }
  v = sum((x1-x2)**2)
  list(plid1=p1, plid2=p2,
       euclid=v,
       nt1=sum(x1),
       nt2=sum(x2)
  )
}


do_chi2 = function(p1, p2, a) {
  x1 = a %>%
    filter(person_key == p1) %$% n
  x2 = a %>%
    filter(person_key == p2) %$% n
  v = chisq.test(as.table(rbind(x1, x2)))
  list(plid1=p1, plid2=p2,
       pval=v$p.value,
       chi2=v$statistic %>% as.numeric(),
       nt1=sum(x1),
       nt2=sum(x2)
       )
}


do_euclid_one = function(pref, a) {
  persons = unique(a$person_key)
  n1 = length(persons)
  ll = lapply(1:n1, function(i1) {
    p1 = persons[i1]
    do_euclid(p1, pref, a) %>% bind_rows()
  })

  dfX = ll %>% bind_rows()
}


do_ana_one = function(pref, a) {
  persons = unique(a$person_key)
  n1 = length(persons)
  ll = lapply(1:n1, function(i1) {
    p1 = persons[i1]
    do_chi2(p1, pref, a) %>% bind_rows()
  })

  dfX = ll %>% bind_rows()
}

do_ana_all = function(p1, p2, a) {
  persons = unique(a$person_key)[1:200]
  n1 = length(persons)
  ll = lapply(1:n1, function(i1) {
    p1 = persons[i1]
    lapply(i1:n1, function(i2) {
      p2 = persons[i2]
      do_chi2(p1, p2, a) %>% bind_rows()
    }) %>% bind_rows()
  })

  dfX = ll %>% bind_rows()
}

plot_jeter_h_per_g = function(df) {
  df %>%
    filter(grepl('^jeted', person_key)) %>%
    ggplot(aes(x=b_h, y=n/sum(n))) +
    geom_bar(stat='identity') +
    theme_minimal(base_size = 18) +
    scale_x_continuous(breaks=0:5) +
    labs(x="Hits", y="frac. of career games", title="D. Jeter - Career Hits per Game")
}

plot_bavg_nohit = function(df) {

  low_plids = df %>%
    filter(b_h == 0, nt>=1000) %>%
    filter(z <= 0.25) %$% person_key

  lab_df = df %>%
    filter(b_h == 0, nt>=1000) %>%
    filter(z <= 0.25) %>%
    filter(person_key %in% low_plids) %>%
    merge(pl_lkup, by.x = "person_key", by.y="retroID")

  df %>%
    filter(b_h == 0, nt>=1000) %>%
    ggplot(aes(x=bavg, y=z)) + geom_point() +
    geom_text_repel(data=lab_df, aes(label=nameAbbv)) +
    theme_minimal(base_size = 18) +
    labs(x="Batting Avg.",
         y="frac. 0-hit games",
         title="")
}
