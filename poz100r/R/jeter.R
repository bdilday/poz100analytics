
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggrepel)
library(DBI)
library(RPostgres)

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
    filter(m >= 4) %>%
    filter(b_h <= 4)
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


do_ana = function(p1, p2, a) {
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

