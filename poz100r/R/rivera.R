
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(zoo)
library(stringr)
library(ggrepel)
library(DBI)
library(RPostgres)
# install.
library(marcelR)


pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')

get_data = function() {
  query = paste0("select year_id, pit_id, bat_id, event_cd, count(*) n ",
                 "from event_post group by year_id, pit_id, bat_id, event_cd ",
                 "order by n desc")
  b = dbGetQuery(conn, query) %>%
    filter(event_cd %in% c(2, 3, 14:16, 18, 20:23)) %>%
    mutate(n = as.numeric(n))

  bm = b %>%
    left_join(pl_lkup, by=c("bat_id" = "retroID")) %>%
    left_join(marcels$Batting %>% mutate(yearID = yearID - 1),
              by=c("playerID"="playerID", "year_id"="yearID"))

  # bc = Lahman::battingStats() %>%
  #   group_by(playerID, yearID) %>%
  #   summarise(pa=sum(PA), OPS = sum(PA * OPS, na.rm=T) / sum(PA, na.rm = T)) %>%
  #   filter(pa>0) %>% dplyr::inner_join(pl_lkup, by=("playerID"))

  # add picther ID
  pit_ids = pl_lkup %>% select(retroID, nameAbbv) %>% rename(nameAbbvPitcher = nameAbbv)
  bm %>% left_join(pit_ids, by=c("pit_id" = "retroID"))

}

compute_woba = function(df) {

  woba_weights = readRDS("inst/fg_guts.rds")

  dfX = df %>%
    left_join(woba_weights, by=c("year_id" = "Season")) %>%
    mutate(woba_pts =
             wBB * BB +
             wHBP * HBP +
             w1B * X1B + w2B * X2B + w3B * X3B + wHR * HR,
           proj_woba = woba_pts / proj_pa) %>% select(year_id:SF, nameAbbvPitcher, proj_woba)

  woba_lkup = woba_weights %>% mutate(X = 0) %>%
    select(Season, wBB: wHR, X) %>%
    gather(key, value, -Season)

  event_key_trans =
    data.frame(event_cd = c(2,3,14,15,16,18,20,21,22,23),
               key=c("X","X","wBB","wBB","wBB","wBB","w1B","w2B","w3B","wHR")
    )

  woba_ = woba_lkup %>% left_join(event_key_trans, by=c("key")) %>%
    rename(year_id = Season)

  a = dfX %>% left_join(woba_, by=c("year_id", "event_cd"))

}

get_df = function() {
  bm = get_data()
  w = compute_woba(bm)
}

brett_badass = function() {
  a %>%
    group_by(year_id, pit_id) %>%
    mutate(x = n * value) %>%
    arrange(-x) %>%
    head() %>%
    as.data.frame()
}

rivera_ana_df = function() {
  a = get_df()

  w1 = a1 %>%
    mutate(woba_pts = n * value) %>%
    group_by(year_id, pit_id, nameAbbvPitcher, wOBA) %>%
    mutate(pa=sum(n), woba_allowed=sum(woba_pts)/pa, woba_x = mean(proj_woba, na.rm = T)) %>%
    summarise(pa=mean(pa), woba_allowed=mean(woba_allowed), woba_x=mean(woba_x), lg_wOBA=mean(wOBA)) %>%
    ungroup() %>% mutate(dwoba = woba_allowed-woba_x)

  w2 = a1 %>%
    mutate(woba_pts = n * value) %>%
    group_by(pit_id, nameAbbvPitcher) %>%
    mutate(pa=sum(n), woba_allowed=sum(woba_pts)/pa, woba_x = mean(proj_woba, na.rm = T)) %>%
    summarise(pa=mean(pa), woba_allowed=mean(woba_allowed),
              woba_x=mean(woba_x)) %>%
    ungroup() %>% mutate(dwoba = woba_allowed-woba_x)

  list(w1=w1, w2=w2)
}

plot_career1 = function(dfs) {
  mo = dfs$w2 %>% filter(grepl("^rivem002", pit_id))
  others = dfs$w2 %>%
    mutate(i1=pa>=150 & woba_allowed <= 0.25) %>%
    mutate(i2=pa>=425) %>%
    filter(i1 | i2) %>%
    filter(pit_id != "rivem002")

  p = dfs$w2 %>%
    ggplot(aes(x=pa, y=woba_allowed)) +
    geom_point() +
    coord_cartesian(ylim=c(0.1, 0.5)) + theme_minimal(base_size = 18) +
    labs(x="PA", y="wOBA allowed", title="career post-season")
  p = p +
    geom_point(data=others, color="red", size=2) +
    geom_text_repel(data=others, aes(label = nameAbbvPitcher))
  p = p +
    geom_point(data=mo, color="orange", size=3) +
    geom_text_repel(data=mo, aes(label = nameAbbvPitcher))
}

plot_career2 = function(dfs, pa_lim = 140) {

  mo = dfs$w2 %>% filter(grepl("^rivem002", pit_id))
  others = dfs$w2 %>%
    mutate(i1=pa>=150 & dwoba <= -0.08) %>%
    mutate(i2=pa>=425) %>%
    filter(i1 | i2) %>%
    filter(pit_id != "rivem002")


  p = dfs$w2 %>%
    filter(pa >= 1) %>%
    ggplot(aes(x=pa, y=woba_allowed -woba_x)) +
    geom_point(alpha=0.5) +
    theme_minimal(base_size = 18) +
    labs(x="PA", y="dwOBA (allowed - projected)", title="career post-season")

  p = p +
    geom_point(data=mo, color="orange", size=5) +
    geom_text_repel(data=mo, aes(label = nameAbbvPitcher))

  p = p +
    geom_point(data=others, color="red", size=2) +
    geom_text_repel(data=others, aes(label = nameAbbvPitcher)) +
    coord_cartesian(ylim = c(-0.25, 0.1))
  p = p + geom_hline(yintercept = 0, alpha=0.5)
  p
}

plot_season2 = function(dfs, pa_lim = 40) {

  mo = dfs$w1 %>% filter(grepl("^rivem002", pit_id))
  others = dfs$w1 %>%
    mutate(i1=pa>=55 & dwoba <= -0.16) %>%
    mutate(i2=pa>=101 & dwoba <= -0.101) %>%
    mutate(i3=pa>=151) %>%
    filter(i1 | i2 | i3) %>%
    filter(pit_id != "rivem002") %>%
    select(-i1, -i2, -i3)

  p = dfs$w1 %>%
    filter(pa >= 1) %>%
    ggplot(aes(x=pa, y=woba_allowed -woba_x)) +
    geom_point(alpha=0.5) +
    theme_minimal(base_size = 18) +
    labs(x="PA", y="dwOBA (allowed - projected)", title="seasonal post-season")


  p = p +
    geom_point(data=others, color="red", size=2) +
    geom_text_repel(data=rbind(others, mo), aes(label = paste0(nameAbbvPitcher, year_id)))

  p = p +
    geom_point(data=mo, color="orange", size=3)

  p = p +
    coord_cartesian(ylim = c(-0.5, 0.2))
  p = p + geom_hline(yintercept = 0, alpha=0.5)
  p
}

placeholder = function() {
  q1 = p1 %>% filter(pa>=175, pa<=11150)
  q1 %>% mutate(m1=median(z), s=sd(z), k=(z-m1)/s) %>% arrange(k)

}
