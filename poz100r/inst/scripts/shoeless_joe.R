
library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)

b = Lahman::battingStats()

pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')


aggregate_stints = function(dfX) {

  c("playerID", "yearID", "stint", "teamID", "lgID", "G", "AB",
    "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "SO",
    "IBB", "HBP", "SH", "SF", "GIDP", "BA", "PA", "TB", "SlugPct",
    "OBP", "OPS", "BABIP")

  dfX = dfX %>%
    group_by(playerID, yearID, lgID)
}

order_stat = function(dfX, stat_name) {
  dfX %>%
    group_by(playerID, yearID, lgID) %>%
    summarise(x = sum(!!enquo(stat_name))) %>%
    group_by(yearID, lgID) %>%
    arrange(-x) %>%
    mutate(row_idx = rank(-x, ties.method='min'))  %>%
    ungroup() %>%
    arrange(-x) %>%
    mutate(all_time_idx = rank(-x,ties.method = 'min'))

}

triple_graph = function(dfX) {
  plot_df = dfX %>%
    group_by(playerID) %>%
    summarise(X3B=sum(X3B), PA=sum(PA)) %>%
    ungroup() %>%
    mutate(z=X3B/PA) %>%
    filter(PA>=2500)

  p = plot_df %>%
    ggplot(aes(x=PA, y=X3B/PA)) + geom_point()

  lab_df = plot_df %>%
    filter((z > 0.02501) | (PA>=7500 & z>0.02) | (PA>=10000 & z>=0.01)) %>%
    merge(pl_lkup, by='playerID')

  lab_df$k = lab_df$nameAbbv

  sj = plot_df %>% filter(playerID == 'jacksjo01')
  p = p + geom_text_repel(data = lab_df, aes(label = k) )

  p = p + geom_point(data = sj, color='red3', size=5)
}

make_tables = function(b) {

  w = order_stat(b,H)
  tbl_h = w %>% filter(row_idx>=2) %>% head(20) %>% as.data.frame() %>% merge(pl_lkup, by="playerID") %>% select(name=nameAbbv, season=yearID, lg=lgID, H=x, all_time_idx) %>% arrange(-H)

  bw = b %>% filter(yearID>=1901, AB >= 449) %>% mutate(w = 1) %>% group_by(yearID, lgID) %>% mutate(wm = sum(w * BA)/sum(w), wm2 = sum(w * BA *BA)/sum(w), wsd=sqrt(wm2 - wm*wm), wz=(BA-wm)/wm2 ) %>% select(playerID:SB, BA, w:wz) %>% arrange(-wz)
  w = order_stat(bw, wz)
  tbl_hsd = w %>%
    filter(row_idx>=2) %>%
    head(20) %>%
    as.data.frame() %>%
    merge(pl_lkup, by="playerID") %>% select(name=nameAbbv, season=yearID, lg=lgID, H_sd=x, all_time_idx) %>% arrange(-H_sd)



}

