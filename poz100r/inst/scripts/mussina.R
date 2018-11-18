
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(zoo)

pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')

compute_woba = function(){

  b = Lahman::battingStats() %>%
    mutate(X1B = H - X2B - X3B - HR, nil=AB-H)
  woba_weights = load_woba_weights()

  dfX = b %>%
    select(playerID, yearID, X1B, X2B, X3B, HR, BB, HBP, nil) %>%
    gather(key, value, -playerID, -yearID) %>%
    group_by(playerID, yearID, key) %>%
    summarise(value=sum(value, na.rm = T)) %>% ungroup()
  a = dfX %>%
    rename(year_id=yearID, lahman_cd=key) %>%
    dplyr::inner_join(woba_weights, by=c("year_id", "lahman_cd")) %>%
    rename(yearID = year_id)

  woba_mean = readRDS("inst/fg_guts.rds") %>%
    select(yearID=Season, mean_woba=wOBA)

  aa = a %>%
    dplyr::inner_join(woba_mean, by="yearID") %>%
    group_by(playerID, yearID) %>%
    summarise(woba_pts=sum(value*woba_pts),
              n=sum(value),
              lg_woba=mean(mean_woba)) %>%
    ungroup()

  aa = aa %>%
    mutate(z = woba_pts / n,
           rz=(woba_pts + 250*lg_woba)/(n+250))

  aa %>% merge(Lahman::Master, by="playerID") %>%
    select(bat_id=retroID, year_id=yearID, rz)
}

close_miss = function(dfX, stat_name, stat_target, stat_miss_frac=0.9) {
  s = enquo(stat_name)

  dfX %>%
    group_by(playerID, yearID) %>%
    summarise(x = sum(!!s)) %>%
    mutate(i1 = x >= 0.9 * stat_target, i2 = x>= stat_target) %>%
    group_by(playerID) %>%
    summarise(c1=sum(i1), c2 = sum(i2), c12=sum(i1-i2)) %>%
    ungroup %>%
    inner_join(pl_lkup, by="playerID") %>%
    mutate(stat_name=as.character(s)[[2]], stat_target=stat_target) %>%
    select(name=nameAbbv, stat_name, stat_target, close=c12, got_there=c2) %>%
    arrange(-close, got_there)
}

load_woba_weights = function() {
  fg_guts = readRDS("inst/fg_guts.rds")

  woba_yr = function(yr) {
    ev_cds = c(2, 3, 14, 15, 16, 20, 21, 22, 23)
    lahman_cds = c("nil", "-", "BB", "-", "-", "X1B", "X2B", "X3B", "HR")
    ev_weights = fg_guts %>%
      filter(Season == yr) %$% c(0, 0, wBB, wBB, wBB, w1B, w2B, w3B, wHR)
    df = data.frame(year_id=yr,
                    event_cd = ev_cds,
                    lahman_cd = lahman_cds,
                    woba_pts=ev_weights)
  }

  dplyr::bind_rows(lapply(1871:2018, woba_yr))

}

hits_vs_outs = function(rf) {
  only_starters= rf %>%
    filter(inn_ct==1 & inn_new_fl) %>%
    select(game_id, pit_id)

  rf %>%
    mutate(ih=event_cd%in%c(20:23),
           iout = event_outs_ct,
           bo_outs = 3*(inn_ct-1) + outs_ct) %>%
    arrange(game_id, bat_home_id, event_id) %>%
    group_by(game_id, pit_id) %>%
    mutate(h=cumsum(ih), o=cumsum(iout)) %>%
    group_by(game_id, year_id, pit_id, o) %>%
    summarise(h=min(h)) %>% ungroup() %>%
    dplyr::inner_join(only_starters)

}

hits_outs_grouped = function(ho, only_starters) {
  gs = only_starters %>%
    group_by(pit_id) %>%
    summarise(n_starts = n())

  hoX = ho %>%
    filter(h==0) %>%
    group_by(game_id, pit_id) %>%
    summarise(max_outs=max(o)) %>% ungroup() %>%
    inner_join(gs, by="pit_id")
}

hits_outs_counts = function(hoX, target, min_starts=120) {

  hoX %>%
    group_by(pit_id, max_outs, n_starts) %>%
    summarise(n_games = n()) %>%
    ungroup() %>%
    filter(n_starts >= min_starts, max_outs >= target) %>%
    group_by(pit_id, n_starts) %>%
    summarise(n_games=sum(n_games)) %>%
    ungroup() %>%
    mutate(target_outs = target, z=n_games/n_starts) %>%
    arrange(-z)
}

hits_outs_counts_data_frame = function(hoX, min_starts=120) {
  #right_join(fill_in) %>%

  fill_in = hoX %>% select(pit_id, n_starts) %>% distinct() %>% filter(n_starts >= min_starts)

  all_p = as.vector(as.data.frame(fill_in)$pit_id)
  all_s = as.vector(as.data.frame(fill_in)$n_starts)
  n_p = length(all_p)
  fill_in = data_frame(pit_id = rep(all_p, each=27),
                       n_starts = rep(all_s, each=27),
                       target_outs=rep(1:27, n_p))

  lapply(1:27, function(i) {
    hits_outs_counts(hoX, i)
  }) %>%
    dplyr::bind_rows() %>%
    right_join(fill_in) %>%
    mutate(n_games = ifelse(is.na(n_games), 0, n_games), z=n_games/n_starts)

}

hits_outs_plot_baseline = function(ho_df) {
  ho_q_df = ho_df %>%
    group_by(target_outs) %>%
    summarise(wmin=min(z), wmax=max(z),
              w10 = quantile(z, 0.1),
              w90 = quantile(z, 0.9), w50=quantile(z, 0.5)) %>%
    ungroup()

  p1 = ho_df %>%
    ggplot(aes(x=target_outs, y=z))
  p1 = p1 + geom_jitter(size=0.1, alpha=0.15, height = 0)
  #p1 = p1 + geom_violin(aes(group=target_outs))
  p1 = p1 +
    geom_line(data=ho_q_df, aes(x=target_outs, y=w50), size=0.5, color='black') +
    geom_line(data=ho_q_df,aes(y=w10), size=0.15, color='black') +
    geom_line(data=ho_q_df,aes(y=w90), size=0.15, color='black')
  p1 = p1 + theme_minimal(base_size = 16)
  p1 = p1 + scale_y_log10()

  persons = c("mussm001")

  pits_df = ho_df %>%
    filter(pit_id %in% persons) %>%
    merge(pl_lkup, by.x="pit_id", by.y="retroID") %>%
    select(name=nameAbbv, target_outs, z)
    p1 = p1 +
    geom_line(data=pits_df, aes(x=target_outs, y=z), size=1., color='steelblue')  +
    scale_color_brewer(type = "qual", palette = 6) + guides(color='none')

  p1 + labs(x="outs", y="fraction of career starts",
            'title' = 'Outs before 1st hit - M. Mussina') +
    scale_x_continuous(breaks = seq(3, 27, 3))

}

hits_outs_plot = function(ho_df) {
  ho_q_df = ho_df %>%
    group_by(target_outs) %>%
    summarise(wmin=min(z), wmax=max(z),
              w10 = quantile(z, 0.1),
              w90 = quantile(z, 0.9), w50=quantile(z, 0.5)) %>%
    ungroup()

  p1 = ho_df %>%
    ggplot(aes(x=target_outs, y=z))
  #p1 = p1 + geom_jitter(size=0.1, alpha=0.15, height = 0)
  #p1 = p1 + geom_violin(aes(group=target_outs))
  p1 = p1 +
    geom_line(data=ho_q_df, aes(x=target_outs, y=w50), size=0.5, color='black') +
    geom_line(data=ho_q_df,aes(y=w10), size=0.15, color='black') +
    geom_line(data=ho_q_df,aes(y=w90), size=0.15, color='black')
  p1 = p1 + theme_minimal(base_size = 16)
  p1 = p1 + scale_y_log10()


  p2 = ho_df %>%
    ggplot(aes(x=target_outs, y=z))
  #p2 = p2 + geom_jitter(size=0.1, alpha=0.25, height = 0)
  #p2 = p2 + geom_violin(aes(group=target_outs))
  p2 = p2 +
    geom_line(data=ho_q_df, aes(x=target_outs, y=w50), size=0.5, color='black') +
    geom_line(data=ho_q_df,aes(y=w10), size=0.35, color='black') +
    geom_line(data=ho_q_df,aes(y=w90), size=0.35, color='black') +
    geom_line(data=ho_q_df,aes(y=wmin), size=0.15, color='black') +
    geom_line(data=ho_q_df,aes(y=wmax), size=0.15, color='black')
  p2 = p2 + theme_minimal(base_size = 16)
  #p2 = p2 + scale_y_log10()

  persons = c("jones105", "koufs101", "scorh101", "mussm001", "ryann001")

  pits_df = ho_df %>%
    filter(pit_id %in% persons) %>%
    merge(pl_lkup, by.x="pit_id", by.y="retroID") %>%
    select(name=nameAbbv, target_outs, z)
  p1 = p1 +
    geom_line(data=pits_df, aes(x=target_outs, y=z, color=name), size=1.)  +
    scale_color_brewer(type = "qual", palette = 6)
  p1 + labs(x="outs", y="fraction of career starts",
            'title' = 'Outs before 1st hit') +
    scale_x_continuous(breaks = seq(3, 27, 3))
}


load_retro_data = function(conn = NULL, yr) {

  if (is.null(conn)) {
    conn <- dbConnect(RPostgres::Postgres(),
                      password=Sys.getenv("PSQL_PASS"),
                      user=Sys.getenv("PSQL_USER"),
                      port=Sys.getenv("PSQL_PORT"),
                      dbname='retrosheet')
  }


  fields = c("game_id", "year_id", "inn_ct", "outs_ct",
             "away_team_id", "home_team_id", "bat_home_id",
             "bat_id", "pit_id",
             "event_id", "event_cd", "event_outs_ct",
             "bat_fld_cd", "inn_new_fl")

  query_text = paste0("select ", paste0(fields, sep=" ", collapse = ","),
                      " from event where year_id >= ", yr)

  dfX = dbGetQuery(conn, query_text)

}

process_retro_data = function(dfX) {

  dfX = dfX %>% filter(bat_fld_cd != 1)
  dfX$park_key = with(dfX, paste(home_team_id, year_id, sep="_"))
  dfX$pit_key = with(dfX, paste(bat_id, year_id, sep="_"))
  dfX$bat_key = with(dfX, paste(pit_id, year_id, sep="_"))

  woba_weights = load_woba_weights()
  dfX = dfX %>% dplyr::inner_join(woba_weights, by=c("year_id", "event_cd"))
  dfX$year_id %<>% as.factor
  dfX
}

