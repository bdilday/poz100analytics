
library(dplyr)
library(ggplot2)
library(DBI)
library(RPostgres)

pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')

get_data = function() {
  b = dbGetQuery(conn,
                 paste0("select year_id, bat_id, event_cd, base1_run_id, ",
                        "base2_run_id, base3_run_id, outs_ct, ",
                        "pos3_fld_id, pos4_fld_id, pos5_fld_id, pos6_fld_id, ",
                        "event_outs_ct, fld_cd, dp_fl, event_tx from event ",
                        "where year_id>=1955"))

  aa = b %>%
    mutate(i1 = !is.na(base1_run_id),
           i2 = !is.na(base2_run_id),
           i3 = !is.na(base3_run_id),
           i1=as.integer(i1),
           i2=as.integer(i2),
           i3=as.integer(i3)) %>%
    filter(event_cd %in% c(2,3,14,15,16,18,19,20,21,22,23))
}

process_data = function(dfX, ...) {
  groupers =  enquos(...)
  a = dfX %>%
    mutate(dp_opp = (i1 == 1) & (outs_ct <=1),
           is_dp = (event_outs_ct>=2) & (fld_cd %in% c(4,6)),
           is_dp_br = grepl('GDP', event_tx) & (fld_cd %in% c(4, 6)),
           babip=event_cd %in% c(2, 18:22))

  a = a %>%
    mutate(i_reg_pa=dp_opp == FALSE) %>%
    group_by(!!!groupers) %>%
    summarise(reg_pa=sum(i_reg_pa),
              dp_pa=sum(!i_reg_pa),
              is_dp=sum(is_dp),
              is_dp_br = sum(is_dp_br),
              is_babip = sum(babip),
              is_dp_babip = sum(babip * !i_reg_pa),
              all_pa = reg_pa+dp_pa) %>%
    ungroup()

#  a = a %>% merge(pl_lkup, by.x="bat_id", by.y="retroID")

  a %>% select(-is_dp) %>% rename(is_dp=is_dp_br)
}


get_war_data = function() {
  br_war = readr::read_csv("~/Downloads/war_daily_bat.txt", guess_max = 1000000) %>%
    mutate(WAR = as.numeric(WAR))
}

most_game_together = function(dbd_data) {
  b %>% group_by(person_key, i.person_key) %>%
    summarise(n=n()) %>%
    arrange(-n) %>% head(20) %>%
    merge(pl_lkup, by.x="person_key", by.y='retroID') %>%
    merge(pl_lkup, by.x="i.person_key", by.y='retroID')
}

get_dbd_data = function() {
  b = dbGetQuery(conn, "select * from daybyday_playing_primary ")
  b$pa = ifelse(is.na(b$B_PA), b$B_AB+b$B_BB, b$B_PA)
  b = b %>% filter(pa>0, season_phase=='R') %>% select(game_key:B_CS, F_1B_DP, F_2B_DP, F_3B_DP, F_SS_DP, pa)
  b$k = sprintf("%s_%s", b$game_key, b$team_key)
  dt1 = data.table(b, key='k')
  aa = dt1[dt1, on='k', allow.cartesian=T]
  rm(dt1)
  aa = aa[person_key < i.person_key,]
}

oldtimey_gidp = function(b) {
  # not useful
  b %>%
    mutate(year_id = year(game_date)) %>%
    group_by(person_key, i.person_key) %>%
    summarise(x1=sum(F_SS_DP+i.F_SS_DP), x2=sum(F_2B_DP+i.F_2B_DP), xx=(x1+x2)) %>%
    arrange(-xx) %>% head(20)
}

sum_data_hitrun = function(a) {
  ss = a %>%
    mutate(i1= B_R+B_H > 0, i2 = i.B_R+i.B_H > 0) %>%
    group_by(person_key, i.person_key) %>%
    summarise(z=sum(i1 * i2), m=mean(i1 * i2), n=n()) %>%
    arrange(-n) %>% head(40) %>%
    merge(pl_lkup, by.x="person_key", by.y='retroID') %>%
    merge(pl_lkup, by.x="i.person_key", by.y='retroID')

}

sum_data_hr = function(a) {
  ss = a %>%
    mutate(i1= B_HR > 0, i2 = i.B_HR > 0) %>%
    group_by(person_key, i.person_key) %>%
    summarise(z=sum(i1 * i2), m=mean(i1 * i2), n=n()) %>%
    arrange(-n)

}

harmonic_means = function(br_war) {
  b = br_war %>% mutate(WAR = as.numeric(WAR)) %>%
    filter(WAR > 0) %>%
    select(player_ID, year_ID, team_ID, WAR)
  b = b %>% inner_join(b, by=c("year_ID", "team_ID")) %>%
    filter(player_ID.x < player_ID.y) %>% mutate(z = 2/(1/WAR.x + 1/WAR.y))
}



