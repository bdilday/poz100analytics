
library(dplyr)
library(ggplot2)

pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')

load_br_data = function() {
  br_war = readr::read_csv("~/Downloads/war_daily_bat.txt", guess_max = 1000000) %>%
    mutate(age = as.numeric(age)) %>%
    mutate(runs_bat = as.numeric(runs_bat)) %>%
    mutate(runs_br = as.numeric(runs_br)) %>%
    mutate(runs_defense = as.numeric(runs_defense))

  br_war %>% group_by(player_ID, year_ID) %>%
    summarise(runs_bat=sum(runs_bat),
              runs_br = sum(runs_br),
              runs_defense = sum(runs_defense)) %>% ungroup()

}

load_br_data_career = function() {
  br_war = readr::read_csv("~/Downloads/war_daily_bat.txt", guess_max = 1000000) %>%
    mutate(age = as.numeric(age)) %>%
    mutate(runs_bat = as.numeric(runs_bat)) %>%
    mutate(runs_br = as.numeric(runs_br)) %>%
    mutate(runs_defense = as.numeric(runs_defense))

  br_war %>% group_by(player_ID) %>%
    summarise(runs_bat=sum(runs_bat),
              runs_br = sum(runs_br),
              runs_defense = sum(runs_defense)) %>% ungroup()

}

top10_hm_runs = function(br_war) {
  br_war %>%
    ungroup() %>%
    filter(runs_br>0, runs_bat>0, runs_defense>0) %>%
    mutate(z = 3/(1/runs_bat + 1/runs_br + 1/runs_defense)) %>%
    arrange(-z) %>%
    select(player_ID, year_ID, runs_bat, runs_br, runs_defense, z) %>%
    head(10) %>%
    merge(pl_lkup, by.x="player_ID", by.y="bbrefID") %>%
    select(name=nameAbbv, year_ID, runs_bat:runs_defense, hm=z) %>%
    arrange(-hm) %>%
    mutate(runs_bat = sprintf("%.1f", runs_bat),
           runs_br=sprintf("%.1f", runs_br),
           runs_defense=sprintf("%.1f", runs_defense),
           hm=sprintf("%.1f", hm))

}

season_gt_75 = function(br_war) {
  br_war %>%
    ungroup() %>%
    filter(runs_br>0, runs_bat>0, runs_defense>0) %>%
    mutate(z = 3/(1/runs_bat + 1/runs_br + 1/runs_defense)) %>%
    arrange(-z) %>%
    select(player_ID, year_ID, runs_bat, runs_br, runs_defense, z) %>%
    filter(z >= 7.5) %>%
    count(player_ID, sort = T) %>%
    filter(n>=4) %>%
    merge(pl_lkup, by.x="player_ID", by.y="bbrefID") %>%
    select(name = nameAbbv, Season=n) %>% arrange(-Season, name)
}

load_lahman_data = function() {
  b = Lahman::battingStats()
  b = b %>% group_by(playerID, yearID) %>%
    summarise(SB=sum(SB), CS=sum(CS), HR=sum(HR))
}

load_lahman_data_career = function() {
  b = Lahman::battingStats()
  b = b %>% group_by(playerID, yearID) %>%
    summarise(SB=sum(SB), CS=sum(CS), HR=sum(HR))
}

best_sb_pct = function(sb) {
  dfX = sb %>% mutate(hm=2/(1/HR+1/SB), z=SB/(SB+CS)) %>%
    filter(SB>=30) %>% arrange(-z) %>%
    head(20) %>% as.data.frame() %>%
    merge(pl_lkup, by.x = "playerID", by.y="playerID")

  dfX %>% arrange(-z) %>% mutate(z=sprintf("%.1f", z*100)) %>% select(name=nameAbbv, Season=yearID, SB, CS, pct=z)


}

best_hr_sb_sbpct_seasons = function(sb, sb_cut=23, hr_cut=23, sb_pct_cut=0.88) {
  sb %>% mutate(z=SB/(SB+CS), hm=2/(1/HR+1/SB)) %>%
    filter(SB>=sb_cut, HR>=hr_cut, z>=sb_pct_cut) %>%
    group_by(playerID) %>%
    mutate(n=n()) %>% ungroup()
}
