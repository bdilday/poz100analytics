
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggrepel)
library(zoo)

pl_lkup = Lahman::Master %>%
  dplyr::select(playerID, retroID, bbrefID, nameLast, nameFirst, birthYear)

nameAbbv = paste(
  stringr::str_sub(pl_lkup$nameFirst, 1, 1),
  pl_lkup$nameLast, sep=". ")

pl_lkup$nameAbbv = nameAbbv


get_war_df = function() {
  war_df = readr::read_csv("https://gist.githubusercontent.com/bdilday/28621eb7b91f42d7b90d56475f098cf3/raw/0216fb7fe917695db4b5e90070d90dd3116a6dcf/war_data.csv", guess_max = 1000000)
}

roll_war = function(war_df, plid, k=3) {
  pos = war_df %>% filter(playerID == plid) %$% POS %>% '[['(1)
  y=war_df %>% filter(playerID == plid) %$% rollmax(-yearID, 3) %>% '*'(-1)
  w=war_df %>% filter(playerID == plid) %$% rollmean(WAR_def, 3)
  list(plid=rep(plid, length(y)), y=y, w=w, POS=rep(pos, length(y)))
}
