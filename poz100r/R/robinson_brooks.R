
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggrepel)
library(zoo)
library(ggalt)
library(forcats)
library(grcdr)

pl_lkup = Lahman::Master %>%
  dplyr::select(playerID, retroID, bbrefID, nameLast, nameFirst, birthYear)

nameAbbv = paste(
  stringr::str_sub(pl_lkup$nameFirst, 1, 1),
  pl_lkup$nameLast, sep=". ")

pl_lkup$nameAbbv = nameAbbv

pos_levels = c("C", "1B", "2B", "3B", "SS", "OF")

get_war_df = function() {
  war_df = readr::read_csv("https://gist.githubusercontent.com/bdilday/28621eb7b91f42d7b90d56475f098cf3/raw/0216fb7fe917695db4b5e90070d90dd3116a6dcf/war_data.csv", guess_max = 1000000)
  war_df$POS = factor(war_df$POS, levels = pos_levels)
}

roll_war = function(war_df, plid, k=3) {
  pos = war_df %>% filter(playerID == plid) %$% POS %>% '[['(1)
  y=war_df %>% filter(playerID == plid) %$% rollmax(-yearID, 3) %>% '*'(-1)
  w=war_df %>% filter(playerID == plid) %$% rollmean(WAR_def, 3)
  list(plid=rep(plid, length(y)), y=y, w=w, POS=rep(pos, length(y)))
}


make_plot1_annotate = function(war_df, text_sz=4, text_nudge=7) {
  p = war_df %>%
    filter(yearID>=1901, playerID!="bankser01") %>%
    group_by(playerID, POS) %>%
    summarise(war_def = sum(WAR_def)) %>%
    group_by(POS) %>%
    mutate(r=rank(desc(war_def), ties.method = "first")) %>%
    ungroup() %>%
    filter(r<=20) %>%
    mutate(POS = factor(POS, pos_levels)) %>%
    merge(pl_lkup %>% select(nameAbbv, bbrefID), by.x = "playerID", by.y="bbrefID") %>%
    ggplot(aes(x=r, y=war_def)) +
    geom_lollipop() +
    facet_wrap(~POS) +
    geom_text(aes(label=nameAbbv), nudge_y=text_nudge, size=text_sz) +
    scale_x_reverse() +
    coord_flip()

  p + theme_minimal(base_size = 16) + labs(x="WAR-defense rank", y="WAR-defense")

}

make_plot1 = function(war_df) {
  war_df %>%
    group_by(playerID, POS) %>%
    summarise(war_def = sum(WAR_def)) %>%
    group_by(POS) %>%
    mutate(r=rank(desc(war_def), ties.method = "first")) %>%
    ungroup() %>%
    filter(r<=20) %>%
    mutate(POS = factor(POS, pos_levels)) %>%
    ggplot(aes(x=r, y=war_def)) +
    geom_lollipop() + facet_wrap(~POS)
}

make_plot2 = function(war_df) {
  war_df %>%
    group_by(playerID, POS) %>%
    summarise(war_def = sum(WAR_def)) %>%
    group_by(POS) %>%
    mutate(r=rank(desc(war_def), ties.method = "first")) %>%
    ungroup() %>%
    filter(r <= 2) %>%
    select(-playerID) %>%
    spread(r, war_def) %>%
    rename(x1=`1`, x2=`2`) %>%
    mutate(z= 2*(x1-x2)/(x1+x2)) %>%
    ggplot(aes(x=fct_reorder(POS, z), y=z)) +
    geom_lollipop() +
    coord_flip()

}

make_plot3 = function(war_df) {

  topn =
    war_df %>%
    filter(POS=="3B") %>%
    group_by(playerID) %>%
    summarise(z=sum(WAR)) %>%
    ungroup %>%
    mutate(r=rank(-z, ties.method="first")) %>%
    filter(r<=16) %>%
    select(playerID)

  war_df %>%
    merge(topn, by="playerID") %>%
    merge(pl_lkup, by="playerID") %>%
    ggplot(aes(x=WAR_def, y=WAR_off)) +
    geom_excursion(aes(t=yearID), run_length = 3) +
    facet_wrap(~fct_reorder(nameAbbv, -WAR_def))
}

make_table1 = function(war_df) {
  war_df %>%
    filter(yearID>=1901) %>%
    filter(playerID!="bankser01") %>% group_by(playerID, POS) %>% summarise(war_def = sum(WAR_def)) %>% group_by(POS) %>% mutate(r=rank(desc(war_def), ties.method = "first")) %>% ungroup() %>% filter(r <= 1) %>% mutate(POS = factor(POS, levels = c("C", "1B", "2B", "3B", "SS", "OF"))) %>% arrange(POS) %>% merge(pl_lkup) %>% select(nameAbbv, POS, war_def) %>% mutate(war_def = sprintf("%.1f", war_def))
}

make_table2 = function(war_df) {
  tmp = war_df %>%
    filter(yearID >=1901) %>%
    filter(playerID!="bankser01") %>%
    group_by(playerID, POS) %>%
    summarise(war_def = sum(WAR_def)) %>%
    group_by(POS) %>%
    mutate(r=rank(desc(war_def), ties.method = "first")) %>%
    ungroup() %>%
    filter(r <= 2) %>%
    arrange(POS, r)

  tmp1 = tmp %>% filter(r==1) %>% merge(pl_lkup) %>% select(POS, name1=nameAbbv, war_def1=war_def)
  tmp2 = tmp %>% filter(r==2) %>% merge(pl_lkup) %>% select(POS, name2=nameAbbv, war_def2=war_def)

  tmp1 %>% merge(tmp2, by="POS") %>%
    mutate(z = 2*(war_def1-war_def2) / (war_def1 + war_def2)) %>%
    mutate(delta_war_pct=sprintf("%.1f", z*100)) %>%
    select(-z)

}


make_table3 = function(war_df) {
  tmp = war_df %>%
    filter(yearID >=1901) %>%
    filter(playerID!="bankser01") %>%
    filter(POS=="3B") %>%
    merge(pl_lkup, by="playerID") %>%
    arrange(-WAR_def) %>%
    head(20) %>%
    select(name=nameAbbv, yearID, WAR_def)

}
