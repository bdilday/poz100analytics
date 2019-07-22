
library(dplyr)
library(ggplot2)
library(gganimate)
library(magrittr)
library(ggrepel)

pl_lkup = Lahman::Master %>%
  dplyr::select(bbrefID, nameLast, nameFirst, birthYear)

nameAbbv = paste(
  stringr::str_sub(pl_lkup$nameFirst, 1, 1),
  pl_lkup$nameLast, sep=". ")

pl_lkup$nameAbbv = nameAbbv

get_war = function() {
  df1 = readr::read_csv("https://www.baseball-reference.com/data/war_daily_bat.txt")
  df1 %>%
    mutate(WAR=as.numeric(WAR)) %>%
    group_by(player_ID) %>%
    summarise(WAR=sum(WAR, na.rm=T)) %>%
    ungroup()
}

get_data = function(war_df = NULL) {
  if (is.null(war_df)) {
    war_df = get_war()
  }

  b = Lahman::battingStats()
  a = b %>%
    merge(Lahman::Master, by="playerID") %>%
    mutate(age=yearID-birthYear) %>%
    group_by(bbrefID, age, yearID) %>%
    summarise(H= sum(H, na.rm=T)) %>% ungroup() %>%
    merge(war_df, by.x="bbrefID", by.y="player_ID") %>%
    merge(pl_lkup, by="bbrefID") %>%
    select(-nameLast, -nameFirst, -birthYear)

}

get_player_list = function(a, topn=20) {
  p1 = a %>%
    filter(age<=25) %>%
    group_by(bbrefID) %>%
    summarise(H=sum(H)) %>%
    ungroup %>%
    arrange(-H) %>%
    head(topn) %$% bbrefID

  p2 = a %>%
    group_by(bbrefID) %>%
    summarise(WAR=max(WAR)) %>%
    ungroup %>% filter(WAR>=75) %$% bbrefID

  unique(c(p1, p2))
}

get_h_at_age = function(a, age_val) {
  pl = get_player_list(a)
  b = a %>%
    mutate(i = age <= age_val) %>%
    group_by(bbrefID, nameAbbv, WAR) %>%
    summarise(H=sum(i*H)) %>%
    ungroup() %>%
    filter(bbrefID %in% pl)
  b$age_val = age_val
  b
}


make_animation = function(a) {
  ll = lapply(18:40, function(age_val) {get_h_at_age(a, age_val)})
  df = ll %>% bind_rows()
df %>% ggplot(aes(x=H, y=WAR)) + geom_point() + geom_text(aes(label=nameAbbv), size=3, nudge_x = 325) + theme_minimal(base_size = 14) + transition_time(age_val)
}

# a %>%
#   mutate(age=yearID-birthYear) %>%
#   mutate(i=age <= 21) %>%
#   group_by(bbrefID) %>%
#   summarise(h=sum(i*H)) %>% ungroup() %>% arrange(-h)
#
