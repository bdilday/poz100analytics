

library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggrepel)

pl_lkup = Lahman::Master %>%
  dplyr::select(playerID, bbrefID, nameLast, nameFirst, birthYear)

nameAbbv = paste(
  stringr::str_sub(pl_lkup$nameFirst, 1, 1),
  pl_lkup$nameLast, sep=". ")

pl_lkup$nameAbbv = nameAbbv

get_data = function() {
  b = Lahman::battingStats() %>%
    filter(lgID %in% c("AL", "NL"), yearID>=1901) %>%
    mutate(X1B = H - X2B - X3B - HR)
}


get_lg_data = function(b, stat_list) {
  lapply(stat_list, function(s) {get_lg_data_(b, s)}) %>%
    bind_rows()
}

get_lg_data_ = function(b, stat_quo) {
  s = rlang::quo_name(stat_quo)
  b %>%
    group_by(yearID) %>%
    summarise(z=sum(!!stat_quo, na.rm=T) / sum(PA, na.rm = T)) %>%
    ungroup() %>% mutate(stat_idx = s)


#  lg_babip_df = b %>% mutate(x1b=H-X2B-X3B-HR) %>% group_by(yearID) %>% summarise(lg_babip = sum(x1b, na.rm=T) / sum(PA, na.rm=T)) %>% ungroup()

#  b_df = b %>% merge(lg_babip_df, by="yearID")

 # b_df %>% filter(yearID>=1901) %>% mutate(x=(H-X2B-X3B-HR)/PA) %>% mutate(babip_plus=x/lg_babip) %>% group_by(playerID) %>% summarise(babip_plus=sum(babip_plus*PA, na.rm=T) /  sum(PA, na.rm = T), ab=sum(AB, na.rm = T), pa=sum(PA, na.rm=T))  %>% ungroup %>% arrange(-babip_plus) %>% filter(pa>=5000) %>% mutate(r=row_number())  %>% head(20) %>% as.data.frame()

}

make_babip_plus = function(b) {
  lg_babip_df = b %>%
    group_by(yearID) %>%
    summarise(lgz=sum(BABIP*AB, na.rm=T) / sum(AB, na.rm=T)) %>%
    ungroup()

  babip_df = b %>%
    group_by(yearID, playerID) %>%
    summarise(babip = sum(BABIP*AB, na.rm=T) / sum(AB, na.rm=T),
              ab=sum(AB, na.rm=T)) %>%
    ungroup()

  babip_df %>% merge(lg_babip_df, by="yearID")

}

make_babip_graph1 = function(b) {
  b %>%
    filter(lgID %in% c("AL", "NL"), yearID>=1901) %>%
    group_by(yearID) %>%
    summarise(babip=sum(AB*BABIP, na.rm=T)/sum(AB, na.rm=T),
              ba=sum(AB*BA, na.rm=T)/sum(AB, na.rm=T)) %>%
    ungroup %>% ggplot(aes(x=yearID, y=babip)) +
    geom_line() +
    scale_x_continuous(breaks=seq(1880, 2020, 10)) +
    geom_line(aes(y=ba))
}

make_babip_graph2 = function(b) {
  b %>%
    filter(lgID %in% c("AL", "NL"), yearID>=1901) %>%
    group_by(yearID) %>%
    summarise(babip=sum(AB*BABIP, na.rm=T)/sum(AB, na.rm=T),
              ba=sum(AB*BA, na.rm=T)/sum(AB, na.rm=T)) %>%
    ungroup %>%
    ggplot(aes(x=yearID)) +
    scale_x_continuous(breaks=seq(1880, 2020, 10)) +
    geom_line(aes(y=(babip-ba)*1000)) +
    ylim(0, 50)
}

make_hits_graph1 = function(b) {
  b %>%
    filter(lgID %in% c("AL", "NL"), yearID>=1901) %>%
    mutate(X1B = H - X2B - X3B - HR) %>%
    group_by(yearID) %>%
    summarise(ab=sum(AB, na.rm=T),
              pa=sum(PA, na.rm=T),
              babip=sum(AB*BABIP, na.rm=T)/ab,
              ba=sum(AB*BA, na.rm=T)/ab,
              xbb=sum(BB, na.rm=T)/pa,
              x1b=sum(X1B, na.rm=T)/pa,
              x2b=sum(X2B, na.rm=T)/pa,
              x3b=sum(X3B, na.rm=T)/pa,
              x4b=sum(HR, na.rm=T)/pa,
              so=sum(SO, na.rm=T)/pa
              ) %>%
    ungroup %>%
    ggplot(aes(x=yearID)) +
    scale_x_continuous(breaks=seq(1880, 2020, 10)) +
    geom_line(aes(y=(babip-ba)*1000)) + ylim(0, 50)
}

babip_top_season = function(b) {
  bap = make_babip_plus(b)
  bap %>%
    mutate(babip_plus=babip/lgz) %>%
    filter(ab>=350) %>%
    arrange(-babip_plus) %>%
    head(10) %>%
    merge(pl_lkup, by="playerID") %>%
    arrange(-babip_plus) %>%
    mutate(
      babip = sprintf("%.3f", babip),
      lg_babip=sprintf("%.3f", lgz),
      babip_plus=sprintf("%.2f", babip_plus)) %>%
    select(nameAbbv, yearID, babip, lg_babip, babip_plus)

}

babip_top_career = function(b) {
  bap = make_babip_plus(b)
  bap %>%
    mutate(babip_plus=babip/lgz, AB=ab) %>%
    group_by(playerID) %>%
    summarise(
      babip_plus = sum(babip_plus * AB, na.rm=T) / sum(AB, na.rm=T),
      babip = sum(babip * AB, na.rm=T) / sum(AB, na.rm=T),
      lg_babip = sum(lgz * AB, na.rm=T) / sum(AB, na.rm=T),
      ab = sum(ab, na.rm=T)
      ) %>%
    filter(ab>=3500) %>%
    arrange(-babip_plus) %>%
    head(10) %>%
    merge(pl_lkup, by="playerID") %>%
    arrange(-babip_plus) %>%
    mutate(
      babip = sprintf("%.3f", babip),
      lg_babip=sprintf("%.3f", lg_babip),
           babip_plus=sprintf("%.2f", babip_plus)) %>%
    select(nameAbbv, babip, lg_babip, babip_plus)

}
