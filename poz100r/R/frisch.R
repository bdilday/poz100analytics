
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(zoo)
library(stringr)
library(ggrepel)

pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')

bc = Lahman::Batting %>%
  filter(yearID>=1901) %>%
  group_by(playerID) %>%
  summarise(X3B=sum(X3B), HR=sum(HR), SB=sum(SB, na.rm=T)) %>%
  ungroup() %>% left_join(pl_lkup %>% select(playerID, nameAbbv), by="playerID")

plot1 = function(bc) {
  frisch = bc %>% filter(playerID == 'friscfr01')

  bc %>%
    filter(playerID != 'friscfr01') %>%
    filter(X3B>=100, HR>=100, SB>=0) %>%
    ggplot(aes(x=HR, y=X3B)) +
    geom_point() +
    geom_text_repel(aes(label=nameAbbv)) +
    geom_point(data=frisch, color='maroon', size=4) +
    geom_text_repel(data=frisch, aes(label=nameAbbv), color='maroon', size=4.5) +
    theme_minimal(base_size = 18) + labs(x="Career HR", y="Career Triples")
}

table1 = function(bc) {

}
