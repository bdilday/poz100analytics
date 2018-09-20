
library(dplyr)
library(ggplot2)
library(tidyr)

# ideas
# highest k / BFP from 1908 - 1939
# who led the league 7 years in a row an anything?
# who led the league with highest percentage above 2nd place?

pl_lkup = Lahman::Master %>% select(playerID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv =
  paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')

k_rates1 = function() {
  a = Lahman::Pitching %>%
    filter(!is.na(SO), !is.na(BFP), BFP>0) %>%
    group_by(yearID, lgID) %>%
    mutate(w=BFP, kr = SO/w, m=sum(kr * w)/sum(w),
           m2 = sum(kr*kr*w)/sum(w), s=sqrt(m2-m*m),
           z=(kr-m)/s) %>%
    arrange(-z) %>%
    filter(BFP>=550, yearID>=1901, GS/G > 0.7, z>=3.3) %>%
    #select(playerID, yearID, SO, kr, z) %>%
    as.data.frame()

  a = a %>% mutate(fk = kr/m * 100) %>%
    mutate(kr = kr * 100, m = m * 100) %>%
    arrange(-fk) %>%
    head(20) %>% merge(pl_lkup, by="playerID") %>%
    select(nameAbbv, yearID, lgID, SO, Krate=kr, lg_Krate=m, `Krate+`=fk) %>%
    arrange(-`Krate+`)

    a$Krate = sprintf("%.1f", a$Krate)
    a$lg_Krate = sprintf("%.1f", a$lg_Krate)
    a$`Krate+` = sprintf("%.1f", a$`Krate+`)
    a
}

k_rates3 = function() {
  Lahman::Pitching %>%
    filter(!is.na(SO), !is.na(BFP), BFP>0) %>%
    group_by(yearID) %>%
    mutate(w=BFP, kr = SO/w, m=sum(kr * w)/sum(w),
           m2 = sum(kr*kr*w)/sum(w), s=sqrt(m2-m*m),
           z=(kr-m)/s) %>%
    filter(BFP>=550, yearID>=1901, GS/G > 0.7) %>%
    filter(yearID>=1901, yearID<=21939)

}

k_rates2 = function() {
  a = Lahman::Pitching %>%
    filter(!is.na(SO), !is.na(BFP)) %>%
    group_by(yearID, lgID) %>%
    mutate(w=BFP, kr = SO/w, m=sum(kr * w)/sum(w),
           m2 = sum(kr*kr*w)/sum(w), s=sqrt(m2-m*m),
           z=(kr-m)/s) %>%
    arrange(-z) %>%
    filter(BFP>=550, yearID>=1901, GS/G > 0.7) %>%
    filter(yearID>=1901, yearID<=21939) %>%
    mutate(dk = kr-m) %>%
    arrange(-dk) %>%
    select(playerID, yearID, lgID, SO, kr, m, dk) %>%
    filter(dk>=0.09) %>%
    arrange(yearID) %>%
    as.data.frame()

  a = a %>%
    filter(yearID>=1920, yearID<=1939) %>%
    merge(pl_lkup, by="playerID") %>%
    mutate(kr = kr * 100, m = m * 100, dk = dk * 100) %>%
    select(playerID, nameAbbv, yearID, lgID, SO, Krate=kr, lg_Krate=m, `Krate-LG`=dk) %>%
    arrange(yearID, playerID) %>% select(-playerID)

  a$Krate = sprintf("%.1f", a$Krate)
  a$lg_Krate = sprintf("%.1f", a$lg_Krate)
  a$`Krate-LG` = sprintf("%.1f", a$`Krate-LG`)
  a

}

leading_n = function() {
  Lahman::Pitching %>% group_by(yearID, lgID) %>% mutate(r=rank(-SO, ties.method = "min")) %>% ungroup %>% filter(r <= 2) %>% select(playerID, yearID, lgID, SO) %>% group_by(playerID) %>% mutate(n=n()) %>% ungroup %>% arrange(playerID, yearID) %>% filter(n>=7) %>% as.data.frame()
}

leading_inrow = function(df, metric) {
  metric_enquo = enquo(metric)

  a = df %>%
    group_by(yearID, lgID) %>%
    mutate(r=rank(- !!metric_enquo, ties.method = "min")) %>%
    ungroup %>%
    filter(r == 1) %>%
    select(playerID, yearID, lgID, !!metric_enquo, r) %>%
    arrange(playerID, yearID) %>%
    select(playerID, yearID, lgID, !!metric_enquo, r) %>%
    group_by(playerID) %>% mutate(n= n()) %>% ungroup()
}

old_war = function(war_type = 'pitch') {

  if (war_type == 'pitch') {
    br = read_csv("~/Downloads/war_daily_pitch.txt", guess_max = 1000000) %>% mutate(WAR = as.numeric(WAR))
  } else {
    br = read_csv("~/Downloads/war_daily_bat.txt", guess_max = 1000000) %>% mutate(WAR = as.numeric(WAR))
  }

  br1 = br %>% filter(age<=30) %>% group_by(player_ID) %>% summarise(war=sum(WAR, na.rm=T))
  br2 = br %>% filter(age>30) %>% group_by(player_ID) %>% summarise(war=sum(WAR, na.rm=T))
  br1 %>% merge(br2, all=T, by="player_ID") %>% mutate(war.x=ifelse(is.na(war.x), 0, war.x), war.y=ifelse(is.na(war.y), 0, war.y)) %>% ggplot(aes(x=war.x, y=war.y)) + geom_point()

  plot_df = br1 %>% merge(br2, all=T, by="player_ID") %>% mutate(war.x=ifelse(is.na(war.x), 0, war.x), war.y=ifelse(is.na(war.y), 0, war.y))
  lab_df = plot_df %>% filter(sqrt(war.x**2 + war.y**2) > 60)

  plot_df = plot_df %>% merge(pl_lkup, by.x="player_ID", by.y="bbrefID")
  lab_df = lab_df %>% merge(pl_lkup, by.x="player_ID", by.y="bbrefID")

  p = plot_df %>% ggplot(aes(x=war.x, y=war.y)) + geom_point(size=0.5) + geom_text_repel(data=lab_df, aes(label=nameAbbv)) + geom_abline(slope = 1)  + theme_minimal(base_size = 18) + labs(x="WAR (<=30)", y="WAR (31+)")
}

lead_k9 = function() {
  a = leading_inrow(Lahman::Pitching %>% filter(BFP>=550, GS/G>=0.7) %>% mutate(K_9 = SO/IPouts), K_9)
  a %>% filter(n>=7) %>% as.data.frame() %>% group_by(playerID, n) %>% summarise() %>% merge(pl_lkup, by="playerID") %>% select(nameAbbv, n) %>% arrange(-n)
}

leading_vs_second = function(df, metric) {
  metric_enquo = enquo(metric)

  a = df %>%
    group_by(yearID, lgID) %>%
    mutate(r=rank(- !!metric_enquo, ties.method = "min")) %>%
    ungroup %>%
    filter(r <= 2) %>%
    select(playerID, yearID, lgID, !!metric_enquo, r) %>%
    arrange(playerID, yearID)

  b = a %>% select(yearID, lgID, !!metric_enquo, r) %>%
    distinct %>%
    spread(r, !!metric_enquo) %>%
    mutate(z= (`1`-`2`)/`2`) %>%
    arrange(-z)

  a %>% merge(b, by=c("yearID", "lgID"))
}

lead_vs_second_table = function() {
  a = readr::read_csv("~/Documents/vance_1vs2.csv")
  a = a %>%
    filter(r==1) %>%
    select(nameAbbv, yearID, r, metric) %>% merge(a %>% filter(r==2), by=c("yearID", "metric"))

  a = a %>%
    select(-r.x, -r.y) %>%
    rename(name1=nameAbbv.x,
           name2=nameAbbv.y, value1=v1, value2=v2, metric_diff=z) %>%
    select(yearID, name_1st=name1, value_1st=value1,
           name_2nd=name2, value_2nd=value2, perc_diff=metric_diff, metric) %>%
    arrange(-as.numeric(perc_diff))

  a$perc_diff = sprintf("%.1f", a$perc_diff*100)

  a
}

ranks_stats = function() {
  Lahman::Pitching %>% filter(GS/G>=0.7, BFP>=550) %>% mutate(WHIP = (BB+H)*3/IPouts, k9=SO*27/IPouts, H9 = H*27/IPouts) %>% group_by(yearID, lgID) %>% mutate(whipX = rank(WHIP, ties.method = 'min'), k9X=rank(-k9, ties.method = 'min'), h9X = rank(H9, ties.method = 'min'), eraX = rank(ERA, ties.method = 'min')) %>% filter(whipX<=2) %>% head %>% as.data.frame()

  a = Lahman::Pitching %>%
    filter(GS/G>=0.7, BFP>=550) %>%
    mutate(WHIP = (BB+H)*3/IPouts, k9=SO*27/IPouts, H9 = H*27/IPouts) %>%
    group_by(yearID) %>%
    mutate(whipX = rank(WHIP, ties.method = 'min'),
           k9X=rank(-k9, ties.method = 'min'),
           h9X = rank(H9, ties.method = 'min'),
           eraX = rank(ERA, ties.method = 'min'),
           kX = rank(SO, ties.method = 'min')) %>%
    filter(whipX<=1, eraX <=1, h9X<=1, k9X <=1) %>% arrange(-yearID) %>% select(playerID, yearID) %>% as.data.frame()
  a %>% merge(pl_lkup, by="playerID") %>%
    filter(yearID>=1900) %>% select(nameAbbv, yearID) %>%
    arrange(yearID)
}

leading_vs_second2 = function(df, metric) {
  metric_enquo = enquo(metric)

  a = df %>%
    group_by(yearID) %>%
    mutate(r=rank(- !!metric_enquo, ties.method = "min")) %>%
    ungroup %>%
    filter(r <= 2) %>%
    select(playerID, yearID, !!metric_enquo, r) %>%
    arrange(playerID, yearID)

  b = a %>% select(yearID, !!metric_enquo, r) %>%
    distinct %>%
    spread(r, !!metric_enquo) %>%
    mutate(z= (`1`-`2`)/`2`) %>%
    arrange(-z)

  a %>% merge(b, by=c("yearID")) %>% merge(pl_lkup, by="playerID") %>%
    select(nameAbbv, yearID, r, `1`, `2`, z)

}
