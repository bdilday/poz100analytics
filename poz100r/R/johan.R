
library(dplyr)
library(magrittr)

pl_lkup = Lahman::Master %>%
  dplyr::select(bbrefID, nameLast, nameFirst, birthYear) %>%
  rename(player_ID=bbrefID)


get_war_data = function() {
  b = readr::read_csv("https://www.baseball-reference.com/data/war_daily_bat.txt", guess_max = 100000)
  p = readr::read_csv("https://www.baseball-reference.com/data/war_daily_pitch.txt", guess_max = 100000)
  b$WAR %<>% as.numeric()
  p$WAR %<>% as.numeric()

  b %<>% group_by(player_ID,year_ID) %>% summarise(WAR=sum(WAR))
  p %<>% group_by(player_ID,year_ID) %>% summarise(WAR=sum(WAR))

  list(b=b, p=p)
}



get_top3 = function(war_df) {

  war_df %>%
    filter(year_ID >=1901) %>%
    group_by(player_ID) %>%
    arrange(player_ID, year_ID) %>%
    mutate(n=n()) %>% filter(n>=4) %>%
    mutate(r=rank(-WAR, ties.method = "random"),
           i=r<=4,
           j=r>4 & r<=8, k = r>8 & r < 15) %>%
    mutate(has_7 = sum(i * (WAR>=7)) == 4) %>%
    summarise(peak=sum(i*WAR)/sum(i),
              offpeak=sum(j * WAR)/sum(j),
              therest=sum(k*WAR)/8,
              nonpeak = sum( (!i) * WAR),
              has_65 = mean(has_7)) %>%
    ungroup() %>% merge(pl_lkup, by="player_ID") %>%
    mutate(nameFull = paste(nameFirst, nameLast))
}

do_plot = function(top3_df) {

  plot_df = top3_df %>%
    filter(peak>= -7.0, offpeak > -1) %>%
    as.data.frame() %>%
    gather(key, value, -player_ID) %>%
    group_by(key) %>%
    mutate(m=mean(value), s=sd(value), z=(value-m)/s) %>%
    ungroup()

  name_contract = function(f, l) {
    paste(stringr::str_sub(f, 1, 1), l)
  }




  player_fct_levels = top3_df %>%
    group_by(player_ID) %>%
    summarise(w=max(peak)) %>%
    ungroup() %>%
    arrange(-w) %$% player_ID

  plot_df = plot_df %>% merge(pl_lkup, by="player_ID")
  plot_df$key = factor(plot_df$key, levels=c("peak", "offpeak", "therest"))
  plot_df$player_ID = factor(plot_df$player_ID, levels=player_fct_levels)

  plot_df$player_ID = factor(plot_df$player_ID, levels=player_fct_levels)

  a = sapply(plot_df$nameFirst, function(s) {stringr::str_sub(s, 1,1)})
  plot_df$nameAbbv = paste0(a, '. ', plot_df$nameLast)
  fs = plot_df %>% mutate(kval = (key=='peak') * value) %>%
    group_by(nameAbbv) %>%
    summarise(kval = max(kval)) %>% ungroup() %>%
    arrange(-kval) %$% nameAbbv


  plot_df$nameAbbv = factor(plot_df$nameAbbv, levels=fs)
  plot_df


  #plot_df %>% ggplot(aes(x=key, y=z, group=player_ID)) + geom_line() + facet_wrap(~nameAbbv) + theme_minimal(base_size = 18) + geom_hline(yintercept = 0, linetype=2, color='gray') + theme(axis.text.x = element_text(angle=60))

  }

