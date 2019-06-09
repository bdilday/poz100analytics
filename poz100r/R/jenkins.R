
library(dplyr)
library(ggplot2)
library(ggrepel)

pl_lkup = Lahman::Master %>%
  dplyr::select(playerID, bbrefID, retroID, nameLast, nameFirst, birthYear)

nameAbbv = paste(
  stringr::str_sub(pl_lkup$nameFirst, 1, 1),
  pl_lkup$nameLast, sep=". ")

pl_lkup$nameAbbv = nameAbbv


get_data = function(){
  p = Lahman::Pitching
  p %>%
    mutate(cg_gs = CG / GS, k_bb = 27*(SO-BB)/IPouts) %>%
    filter(GS >= 15, yearID>=1904)
}

plot_data1 = function(pl_df, plid="^jenkif") {
  plid_df = pl_df %>%
    filter(grepl(plid, playerID)) %>%
    merge(pl_lkup %>%
            select(-playerID), by.x="playerID", by.y="bbrefID")

  pl_df %>%
    ggplot(aes(x=cg_gs, y=k_bb)) +
    geom_point() +
    geom_point(data=plid_df,
               size=5, aes(color=nameAbbv)) +
    scale_colour_manual(values="steelblue") +
    labs(color="") +
    theme_minimal(base_size = 18) +
    labs(x="CG per GS", y="K-BB per 27 outs")
}

plot_data2 = function(pl_df) {
  pl_df %>%
    group_by(playerID) %>%
    summarise(G=sum(G), GS=sum(GS), CG=sum(CG), SO=sum(SO), BB=sum(BB), IPouts=sum(IPouts)) %>%
    mutate(cg_gs=CG/GS, k_bb=27*(SO-BB)/IPouts) %>%
    filter(GS>=200) %>%
    ggplot(aes(x=cg_gs, y=k_bb)) +
    geom_point()

}

get_table = function(pl_df) {
  pl_df %>%
    mutate(cg_gs = CG / GS, k_bb = 27*(SO-BB)/IPouts) %>%
    filter(GS >= 15, yearID>=1904) %>%
    filter(k_bb>=0.2*27, cg_gs>=0.5) %>%
    arrange(-cg_gs)

}

get_table_career = function(pl_df) {
  pl_df %>%
    group_by(playerID) %>%
    summarise(G=sum(G), GS=sum(GS), CG=sum(CG), SO=sum(SO), BB=sum(BB), IPouts=sum(IPouts)) %>%
    mutate(cg_gs=CG/GS, k_bb=27*(SO-BB)/IPouts) %>%
    filter(GS>=100) %>% filter(cg_gs>=0.4, k_bb>=0.1*27) %>%
    arrange(-k_bb) %>%
    as.data.frame() %>%
    arrange(GS)
}

format_table1 = function(df) {
  get_table(df) %>%
    arrange(yearID) %>%
    merge(pl_lkup, by="playerID") %>%
    mutate(IP=IPouts/3, K_IP=SO/IP, BB_IP=BB/IP, K_BB_27=(K_IP-BB_IP)*9) %>% select(nameAbbv, season=yearID, GS, CG, IP, K_IP, BB_IP, K_BB_27) %>%
    rename(name=nameAbbv) %>% mutate(IP=sprintf("%.1f", IP), K_IP=sprintf("%.3f", K_IP), BB_IP=sprintf("%.3f", BB_IP), K_BB_27=sprintf("%.2f",K_BB_27)) %>%
    arrange(name)

}

format_table2 = function(df) {
  get_table_career(df) %>%
    merge(pl_lkup, by="playerID") %>%
    mutate(IP=IPouts/3,
           K_IP=SO/IP,
           BB_IP=BB/IP, K_BB_27=(K_IP-BB_IP)*9) %>%
    select(nameAbbv,
           GS, CG, IP, K_IP, BB_IP, K_BB_27) %>%
    rename(name=nameAbbv) %>%
    mutate(IP=sprintf("%.1f", IP),
           K_IP=sprintf("%.3f", K_IP),
           BB_IP=sprintf("%.3f", BB_IP),
           K_BB_27=sprintf("%.2f",K_BB_27)) %>%
    arrange(name)


}
