
library(dplyr)
library(ggplot2)
library(ggrepel)

pl_lkup = Lahman::Master %>%
  dplyr::select(bbrefID, nameLast, nameFirst, birthYear) %>%
  rename(playerID=bbrefID)

nameAbbv = paste(
  stringr::str_sub(pl_lkup$nameFirst, 1, 1),
  pl_lkup$nameLast, sep=". ")

pl_lkup$nameAbbv = nameAbbv

get_data = function(){
  p = Lahman::Pitching
  p %>%
    mutate(cg_gs = CG / GS, k_bb = (SO-BB)/IPouts) %>%
    filter(GS >= 15, yearID>=1904)
}

plot_data1 = function(pl_df, plid="^jenkif") {
  plid_df = pl_df %>% filter(grepl(plid, playerID))
  pl_df %>%
    ggplot(aes(x=cg_gs, y=k_bb)) +
    geom_point() +
    geom_point(data=plid_df, size=5, color='steelblue')
}

plot_data2 = function(pl_df) {
  pl_df %>%
    group_by(playerID) %>%
    summarise(G=sum(G), GS=sum(GS), CG=sum(CG), SO=sum(SO), BB=sum(BB), IPouts=sum(IPouts)) %>%
    mutate(cg_gs=CG/GS, k_bb=(SO-BB)/IPouts) %>%
    filter(GS>=200) %>%
    ggplot(aes(x=cg_gs, y=k_bb)) +
    geom_point()

  pl_df %>%
    group_by(playerID) %>%
    summarise(G=sum(G), GS=sum(GS), CG=sum(CG), SO=sum(SO), BB=sum(BB), IPouts=sum(IPouts)) %>%
    mutate(cg_gs=CG/GS, k_bb=(SO-BB)/IPouts) %>%
    filter(GS>=100) %>%
    filter(cg_gs>=0.4, k_bb>=0.1) %>%
    arrange(-k_bb)
}

get_table = function(pl_df) {
  pl_df %>%
    mutate(cg_gs = CG / GS, k_bb = (SO-BB)/IPouts) %>%
    filter(GS >= 15, yearID>=1904) %>%
    filter(k_bb>=0.2, cg_gs>=0.5) %>%
    arrange(-cg_gs)

}

get_table_career = function(pl_df) {
  pl_df %>%
    group_by(playerID) %>%
    summarise(G=sum(G), GS=sum(GS), CG=sum(CG), SO=sum(SO), BB=sum(BB), IPouts=sum(IPouts)) %>%
    mutate(cg_gs=CG/GS, k_bb=(SO-BB)/IPouts) %>%
    filter(GS>=100) %>% filter(cg_gs>=0.4, k_bb>=0.1) %>%
    arrange(-k_bb) %>%
    as.data.frame() %>%
    arrange(GS)
}

