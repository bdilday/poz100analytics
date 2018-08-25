
get_bbref = function() {
  df1 = readr::read_csv("https://www.baseball-reference.com/data/war_daily_bat.txt")
  
}

pl_lkup = Lahman::Master %>% select(bbrefID, nameLast, nameFirst) %>% 
  mutate(nameFull=paste(nameFirst, nameLast), nameAbbv=paste(stringr::str_sub(nameFirst, 1, 1), nameLast))

get_data = function(df1) {

  war_df = df1 %>% 
    mutate(WAR=as.numeric(WAR)) %>% 
    group_by(player_ID, year_ID, age) %>% 
    summarise(war=sum(WAR, na.rm=TRUE)) %>% ungroup() %>% 
    group_by(player_ID) %>% 
    mutate(z=sum(war)) %>% ungroup() %>% 
    merge(pl_lkup, by.x="player_ID", by.y="bbrefID")

}

analyze1 = function (war_df) {
  war_df %>% 
    filter(z>10) %>% 
    group_by(player_ID, nameAbbv) %>% 
    mutate(r=rank(-war)) %>% 
    ungroup() %>% 
    mutate(top5 = r<=5, age30=age>=30, i=top5*age30, ywar=war*!age30) %>% 
    group_by(player_ID, nameAbbv) %>% 
    mutate(ii=sum(i), ywar=sum(ywar)) %>% 
    filter(ywar>=20, ii>=4)
}

make_plots = function(plot_df) {
  plot_df %>% 
    ggplot(aes(x=age, y=war)) + 
    geom_point(aes(color=top5, size=top5)) + 
    geom_line() + 
    facet_wrap(~nameAbbv) + 
    theme_minimal(base_size = 16) + 
    geom_vline(xintercept=29.5, alpha=0.5, linetype=2) + 
    guides(color="none", size="none") + scale_size_manual(values = c(0.5, 2)) + scale_color_manual(values = c("black", "steelblue"))

}

analyze2 = function() {
  clms= c("G", "PA", "HR", "RBI", "H", "X2B", "X3B", "R", "SB")
  
  b = Lahman::Batting %>% 
    group_by(playerID, yearID, lgID) %>% 
    summarise(
      G = sum(G),
      PA = sum(AB+BB+HBP+SH), 
      H=sum(H),
      HR=sum(HR), 
      X2B=sum(X2B), 
      X3B=sum(X3B), R=sum(R), RBI=sum(RBI), SB=sum(SB)) %>% 
    ungroup()
  
  get_leader = function(clm) {
    b$x = as.data.frame(b)[,clm]
    
    b %>% 
      group_by(yearID,lgID) %>% 
      filter(x == max(x)) %>% 
      ungroup() %>% mutate(varname=clm) %>% 
      dplyr::select(playerID, yearID, lgID, statname=varname, statval=x)
  }
  
  ll = lapply(clms, function(clm) {
    get_leader(clm)    
  })

  rr = dplyr::bind_rows(ll)
  
  at_least5 = rr %>% 
    group_by(playerID, yearID, lgID) %>% 
    summarise(n=n()) %>% 
    filter(n>=5) %>% ungroup() %>% dplyr::select(-n)
  
  ld_table = pl_lkup %>% rename(playerID=bbrefID) %>% 
    merge(rr, by="playerID") %>%  
    merge(at_least5, by=c("playerID", "yearID", "lgID")) %>%
    spread(statname, statval) %>% 
    select(playerID, nameAbbv, yearID, lgID, G, PA, H, X2B, X3B, HR, R, RBI, SB) %>% 
    arrange(yearID, lgID, playerID) %>% dplyr::select(-playerID)
  
  ld_table[is.na(ld_table)] = "___"
}