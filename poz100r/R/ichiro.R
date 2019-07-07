

b = Lahman::battingStats()

lg_babip_df = b %>% mutate(x1b=H-X2B-X3B-HR) %>% group_by(yearID) %>% summarise(lg_babip = sum(x1b, na.rm=T) / sum(PA, na.rm=T)) %>% ungroup()

b_df = b %>% merge(lg_babip_df, by="yearID")

b_df %>% filter(yearID>=1901) %>% mutate(x=(H-X2B-X3B-HR)/PA) %>% mutate(babip_plus=x/lg_babip) %>% group_by(playerID) %>% summarise(babip_plus=sum(babip_plus*PA, na.rm=T) /  sum(PA, na.rm = T), ab=sum(AB, na.rm = T), pa=sum(PA, na.rm=T))  %>% ungroup %>% arrange(-babip_plus) %>% filter(pa>=5000) %>% mutate(r=row_number())  %>% head(20) %>% as.data.frame()
