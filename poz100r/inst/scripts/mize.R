
library(dplyr)
library(ggplot2)
library(ggrepel)

pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')

b = Lahman::battingStats()

plot1 = function(b) {
  b = b %>% filter(yearID >= 1901)
  lab_df = b %>%
    filter(HR>=5, AB>=250) %>%
    mutate(so_=SO/AB, hr_=HR/AB, z=(HR-SO)/AB) %>%
    filter( ((HR-SO)/AB >= 1000) |
              ((HR/AB >= 0.08) & (z >=-0.02)) |
              ( (HR/AB>=0.037) & (z >= 0.01))
            ) %>%
    merge(pl_lkup, by.x='playerID', by.y='playerID') %>% mutate(k=sprintf("%s %d", nameAbbv, yearID))


  lab_df = lab_df %>% mutate(ismize=ifelse(playerID == 'mizejo01', T, F))
  p = b %>% filter(HR>=5, AB>=250) %>%
    mutate(so_=SO/AB, hr_=HR/AB, z=(HR-SO)/AB) %>%
    ggplot(aes(x=hr_, y=z)) +
    geom_point(alpha=0.25, size=0.25) +
    stat_density2d(color='steelblue') +
    geom_hline(yintercept = 0) +
    geom_point(data=lab_df) +
    geom_text_repel(data=lab_df, aes(label=k, color=ismize), size=2.5) +
    scale_color_manual(values = c("gray24", 'red')) + guides(color='none')

  p = p + theme_minimal(base_size = 16) +
    labs(title='HR vs SO rates - Season', x= 'HR / AB', y = ' (HR - SO) / AB')

  p + coord_cartesian(ylim=c(-0.2, 0.05), xlim=c(0, 0.15))

#  p + coord_cartesian(ylim=c(-0.3, 0.05), xlim=c(0, 0.15))
}

plot2 = function(b) {
  b2 = b %>% filter(yearID>=1901) %>% group_by(playerID) %>% summarise(HR=sum(HR), AB=sum(AB), SO=sum(SO))

  lab_df = b2 %>%
    filter(HR>=5, AB>=250) %>%
    mutate(so_=SO/AB, hr_=HR/AB, z=(HR-SO)/AB) %>%
    filter( ((HR-SO)/AB >= 1000) |
              ((HR/AB >= 0.05) & (z >=-0.08)) |
              ( (HR/AB>=0.025) & (z >= -0.02))
    ) %>%
    merge(pl_lkup, by.x='playerID', by.y='playerID') %>% mutate(k=sprintf("%s", nameAbbv))


  lab_df = lab_df %>% mutate(ismize=ifelse(playerID == 'mizejo01', T, F))
  p = b2 %>% filter(HR>=5, AB>=250) %>%
    mutate(so_=SO/AB, hr_=HR/AB, z=(HR-SO)/AB) %>%
    ggplot(aes(x=hr_, y=z)) +
    geom_point(alpha=0.25, size=0.25) +
    stat_density2d(color='steelblue') +
    geom_hline(yintercept = 0) +
    geom_point(data=lab_df) +
    geom_text_repel(data=lab_df, aes(label=k, color=ismize), size=2.5) +
    scale_color_manual(values = c("gray24", 'red')) + guides(color='none')

  p = p + theme_minimal(base_size = 16) +
    labs(title='HR vs SO rates - Career', x= 'HR / AB', y = ' (HR - SO) / AB')

  p + coord_cartesian(ylim=c(-0.3, 0.015), xlim=c(0, 0.1))
}
