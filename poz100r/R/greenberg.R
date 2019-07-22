
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggrepel)

pl_lkup = Lahman::Master %>%
  dplyr::select(bbrefID, nameLast, nameFirst, birthYear)

nameAbbv = paste(
  stringr::str_sub(pl_lkup$nameFirst, 1, 1),
  pl_lkup$nameLast, sep=". ")

pl_lkup$nameAbbv = nameAbbv


get_data = function() {
  b = Lahman::battingStats()
  hr_df = b %>%
    group_by(playerID, yearID) %>%
    summarise(ab=sum(AB, na.rm = T),
              pa=sum(PA, na.rm = T),
              hr=sum(HR, na.rm = T)) %>%
    ungroup()

  hr_age_df = hr_df %>%
    merge(Lahman::Master %>%
            select(-playerID), by.x = "playerID", by.y = "bbrefID") %>%
    mutate(age=yearID-birthYear)
  adf = hr_age_df %>% mutate(hr_pa=hr/pa)
}


get_hr_data = function() {
  adf = get_data()
  adf %>%
    mutate(
      i1=age>=26 & age<=29,
      i3=age>=34 & age<=36,
      i2=age>=30 & age<=33) %>%
    group_by(playerID) %>%
    summarise(
      pa1 = sum(pa*i1, na.rm=T),
      pa2 = sum(pa*i2, na.rm=T),
      pa3 = sum(pa*i3, na.rm=T),

      hr1 = sum(i1*hr, na.rm=T),
      hr2 = sum(i2*hr, na.rm=T),
      hr3 = sum(i3*hr, na.rm=T),

      hr_pa1 = sum(i1*hr, na.rm=T) / pa1,
      hr_pa2 = sum(i2*hr, na.rm=T) / pa2,
      hr_pa3 = sum(i3*hr, na.rm=T) / pa3
    )
}

get_hr_plus_data = function(b) {
  adf = get_data()
  lg_hr_df = get_lg_hr(b)
  aa = adf %>%
    merge(lg_hr_df, by="yearID") %>%
    mutate(hr_plus = hr_pa / lg_hr_pa)

  aa %>%
    mutate(
      i1=age>=26 & age<=29,
      i3=age>=34 & age<=36,
      i2=age>=30 & age<=33) %>%
    group_by(playerID) %>%
    summarise(
      pa1 = sum(pa*i1, na.rm=T),
      pa2 = sum(pa*i2, na.rm=T),
      pa3 = sum(pa*i3, na.rm=T),
      hr1 = sum(i1*hr_plus*pa, na.rm=T) / pa1,
      hr2 = sum(i2*hr_plus*pa, na.rm=T) / pa2,
      hr3 = sum(i3*hr_plus*pa, na.rm=T) / pa3
    )
}

get_lg_hr = function(b) {
  b %>%
    group_by(yearID) %>%
    summarise(lg_hr_pa = sum(HR, na.rm=T) / sum(PA, na.rm=T)) %>%
    ungroup()
}


hr_table1 = function(hdf=NULL) {
  if (is.null(hdf)) {
    hdf = get_hr_data()
  }

  hdf %>%
    filter(hr_pa1>=0.006, hr1>=165) %>%
    merge(pl_lkup, by.x ="playerID", by.y="bbrefID") %>%
    select(nameAbbv, hr26_29=hr1, hr_pa=hr_pa1, hr30_33=hr2) %>%
    mutate(hr_pa=sprintf("%.3f", hr_pa)) %>%
    arrange(-hr26_29)

}

hr_table2 = function(hdf=NULL) {
  if (is.null(hdf)) {
    hdf = get_hr_data()
  }

  hdf %>%
    filter(hr_pa3>=0.057, pa3>=950) %>%
    merge(pl_lkup, by.x ="playerID", by.y="bbrefID") %>%
    select(nameAbbv, hr34_36=hr3, hr_pa=hr_pa3, hr30_33=hr2) %>%
    mutate(hr_pa=sprintf("%.3f", hr_pa)) %>% arrange(-hr34_36)
}

hr_table3 = function(hdf=NULL) {
  if (is.null(hdf)) {
    hdf = get_hr_data()
  }

  hdf %>% filter(hr_pa1>=0.06, hr_pa3>=0.055, pa3>=950) %>%
    merge(pl_lkup, by.x ="playerID", by.y="bbrefID") %>%
    select(nameAbbv,
           hr_pa26_29=hr_pa1,
           hr_pa34_36=hr_pa3,
           hr_pa30_33=hr_pa2,
           hr30_33=hr2) %>%
    mutate(hr_pa26_29=sprintf("%.3f", hr_pa26_29),
           hr_pa34_36=sprintf("%.3f", hr_pa34_36),
           hr_pa30_33=sprintf("%.3f", hr_pa30_33)) %>%
    arrange(nameAbbv)

}

hr_table4 = function() {
  hdf %>% filter(hr_pa1>=0.05, hr_pa3>=0.05, pa3>=950) %>%
    merge(pl_lkup, by.x ="playerID", by.y="bbrefID") %>%
    mutate(hr_pa1=sprintf("%.3f", hr_pa1),
           hr_pa3=sprintf("%.3f", hr_pa3)) %>% arrange(nameAbbv)

}

hr_table5 = function(hdf=NULL) {
  if (is.null(hdf)) {
    hdf = get_hr_data()
  }

  hdf %>%
    filter(hr1>4, hr3>4) %>%
    merge(pl_lkup, by.x="playerID", by.y="bbrefID") %>%
    mutate(hr_pa_plus26_29=hr1, hr_pa_plus34_36=hr3) %>%
    mutate(hr_pa_plus26_29=sprintf("%d", round(100*hr_pa_plus26_29))) %>%
    mutate(hr_pa_plus34_36=sprintf("%d", round(100*hr_pa_plus34_36)))
}

hr_graph1 = function(hdf = NULL) {
  if (is.null(hdf)) {
    hdf = get_hr_data()
  }

  pl_df = hdf %>%
    filter(hr_pa1>=0.02, pa1>=1200) %>%
    merge(pl_lkup, by.x ="playerID", by.y="bbrefID")

  lab_df = pl_df %>% filter(grepl('^greenha01', playerID))

  pl_df %>%
    ggplot(aes(x=hr1, y=hr2)) +
    geom_point() +
    geom_point(data=lab_df, color="steelblue") +
    geom_smooth() +
    theme_minimal(base_size = 16) +
    labs(x="HR (age 26-29)", y="HR (age 30-33)") +
    geom_text_repel(data=lab_df, aes(label=nameAbbv))
}

hr_graph2 = function(hdf = NULL) {
  if (is.null(hdf)) {
    hdf = get_hr_data()
  }

  pl_df = hdf %>%
    filter(hr_pa3>=0.02, pa3>=950) %>%
    merge(pl_lkup, by.x ="playerID", by.y="bbrefID")

  lab_df = pl_df %>% filter(grepl('^greenha01', playerID))

  pl_df %>%
    ggplot(aes(x=hr3, y=hr2)) +
    geom_point() +
    geom_point(data=lab_df, color="steelblue") +
    geom_smooth() +
    theme_minimal(base_size = 16) +
    labs(x="HR (age 34-36)", y="HR (age 30-33)") +
    geom_text_repel(data=lab_df, aes(label=nameAbbv))
}
