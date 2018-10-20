
library(dplyr)
library(ggplot2)

pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')

br_war = readr::read_csv("~/Downloads/war_daily_pitch.txt", guess_max = 1000000) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(ERA_plus = as.numeric(ERA_plus)) %>%
  mutate(ERA_plus = ifelse(is.na(ERA_plus), 100, ERA_plus)) %>%
  mutate(w = IPouts, w = ifelse(is.na(w), 1e-6, w)) %>%
  mutate(ERA_plus = as.numeric(ERA_plus)) %>%
  group_by(player_ID, year_ID) %>%
  mutate(ERA_plus = sum(w * ERA_plus) / sum(w)) %>% filter(stint_ID == 1) %>%
  ungroup()

test_pattern = function(dfX, pid,
                        metric_name, metric_val1, metric_val2, metric_val3,
                        metric_count1, metric_count3,
                        qual_name, qual_val) {

  message(pid)
  df1 = dfX[dfX$player_ID == pid,] %>% as.data.frame()
  if (nrow(df1) <= 6) {
    return(NULL)
  }
  message(nrow(df1))
  ss = sapply(1:nrow(df1), function(i) {
    r = df1[i,]
    if (r[1,qual_name] < qual_val) {
      'x'
    } else if (r[1,metric_name] >= metric_val1) {
      'a'
    } else if (r[1,metric_name] < metric_val2) {
      'b'
    } else {
      'x'
    }
  })

  df1$test_vec = ss
  ss = paste(ss, sep="", collapse = "")
  #df1 = df1 %>% select(player_ID, year_ID, age, IPouts, ERA_plus, test_vec)

  test_val = grepl(sprintf("(a){%s,}b{1,}(a){%d,}", metric_count1, metric_count3), ss)
  df1$test_val = test_val
  df1
}


test_pattern2 = function(dfX, pid,
                         metric_name,
                         metric_val1, metric_val2, metric_val3,
                         metric_count1, metric_count3,
                         qual_name, qual_val) {

  message(pid)
  cc = which(dfX[,qual_name] >= qual_val)
  if (length(cc) == 0) {
    return(NULL)
  }
  dfX = dfX[cc,]
  df1 = dfX[dfX$player_ID == pid,] %>% as.data.frame()
  df1 = df1[]
  if (nrow(df1) <= 6) {
    return(NULL)
  }

  res = NULL
  message(nrow(df1))
  for (i in 2:(nrow(df1)-1)) {
    r = df1[i,]
    if (r[1,qual_name] < qual_val) {
      next
    }
    if (r[metric_name] < metric_val2) {
      lhs = sum(df1[1:(i-1),metric_name] >= metric_val1, na.rm=T)
      rhs = sum(df1[(i+1):nrow(df1),metric_name] >= metric_val3, na.rm=T)
      if (lhs >= metric_count1 && rhs >= metric_count3) return(df1)
    }
  }
  return(NULL)

}


test_patternX = function(dfX, pid) {
  message(pid)
  df1 = dfX[dfX$player_ID == pid,]
  if (nrow(df1) <= 6) {
    return(NULL)
  }
  message(nrow(df1))
  df1 = df1 %>% filter(IPouts >= 125 * 3)
  df1 = df1 %>% select(player_ID, year_ID, age, IPouts, ERA_plus)
  df1 = df1 %>% mutate(z = ERA_plus > 100) %>% arrange(year_ID)
  df1 %$% z %>% rle -> rr

  if (length(rr$lengths) < 3) {
      return(NULL)
  }

  n1 = 1
  n2 = length(rr$lengths) - 2
  for (i in n1:n2) {
    test_val = TRUE
    test_val = test_val && rr$lengths[i+0] >= 5 && rr$values[i+0] == TRUE
    test_val = test_val && rr$lengths[i+1] >= 1 && rr$values[i+1] == FALSE
    test_val = test_val && rr$lengths[i+2] >= 3 && rr$values[i+2] == TRUE
    if (test_val) {
      return(df1)
    }
  }

  return(NULL)
}

do_plot = function() {
  rr %>% group_by(player_ID) %>% mutate(year_ID = year_ID - min(year_ID), i=ERA_plus >= 140, ii=sum(i)) %>% filter(ii >= 2) %>% ungroup %>% merge(pl_lkup, by.x="player_ID", by.y="bbrefID") -> plot_df

  plot_df %>% ggplot(aes(x=year_ID, y=ERA_plus)) + geom_line() + facet_wrap(~nameAbbv) + coord_cartesian(ylim=c(200, 50)) + geom_hline(yintercept = 100, color='steelblue') + geom_point(size=0.5)
}

# ll = lapply(pids_br, function(pid) test_pattern2(br_war, pid, "ERA_plus", 150, 95, 135, 2, 2, "IPouts", 375))
