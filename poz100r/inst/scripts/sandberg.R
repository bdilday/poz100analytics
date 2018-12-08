
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(zoo)
library(stringr)

pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')

the_metrics = c("X2B", "X3B", "HR", "SB", "CS")
the_other_metrics = c("X1B", "BB")
all_the_metrics = c("X1B", "X2B", "X3B", "HR", "BB", "SB", "CS")

get_name_df = function(b) {
  pid1 = outer(b[,"player_season_key"],
               b[,"player_season_key"],
               FUN= function(x1, x2) x1) %>% as.vector()
  pid2 = outer(b[,"player_season_key"],
               b[,"player_season_key"],
               FUN= function(x1, x2) x2) %>% as.vector()

  df = dplyr::data_frame(player_season1 = pid1,
                  player_season2 = pid2
                  )

}

generate_comp_df = function(b, stat_names) {

  df = get_name_df(b)

  tmp = lapply(stat_names, function(stat_name) {
    message(stat_name)
    mm = outer(
      b[,stat_name], b[,stat_name], FUN=dist_metric_) %>% as.vector()
    df = data.frame(v = mm)
    names(df) = c(stat_name)
    df
  }) %>% dplyr::bind_cols()

  dmat = tmp %>% '^'(2) %>% rowSums() %>% sqrt()

  tmp$dmat = dmat
  df %>% cbind(tmp) %>% as_tibble() %>% filter(player_season2 != player_season1)
}

lapply_weighted_dist = function(b, stat_names) {

  wd1 = function(i) {
    matrix(t(player_data)) - as.vector(as.numeric(player_data[i,])) -> ds
    matrix(ds, ncol = ncol(player_data), byrow = T) -> ds1
    list(wd = sqrt(rowSums((ds1 %*% mm)**2)))
  }

  cov_mat_list = derive_cov_mat(b, stat_names)
  player_data = cov_mat_list$player_dat
  mm = solve(chol(cov_mat_list$cov_mat))

  df = get_name_df(b)
  wd = lapply(1:nrow(player_data), wd1) %>% dplyr::bind_rows()

  list(df, wd) %>% dplyr::bind_cols()
  # %>% filter(player_season1 < player_season2)

}



weighted_dist = function(i1, i2) {
  dr = matrix(as.numeric((player_data[i2,] - player_data[i1,])), nrow=1)
  sum((dr %*% mm)**2)
}

derive_cov_mat = function(b, stat_names) {
  player_data = lapply(stat_names, function(stat_name) {
    tmp = data.frame(b[,stat_name] / b[,"PA"])
    names(tmp) = paste(stat_name, "pa", sep="_")
    tmp
  }) %>% dplyr::bind_cols()

  cov_mat = cov(player_data)
  mm = solve(chol(cov_mat))

  list(player_data = player_data, cov_mat = cov_mat)

}

dist_metric_ = function(x1, x2) {
  (x1-x2)/max(1, sqrt(x1+x2))
}

dist_metric = function(row1, row2, stat_name) {
  dv = row1[,stat_name] - row2[,stat_name]
  ds = sqrt(row1[,stat_name] + row2[,stat_name])
  dv / max(1, ds)
}

outer_stat_comp = function(df, stat_name) {
  mm = outer(b[,"HR"], b[,"HR"], FUN=dist_metric_)
  upt = mm[upper.tri(mm)]
}


player_dist = function(row1, row2, metrics) {
  a = sapply(metrics, function(m) {
    v = dist_metric(row1, row2, m)
  })

  v = sqrt(sum(a**2))

  a = as.data.frame(t(a)) %>%
    cbind(
      data.frame(
        pid1=row1$playerID,
        year1=row1$yearID,
        pid2=row2$playerID,
        year2=row2$yearID,
        tot=v)
    )
}

compute_season_distances = function(b) {
  lapply(1:nrow(b), function(i1) {
    message(i1, " ", nrow(b))
    lapply((i1+1):nrow(b), function(i2) {
      a = player_dist(b[i1,], b[i2,], the_metrics)
    }) %>% dplyr::bind_rows()
  }) %>% dplyr::bind_rows()

}

distance_result_to_table = function(df1, do_summary=FALSE) {

  strip_pid = function(pid_key) {
    str_split(pid_key, "_")[[1]][1]
  }

  strip_yr = function(pid_key) {
    str_split(pid_key, "_")[[1]][2]
  }

  pid1=sapply(1:nrow(df1), function(i) {
    tmp = data.frame(p=strip_pid(df1[i, "player_season1"]), stringsAsFactors = F)
    tmp %>% merge(pl_lkup, by.x="p", by.y="playerID") %>% select(nameAbbv) %>% unlist()
  })

  pid2=sapply(1:nrow(df1), function(i) {
    tmp = data.frame(p=strip_pid(df1[i, "player_season2"]), stringsAsFactors = F)
    tmp %>% merge(pl_lkup, by.x="p", by.y="playerID") %>% select(nameAbbv) %>% unlist()
  })

  yr1=sapply(1:nrow(df1), function(i) {
    strip_yr(df1[i, "player_season1"])
  })

  yr2=sapply(1:nrow(df1), function(i) {
    strip_yr(df1[i, "player_season2"])
  })

  df_tab =
    data.frame(pid1=pid1, yr1=yr1,
               pid2=pid2, yr2=yr2, stringsAsFactors = F) %>%
    cbind.data.frame(df1[,3:ncol(df1)])

  if (do_summary) {
    df_tab = df_tab %>%
      mutate(dmat = sprintf("%.2f", dmat)) %>%
      select(name=pid2, season=yr2, distance = dmat)
  }

  df_tab
}

inspect_result = function(res) {
  r1 = b %>% filter(playerID==res$pid1, yearID==res$year1)
  r2 = b %>% filter(playerID==res$pid2, yearID==res$year2)
  rbind(r1, r2)
}

get_lahman_data = function(year_lim=1973, pa_lim=350) {
  b = Lahman::battingStats() %>%
    filter(yearID >= year_lim) %>% filter(!is.na(PA),
                                          PA >= pa_lim, HR>=10, X2B>=10)
  b$X1B = b$H - b$X2B - b$X3B - b$HR
  b$player_season_key = paste(b$playerID, b$yearID, sep="_")
  b$CS = ifelse(is.na(b$CS), 0, b$CS)
  b
}
