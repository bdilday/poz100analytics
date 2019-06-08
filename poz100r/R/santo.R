
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggrepel)

pl_lkup = Lahman::Master %>%
  dplyr::select(bbrefID, nameLast, nameFirst, birthYear) %>%
  rename(playerID=bbrefID)

nameAbbv = paste(
  stringr::str_sub(pl_lkup$nameFirst, 1, 1),
  pl_lkup$nameLast, sep=". ")

pl_lkup$nameAbbv = nameAbbv

harmonic_mean = function(war_df) {

  best_hm = war_df %>%
    filter(WAR>0.1, WAR_off > 0.1, WAR_def > 0.1) %>%
    mutate(z=2/(1/WAR_off+1/WAR_def)) %>%
    # group_by(playerID) %>%
    # filter(z==max(z)) %>%
    # ungroup() %>%
    filter(z>=3.75) %>%
    merge(pl_lkup, by="playerID") %>%
    mutate(k = sprintf("%s (%d)", nameLast, yearID))

  p1 = war_df %>%
    filter(WAR>0.1, WAR_off > 0.1, WAR_def > 0.1) %>%
    mutate(z=2/(1/WAR_off+1/WAR_def)) %>%
    ggplot(aes(x=WAR_off, y=WAR_def)) +
    geom_point(alpha=0.3) + facet_wrap(~POS) +
    geom_point(data=best_hm, size=2, color='steelblue') +
    geom_text_repel(data=best_hm, aes(label=k))

}

graph_best_off_def_combines = function(b3) {

  p = b3 %>%
    ggplot(aes(x=d, y=weighted_war, group=playerID,color=nameFull)) +
    geom_line(size=1.5) +
    labs(x="offense / defense split", y="weight war")

}

graph_best_off_def = function(b3) {

  p = b3 %>%
    ggplot(aes(x=d, y=wrank, group=playerID)) +
    geom_line() +
    facet_wrap(~k) + ylim(21, 0) +
    labs(x="offense / defense split", y="player-position all-time rank")

}

best_by_off_def_tradeoff =
  function(war_df, ranker, brank=10, exp_tau=5, delta_split=1) {

    # use wrank for by position, r for all time
    qr = enquo(ranker)
    a3 = analyze3(war_df,
                  wlim = 1000,
                  exp_tau = exp_tau,
                  exp_split = delta_split)
    b3 = a3 %>%
      group_by(playerID) %>%
      mutate(best_rank=min(!!qr)) %>%
      ungroup() %>% filter(best_rank <= brank)

    b3 = b3 %>% mutate(k=sprintf("%s (%s)", nameFull, POS))
    lev_ord = b3 %>%
      group_by(POS, playerID, k) %>%
      summarise() %>%
      arrange(POS, playerID) %$% k %>% unlist()

    b3$k = factor(b3$k, levels = lev_ord)

    b3

  }


analyze3 = function(war_df,
                    wlim=20,
                    exp_tau=5,
                    exp_split=0.02,
                    plid='smithoz01',
                    max_year=21) {

  dseq = seq(1, 20, exp_split)
  ll = lapply(dseq, function(d) {
    a = top_lists(war_df %>% filter(yearID>=1901), max_year = max_year,
                  baseline_value = 0,
                  off_def_split = 0.5, exp_tau = d)

    w = a %>% filter(grepl(plid, playerID)) %$% wrank

    #    list(w=w, )
    a = a %>% head(wlim)
    a$w = w
    a$d = d
    a = a %>% mutate(r = rank(-weighted_war, ties.method = "first"))
    a
  })

  dplyr::bind_rows(ll)
}

analyze2 = function(war_df, dw=0) {

  nplayer_above = function(nyear) {
    a = transform_data(war_df, max_year = nyear,
                       baseline_value = 0,
                       off_def_split = 0.5,
                       exp_tau = 100000, player_limit = 1000,
                       plid='^utle')

    t1 = a %>% filter(grepl(plid, playerID))

    check_above = sapply(1:nrow(a), function(i) {
      r = a[i,]
      check_above_row = sapply(1:nyear, function(j) {
        k = sprintf("WAR_%02d", j)
        test = r[,k] - t1[,k] >= 0
      })
      Reduce(`&&`, check_above_row, T)
    })

  }

  lapply(1:12, function(y) {
    r = nplayer_above(y) %>% sum
    list(nplayer=r, y=y)
  }) %>% bind_rows()

}

analyze1 = function(war_df, dw = 1) {
  war_rank_df = war_df %>%
    group_by(playerID) %>%
    mutate(wrank = rank(-WAR, ties.method = "first"))

  a = transform_data(war_df, max_year = 5,
                     baseline_value = 0,
                     off_def_split = 0.5,
                     exp_tau = 100000, player_limit = 1000)
  xx = as.matrix(a[,4:8])
  t1 = a %>% filter(grepl('^utle', playerID))
  ss = sapply(1:nrow(xx), function(i) { sqrt(mean((t1[,4:8]-xx[i,])**2)) })
  b = a
  b$ss = ss
  t1 = t1[,4:8]

  b %>%
    arrange(ss) %>%
    filter(WAR_01+dw >= t1[,1],
           WAR_02+dw >= t1[,2],
           WAR_03+dw >= t1[,3],
           WAR_04+dw >= t1[,4],
           WAR_05+dw >= t1[,5])


}

get_war_df = function() {
  war_df = readr::read_csv("https://gist.githubusercontent.com/bdilday/28621eb7b91f42d7b90d56475f098cf3/raw/0216fb7fe917695db4b5e90070d90dd3116a6dcf/war_data.csv", guess_max = 1000000)
}

top_lists = function(war_df,
                     baseline_value = 0,
                     exp_tau = 100, off_def_split = 0.5, max_year = 21) {

  exp_tau = as.numeric(exp_tau)
  baseline_value = as.numeric(baseline_value)
  pl_lkup = Lahman::Master %>%
    select(playerID, bbrefID, nameFirst, nameLast)

  pos_df = war_df %>%
    group_by(playerID, POS) %>%
    summarise() %>%
    ungroup()

  off_v = off_def_split
  def_v = (1 - off_v)

  wrk = war_df %>%
    mutate(w = off_v * WAR_off + def_v * WAR_def, w = w * 2)

  wrk = wrk %>%
    mutate(w = ifelse(w-baseline_value > 0, w-baseline_value, 0))
  wrk = wrk %>% dplyr::select(-POS) %>%
    group_by(playerID) %>%
    arrange(-w) %>%
    mutate(war_rank = row_number())

  pl_ids = unique(war_df$playerID)
  fillin_df =
    data.frame(playerID=rep(pl_ids, each=max_year),
               war_rank=rep(1:max_year, length(pl_ids)),
               stringsAsFactors = FALSE)

  wrk = wrk %>%
    dplyr::right_join(fillin_df, by=c("playerID", "war_rank")) %>%
    dplyr::left_join(pos_df, by="playerID")

  cc = which(is.na(wrk$w))
  if (length(cc) > 0) {
    wrk[cc,]$w = 0
  }

  war_weights_df = data.frame(war_rank = 1:max_year)
  war_weights_df$weight = exp(-(war_weights_df$war_rank-1)/exp_tau)

  wrk %>%
    inner_join(war_weights_df, by="war_rank") %>%
    group_by(playerID, POS) %>%
    summarise(z=sum(w * weight)/sum(weight)) %>%
    group_by(POS) %>%
    arrange(-z) %>%
    mutate(wrank = row_number()) %>%
    rename(weighted_war=z) %>%
    ungroup() %>%
    left_join(pl_lkup, by="playerID") %>%
    mutate(nameFull =
             paste(stringr::str_sub(nameFirst, 1, 1), nameLast)) %>%
    dplyr::select(-bbrefID, -nameFirst, -nameLast) %>%
    dplyr::select(wrank, POS, weighted_war, nameFull, playerID)
}


transform_data = function(war_df,
                          baseline_value = 0,
                          exp_tau = 100,
                          off_def_split = 0.5,
                          max_year = 21,
                          pca=TRUE,
                          pca_scale=TRUE,
                          theta=0,
                          check_duplicates=FALSE,
                          max_iter=1000,
                          player_limit = 300)
{

  pl_lkup = Lahman::Master %>%
    select(playerID, bbrefID, nameFirst, nameLast)

  hofers = Lahman::HallOfFame %>%
    filter(inducted=="Y",
           category=="Player",
           votedBy %in% c("BBWAA", "Special Election")) %>%
    arrange(desc(votedBy)) %>%
    group_by(playerID) %>%
    summarise() %>%
    ungroup() %>%
    mutate(hof=TRUE)

  pos_df = war_df %>%
    group_by(playerID, POS) %>%
    summarise() %>%
    ungroup()

  off_v = off_def_split
  def_v = (1 - off_v)

  wrk = war_df %>%
    mutate(w = off_v * WAR_off + def_v * WAR_def, w = w * 2)

  wrk = wrk %>% dplyr::select(-POS) %>%
    group_by(playerID) %>%
    arrange(-w) %>%
    mutate(war_rank = row_number())

  pl_ids = unique(war_df$playerID)
  fillin_df =
    data.frame(playerID=rep(pl_ids, each=max_year),
               war_rank=rep(1:max_year, length(pl_ids)),
               stringsAsFactors = FALSE)

  wrk = wrk %>%
    dplyr::right_join(fillin_df, by=c("playerID", "war_rank")) %>%
    dplyr::left_join(pos_df, by="playerID")

  cc = which(is.na(wrk$w))
  if (length(cc) > 0) {
    wrk[cc,]$w = 0
  }

  war_weights_df = data.frame(war_rank = 1:max_year)
  war_weights_df$weight = exp(-(war_weights_df$war_rank-1)/exp_tau)

  wrk = wrk %>%
    inner_join(war_weights_df, by="war_rank") %>%
    group_by(playerID) %>%
    mutate(z=(w * weight)) %>%
    arrange(-z) %>%
    mutate(z_rank = row_number()) %>%
    ungroup() %>%
    left_join(pl_lkup, by="playerID") %>%
    select(playerID, POS, war_rank, w) %>%
    mutate(war_rank=sprintf("WAR_%02d", war_rank)) %>%
    left_join(hofers, by="playerID") %>%
    mutate(hof = ifelse(is.na(hof), FALSE, hof))


  filter_df = wrk %>%
    group_by(playerID) %>%
    summarise(sz = sum(w)) %>%
    arrange(-sz) %>%
    head(player_limit)

  wrk %>% merge(filter_df, by="playerID") %>%
    dplyr::select(-sz) %>%
    spread(war_rank, w)
}

make_3b_plot = function(war_df, bo=NULL) {
  if (is.null(bo)) {
    bo = best_by_off_def_tradeoff(war_df, wrank)
  }

  pids = bo %>%
    filter(POS == "3B",wrank <= 5) %>%
    count(playerID) %$% playerID

  plot_df = bo %>%
    filter(playerID %in% pids)

  ordered_names = plot_df %>% filter(d==max(d)) %>% arrange(wrank) %$% nameFull
  plot_df$nameFull = factor(plot_df$nameFull, levels = ordered_names)
  p_rank = plot_df %>%
    ggplot(aes(x=d, y=wrank, color=nameFull)) +
    geom_line() + geom_point() +
    scale_y_reverse() + theme_minimal(base_size = 18) +
    scale_color_brewer(palette=2, type = "qual") +
    labs(x="peak vs. career decay-scale",
         y="positional rank",
         color="",
         title="Top third-basemen")

  p_war = plot_df %>%
    ggplot(aes(x=d, y=weighted_war, color=nameFull)) +
    geom_line() + geom_point() + theme_minimal(base_size = 18) +
    scale_color_brewer(palette=2, type = "qual") +
    labs(x="peak vs. career decay-scale",
         y="weighted WAR",
         color="",
         title="Top third-basemen")



  list(p_rank, p_war)
}

plot_war_3vs12 = function(war_df) {
  top10 = war_df %>%
    group_by(playerID) %>%
    mutate(r=rank(-WAR, ties.method = "first")) %>%
    mutate(i1=as.integer(r<=3), i2=as.integer(r>3 & r<=15)) %>%
    filter(i1+i2>=1) %>%
    mutate(WAR1=WAR*i1, WAR2=WAR*i2) %>%
    summarise(WAR=sum(WAR),
              WAR1=sum(WAR1)/3,
              WAR2=sum(WAR2) / sum(i2)) %>%
    mutate(z=(WAR2)/WAR1) %>% filter(WAR>0, WAR1>0, WAR2>0) %>%
    arrange(z) %>%
    filter(WAR1 >= 8) %>%
    head(10) %$% playerID

  other10 = war_df %>%
    group_by(playerID) %>%
    mutate(r=rank(-WAR, ties.method = "first")) %>%
    mutate(i1=as.integer(r<=3), i2=as.integer(r>3 & r<=15)) %>%
    filter(i1+i2>=1) %>%
    mutate(WAR1=WAR*i1, WAR2=WAR*i2) %>%
    summarise(WAR=sum(WAR),
              WAR1=sum(WAR1)/3,
              WAR2=sum(WAR2) / sum(i2)) %>%
    mutate(z=(WAR2)/WAR1) %>% filter(WAR>0, WAR1>0, WAR2>0) %>%
    arrange(-z) %>%
    filter(WAR1 >= 8) %>%
    head(10) %$% playerID

  plot_df = war_df %>%
    group_by(playerID) %>%
    mutate(r=rank(-WAR)) %>%
    mutate(i1=as.integer(r<=3), i2=as.integer(r>3 & r<=15)) %>%
    mutate(WAR1=WAR*i1, WAR2=WAR*i2) %>%
    summarise(WAR=sum(WAR), WAR1=sum(WAR1)/sum(i1), WAR2=sum(WAR2)/sum(i2)) %>%
    ungroup() %>% merge(pl_lkup, by = "playerID")

  p = plot_df %>% ggplot(aes(x=WAR1,y=WAR2)) +
    geom_point() +
    geom_text_repel(data=plot_df %>% filter(playerID %in% top10),
                    aes(label=nameAbbv), color="red4") +
    geom_text_repel(data=plot_df %>% filter(playerID %in% other10),
                    aes(label=nameAbbv), color="steelblue")

  p + theme_minimal(base_size = 18) + labs(x="Avg. WAR (top 3 seasons)", y="Avg. WAR (next 12 seasons)")

}
