
library(dplyr)
library(RPostgres)
library(DBI)
library(lme4)
library(stringr)
library(ggplot2)
library(htmlTable)
library(magrittr)


pl_lkup = Lahman::Master %>%
  select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv =
  paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')


load_woba_weights = function() {
  fg_guts = readRDS("./inst/fg_guts.rds")

  woba_yr = function(yr) {
    ev_cds = c(2, 3, 14, 15, 16, 20, 21, 22, 23)
    lahman_cds = c("nil", "-", "BB", "-", "-", "X1B", "X2B", "X3B", "HR")
    ev_weights = fg_guts %>%
      filter(Season == yr) %$% c(0, 0, wBB, wBB, wBB, w1B, w2B, w3B, wHR)
    df = data.frame(year_id=yr,
                    event_cd = ev_cds,
                    lahman_cd = lahman_cds,
                    woba_pts=ev_weights)
  }

  dplyr::bind_rows(lapply(1871:2018, woba_yr))

}

process_retro_data = function(dfX) {

  dfX = dfX %>% filter(bat_fld_cd != 1)
  dfX$park_key = with(dfX, paste(home_team_id, year_id, sep="_"))
  dfX$pit_key = with(dfX, paste(pit_id, year_id, sep="_"))
  dfX$bat_key = with(dfX, paste(bat_id, year_id, sep="_"))

  woba_weights = load_woba_weights()
  dfX = dfX %>% dplyr::inner_join(woba_weights, by=c("year_id", "event_cd"))
  dfX$year_id %<>% as.factor
  dfX$bat_home_id %<>% as.factor
  dfX
}


parse_ranefs = function(lmer_mod) {
  rfs = names(ranef(lmer_mod))
  ll = lapply(rfs, function(rf) {
    ranef_to_df(lmer_mod, rf)
  }) %>% dplyr::bind_rows()
}

ranef_to_df = function(lmer_mod, ranef_nm) {
  rr = ranef(lmer_mod)
  data.frame(k=rownames(rr[ranef_nm][[1]]),
             value=rr[ranef_nm][[1]][,1],
             ranef_nm = ranef_nm,
             stringsAsFactors = FALSE)

}



top_seasons_rate = function(topn=20, do_desc=FALSE, ranef_type=NULL, res_df=NULL) {
  if (is.null(res_df)) {
    res_df = sb_contributions(model_frame_df, rr_df, ranef_type)
  }
  m = 1
  if (do_desc) {
    m = -1
  }
  res_df %>%
    merge(pl_lkup, by.x="nm", by.y="retroID") %>%
    mutate(p_n = m  * p_n) %>%
    arrange(p_n) %>%
    head(topn) %>%
    select(nameAbbv, year, p_n) %>%
    mutate(p_n = m * p_n) %>%
    rename(delta_steal_rate=p_n)
}

top_seasons = function(topn=20, do_desc=FALSE, ranef_type=NULL, res_df=NULL) {
  if (is.null(res_df)) {
    res_df = sb_contributions(model_frame_df, rr_df, ranef_type)
  }
  m = 1
  if (do_desc) {
    m = -1
  }
  res_df %>%
    merge(pl_lkup, by.x="nm", by.y="retroID") %>%
    mutate(z = m  * z) %>%
    arrange(z) %>%
    head(topn) %>%
    select(nameAbbv, year, z) %>%
    mutate(z = m * z) %>%

    rename(delta_steals=z)
}


get_data = function(conn = NULL, min_year=1974, max_year=1992){

  if (is.null(conn)) {
    conn <- dbConnect(RPostgres::Postgres(),
                      password=Sys.getenv("PSQL_PASS"),
                      user=Sys.getenv("PSQL_USER"),
                      port=Sys.getenv("PSQL_PORT"),
                      dbname='retrosheet')
  }

  df = dbGetQuery(conn,
                  sprintf("select * from event where year_id>=%d and year_id<=%d",
                          min_year, max_year)
  )


}

get_model_frame_df = function(glmer_mod) {
  model_frame_df = glmer_mod@frame
  model_frame_df$p = predict(glmer_mod)
  model_frame_df
}

get_mod_df = function(sb_df) {
  mod_df = sb_df %>%
    filter(abs(home_score_ct-away_score_ct) <= 4, inn_ct < 9)
}

model_data = function(mod_df) {
  glmer_mod = glmer(run1_sb_fl ~
                      1 + (1|pit_key) + (1|run1_key) + (1|cat_key),
                    data=mod_df,
                    family = binomial(),
                    nAGQ = 0
  )
}

name_from_k = function(k) {
  str_split(k, "_")[[1]][1]
}

year_from_k = function(k) {
  str_split(k, "_")[[1]][2]
}

sb_contributions= function(model_frame_df, rr_df, ranef_nm_) {
  res_df = rr_df %>%
    filter(ranef_nm == ranef_nm_) %>%
    left_join(model_frame_df, by=c("k" = ranef_nm_)) %>%
    mutate(pw = exp(p)/(1+exp(p)), pwo = exp(p-value)/(1+exp(p-value))) %>%
    mutate(z = pw-pwo) %>%
    group_by(k) %>%
    summarise(pw=sum(pw), pwo=sum(pwo), z=sum(z), n=n(), p_n=z/n)

  res_df$nm = res_df$nm = sapply(res_df$k, name_from_k)
  res_df$year = sapply(res_df$k, year_from_k)
  res_df
}

parse_ranefs = function(lmer_mod, rr=NULL) {
  if (is.null(rr)) {
    rr = ranef(lmer_mod)
  }
  rfs = names(rr)
  ll = lapply(rfs, function(rf) {
    ranef_to_df(lmer_mod, rf, rr)
  }) %>% dplyr::bind_rows()
}


ranef_to_df = function(lmer_mod, ranef_nm, rr=NULL) {
  if (is.null(rr)) {
    rr = ranef(lmer_mod)
  }
  data.frame(k=rownames(rr[ranef_nm][[1]]),
             value=rr[ranef_nm][[1]][,1],
             ranef_nm = ranef_nm,
             stringsAsFactors = FALSE)

}



do_lmer_mod = function(dfX) {
  lmer_mod = lmer(
    woba_pts ~
      1 +
      (1|pit_key) +
      (1 + home_team_id | bat_key) +
      year_id, data=dfX)
  saveRDS(lmer_mod, "lmer_mod_X_walker.rds")
  rr = ranef(lmer_mod)
  saveRDS(rr, "lmer_mod_ranef_X_walker.rds")
}
