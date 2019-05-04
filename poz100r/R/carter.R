
library(dplyr)
library(RPostgres)
library(DBI)
library(lme4)
library(stringr)
library(ggplot2)
library(htmlTable)


pl_lkup = Lahman::Master %>%
  select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv =
  paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')


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

  df = df %>%
    select(game_id, year_id, bat_id, pit_id,
           pos2_fld_id,
           inn_ct,
           home_team_id,
           away_score_ct, home_score_ct,
           bat_home_id,
           base1_run_id, base2_run_id, base3_run_id,
           run1_sb_fl, run2_sb_fl, run3_sb_fl,
           run1_cs_fl, run2_cs_fl, run3_cs_fl,
           event_cd) %>%
    mutate(
      run1_key=paste(base1_run_id, year_id, sep = '_'),
      pit_key=paste(pit_id, year_id, sep ='_'),
      cat_key=paste(pos2_fld_id, year_id, sep ='_'),
      home_key=paste(home_team_id, year_id, sep='_')) %>%
    filter(event_cd %in% c(2,3,4,6,14,15,16,18:23))

  sb_df = df %>%
    filter(!is.na(base1_run_id), is.na(base2_run_id), is.na(base3_run_id))
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


catcher_rankings = function(lmer_mod, df) {
  pl_lkup = Lahman::Master %>% mutate(nameFull = paste(nameFirst, nameLast)) %>%
    dplyr::select(retroID, nameFull)


  rf = parse_ranefs(lmer_mod)
  aa = rf %>%
    filter(ranef_nm == "def_catcher") %>%
    merge(df, by.x="k", by.y="def_catcher")

  aa$catcher_id = sapply(str_split(aa$k, "_"), function(s) {s[[1]]})
  aa$season = sapply(str_split(aa$k, "_"), function(s) {s[[2]]})
  career = aa %>%
    group_by(catcher_id) %>%
    summarise(m=mean(value), s=sum(value)) %>% arrange(m) %>%
    mutate(r = row_number()) %>% ungroup() %>%
    merge(pl_lkup, by.x="catcher_id", by.y="retroID")

  yearly = aa %>%
    group_by(catcher_id, season) %>%
    summarise(m=mean(value), s=sum(value)) %>% arrange(m) %>%
    ungroup() %>%
    mutate(r = row_number()) %>% ungroup() %>%
    merge(pl_lkup, by.x="catcher_id", by.y="retroID")

  list(yearly = yearly, career = career)
}
