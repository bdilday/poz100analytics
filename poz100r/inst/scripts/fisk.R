
library(dplyr)
library(RPostgres)
library(DBI)

get_data = function(conn = NULL, dfX = NULL){

  if (is.null(conn)) {
    conn <- dbConnect(RPostgres::Postgres(),
                      password=Sys.getenv("PSQL_PASS"),
                      user=Sys.getenv("PSQL_USER"),
                      port=Sys.getenv("PSQL_PORT"),
                      dbname='retrosheet')
  }

  if (is.null(dfX)) {
    dfX = dbGetQuery(conn,
                     paste0("select * from event where ",
                            "( (inn_ct=1 and inn_new_fl='T') or ",
                            "(inn_ct=5 and inn_end_fl='T')) and ",
                            "year_id>=1961")
    )
  }

  df = dfX %>% dplyr::select(game_id:bat_home_id,
                            away_score_ct:bat_id,
                            pit_id, pos2_fld_id,
                            inn_ct, event_runs_ct,
                            home_team_id:fld_team_id, year_id)

  df1 = df %>% filter(inn_ct==1) %>%
    dplyr::select(game_id, year_id, bat_home_id, pit_id,
                  pos2_fld_id,
                  away_team_id, home_team_id)
  df5 = df %>% filter(inn_ct==5) %>%
    dplyr::select(-pos2_fld_id, -pit_id,
                  -away_team_id, -home_team_id, -year_id)
  df15 = dplyr::inner_join(df1, df5, by=c("game_id", "bat_home_id"))
  away_bat = df15 %>%
    filter(bat_home_id == 0) %>%
    mutate(off_score = away_score_ct + event_runs_ct,
           def_pitcher = paste(pit_id, year_id, sep="_"),
           def_catcher = paste(pos2_fld_id, year_id, sep="_"),
           off_team=paste(away_team_id, year_id, sep="_"),
           def_team=paste(home_team_id, year_id, sep="_"),
           park_id=paste(home_team_id, year_id, sep="_"),
           park = home_team_id
    )
  home_bat = df15 %>%
    filter(bat_home_id == 1) %>%
    mutate(off_score = home_score_ct + event_runs_ct,
           def_pitcher = paste(pit_id, year_id, sep="_"),
           def_catcher = paste(pos2_fld_id, year_id, sep="_"),
           off_team=paste(home_team_id, year_id, sep="_"),
           def_team=paste(away_team_id, year_id, sep="_"),
           park_id=paste(home_team_id, year_id, sep="_"),
           park = home_team_id
)
  rbind.data.frame(away_bat, home_bat) %>%
    dplyr::select(game_id, year_id, bat_home_id,
                  off_score, def_pitcher, def_catcher,
                  off_team, def_team, park_id, park)


}

library(lme4)
model_data = function(df) {
  lmer_mod = lmer(off_score ~
                    1 +
                    off_team +
                    (1|def_pitcher) +
                    (1|def_catcher),
                  data=df, family='gaussian')


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

pl_lkup = Lahman::Master %>% mutate(nameFull = paste(nameFirst, nameLast)) %>%
  dplyr::select(retroID, nameFull)

library(stringr)
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
