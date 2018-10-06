
library(dplyr)
library(ggplot2)
library(DBI)
library(RPostgres)

pl_lkup = Lahman::Master %>% dplyr::select(playerID, retroID, bbrefID, nameFirst, nameLast)
pl_lkup$nameAbbv = paste(stringr::str_sub(pl_lkup$nameFirst, 1, 1), pl_lkup$nameLast, sep='.')


get_data = function() {
  b = dbGetQuery(conn,
                 paste0("select year_id, bat_id, event_cd, base1_run_id, ",
                        "base2_run_id, base3_run_id, outs_ct, ",
                        "event_outs_ct, fld_cd, dp_fl, event_tx from event ",
                        "where year_id>=1955"))

  aa = b %>%
    mutate(i1 = !is.na(base1_run_id),
           i2 = !is.na(base2_run_id),
           i3 = !is.na(base3_run_id),
           i1=as.integer(i1),
           i2=as.integer(i2),
           i3=as.integer(i3)) %>%
    filter(event_cd %in% c(2,3,14,16,18,19, 20,21,22,23))
}

process_data = function(dfX) {
  a = dfX %>%
    mutate(dp_opp = (i1 == 1) & (outs_ct <=1),
           is_dp = (event_outs_ct>=2) & (fld_cd %in% c(4,5,6)),
           is_dp_br = grepl('GDP', event_tx), babip=event_cd %in% c(2, 18:22))

  a = a %>%
    mutate(i_reg_pa=dp_opp == FALSE) %>%
    group_by(bat_id, year_id) %>%
    summarise(reg_pa=sum(i_reg_pa),
              dp_pa=sum(!i_reg_pa),
              is_dp=sum(is_dp),
              is_dp_br = sum(is_dp_br),
              is_babip = sum(babip),
              is_dp_babip = sum(babip * !i_reg_pa),
              all_pa = reg_pa+dp_pa) %>%
    ungroup()

  a = a %>% merge(pl_lkup, by.x="bat_id", by.y="retroID")

  a %>% select(-is_dp) %>% rename(is_dp=is_dp_br)
}


sum_data = function(a,pa_cutoff=3500) {
  sum_df = a %>%
    filter(all_pa >= pa_cutoff) %>%
    mutate(dp_pa_frac=dp_pa/all_pa, dp_frac=is_dp/dp_pa, dp_babip_frac=is_dp/is_dp_babip)

}
dp_hist1 = function(a) {

  a2 %>%
    mutate(z=dp_pa/all_pa) %>%
    filter(dp_pa>=500) %>%
    ggplot(aes(x=dp_frac)) + geom_histogram() + theme_minimal()


}
