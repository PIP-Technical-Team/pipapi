lkups <- pipapi::create_versioned_lkups(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))
lkups <- lkups$versions_paths$latest_release
empty_response <- pip('AGO', 2000, lkup = lkups)[-1]
reporting_level_list <- c("national", "rural", "urban")

tmp <- ui_cp_charts(country = "AGO", povline = 1.9, lkup = lkups)
tmp1 <- tmp$AGO$pov_charts[[1]]$pov_trend[-c(1:3)]
tmp2 <- tmp$AGO$pov_charts[[1]]$pov_mrv[-c(1:11)]
empty_response_cp_poverty <- list(pov_trend = tmp1, pov_mrv = tmp2)

empty_response_grp <- pip_grp('AGO', 2000, lkup = lkups)[-1]

usethis::use_data(
  empty_response,
  empty_response_cp_poverty,
  reporting_level_list,
  empty_response_grp,
  overwrite = TRUE
)
