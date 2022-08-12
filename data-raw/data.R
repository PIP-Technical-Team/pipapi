library(pipapi)
# lkups <- pipapi::create_versioned_lkups(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))
lkups <- create_versioned_lkups(Sys.getenv('PIPAPI_FAKEDATA_FOLDER'))
lkups <- lkups$versions_paths$`20211212_2011_01_01_PROD`

ctr <- "NGA"

empty_response <- pip(ctr, 2012, lkup = lkups)[-1]
reporting_level_list <- c("national", "rural", "urban")

tmp <- ui_cp_charts(country = ctr, povline = 1.9, lkup = lkups)
tmp1 <- tmp[[ctr]]$pov_charts[[1]]$pov_trend[-c(1:3)]
tmp2 <- tmp[[ctr]]$pov_charts[[1]]$pov_mrv[-c(1:11)]
empty_response_cp_poverty <- list(pov_trend = tmp1, pov_mrv = tmp2)

empty_response_grp <-  pip(ctr, 2012, lkup = lkups)[-1]

usethis::use_data(
  empty_response,
  empty_response_cp_poverty,
  reporting_level_list,
  empty_response_grp,
  overwrite = TRUE
)

