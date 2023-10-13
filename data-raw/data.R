# library(pipapi)
# lkups <- pipapi::create_versioned_lkups(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))
pkgload::load_all()

data_dir <-
  if (Sys.info()["user"] == "wb384996") {
    Sys.getenv('PIPAPI_DATA_ROOT_FOLDER_SERVER')
  } else {
    Sys.getenv('PIPAPI_DATA_ROOT_FOLDER_LOCAL')
  }

fs::dir_tree(data_dir, recurse = 0)

lkups <- create_versioned_lkups(data_dir,
                                vintage_pattern = "PROD$")
lkup <- lkups$versions_paths[[lkups$latest_release]]

ctr  <- "AGO"
year <- 2000


empty_response <- pip(ctr, year, lkup = lkup)[-1]
reporting_level_list <- c("national", "rural", "urban")

tmp <- ui_cp_charts(country = ctr, povline = 1.9, lkup = lkup)
tmp1 <- tmp[[ctr]]$pov_charts[[1]]$pov_trend[-c(1:3)]
tmp2 <- tmp[[ctr]]$pov_charts[[1]]$pov_mrv[-c(1:11)]
empty_response_cp_poverty <- list(pov_trend = tmp1, pov_mrv = tmp2)

empty_response_grp <- pip_grp(ctr, year, lkup = lkup)[-1]

usethis::use_data(
  empty_response,
  empty_response_cp_poverty,
  reporting_level_list,
  empty_response_grp,
  overwrite = TRUE
)
