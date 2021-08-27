lkups <- pipapi::create_lkups(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))
empty_response <- pip('AGO', 2000, lkup = lkups)[-1]
reporting_level_list <- c("national", "rural", "urban")

usethis::use_data(empty_response,
  reporting_level_list,
  overwrite = TRUE
)
