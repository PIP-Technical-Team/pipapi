## code to prepare `data` dataset goes here

empty_response <- create_empty_response()
reporting_level_list <- c("national", "rural", "urban")

usethis::use_data(empty_response,
  reporting_level_list,
  overwrite = TRUE
)
