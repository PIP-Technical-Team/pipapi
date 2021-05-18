## code to prepare `data` dataset goes here

empty_response <- create_empty_response()
svy_coverage_list <- c("national", "rural", "urban")

usethis::use_data(empty_response,
                  svy_coverage_list,
                  overwrite = TRUE)
