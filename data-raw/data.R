## code to prepare `data` dataset goes here

usethis::use_data(data, overwrite = TRUE)

empty_response <- create_empty_response()

usethis::use_data(empty_response,
                  overwrite = TRUE)
