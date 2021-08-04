# lkups <- pipapi:::clean_api_data(
#   data_folder_root = "../testdata/20210401/")
#
# tmp <- pip(country = "all",
#            year    = "all",
#            povline = 3.5,
#            lkup = lkups)
#
# test_that("create_empty_response returns expected columns", {
#   empty_response <- create_empty_response()
#   expect_equal(names(empty_response), names(tmp))
# })
# names(tmp)[!names(tmp) %in% names(empty_response)]
