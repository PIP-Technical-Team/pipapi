skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")
data_folder_root <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER")
lkups <- pipapi:::clean_api_data(data_folder_root)

test_that("get_pip_version() is working", {
  res <- get_pip_version(lkup = lkups)
  expect_equal(names(res), c("valid_query_parameters", "packages_version", "data_versions"))
  expect_equal(names(res$packages_version), c("pipapi", "wbpip"))
  expect_equal(res$data_versions[[1]], paste0(data_folder_root, "00010101"))
  expect_equal(res$data_versions[[2]], paste0(data_folder_root, "20210401"))
})
