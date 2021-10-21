skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))

test_that("get_pip_version() is working", {
  res <- get_pip_version(lkup = lkups)
  expect_equal(class(res), "list")
  expect_equal(names(res), c("available_data_versions",
                             "package_versions",
                             "r_version",
                             "server_os",
                             "server_time"))
})
