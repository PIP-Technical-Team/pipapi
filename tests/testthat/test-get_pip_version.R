skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
lkups <- lkups$versions_paths$latest_release

test_that("get_pip_version() is working", {
  res <- get_pip_version(lkup = lkups)
  expect_equal(names(res), c("valid_query_parameters", "packages_version", "data_versions"))
  expect_equal(names(res$packages_version), c("pipapi", "wbpip"))
  expect_equal(res$data_versions$values[1], "latest_release")
})
