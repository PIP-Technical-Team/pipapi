skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))

test_that("get_pip_version() is working", {
  res <- get_pip_version(pip_packages = c("pipapi",
                                          "wbpip"),
                         data_versions = lkups$versions)
  expect_equal(class(res), "list")
  expect_equal(names(res), c("available_data_versions",
                             "pip_packages",
                             "other_packages",
                             "r_version",
                             "server_os",
                             "server_time"))
})

test_that("all objects are correctly passed and used", {
  root <- rprojroot::is_r_package
  tmp <- lintr::lint(filename = root$find_file("R/get_pip_version.R"),
                     linters = lintr::object_usage_linter())

  expect_equal(length(tmp), 0)
})
