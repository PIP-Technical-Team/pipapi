# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
version <- lkups$latest_release

res <- get_param_values(lkup = lkups, version = version)

test_that("get_param_values() returns expected format", {
  res <- get_param_values(lkup = lkups,
                          endpoint = "all",
                          version = version)
  # Expected class is being returned
  expect_equal(class(res), c("data.table", "data.frame"))
  # Expected column names are being returned
  expect_equal(colnames(res), c("param_names",
                                "param_values",
                                "param_boundaries",
                                "param_types"))
  expect_equal(unname(unlist(lapply(res, class))), rep("character", 4))

  res <- get_param_values(lkup = lkups,
                          endpoint = "pip",
                          version = version)
  # Expected class is being returned
  expect_equal(class(res), c("data.table", "data.frame"))
  # Expected column names are being returned
  expect_equal(colnames(res), c("param_names",
                                "param_values",
                                "param_boundaries",
                                "param_types"))
  expect_equal(unname(unlist(lapply(res, class))), rep("character", 4))
})

test_that("get_param_values() works as expected for specific endpoint", {
  res <- get_param_values(lkup = lkups,
                          endpoint = "pip",
                          version = version)
  expect_true(nrow(res) > 0)
  expect_equal(sort(unique(res$param_names)), c("additional_ind",
                                                "country",
                                                "fill_gaps",
                                                "group_by",
                                                "popshare",
                                                "povline",
                                                "ppp",
                                                "reporting_level",
                                                "version",
                                                "welfare_type",
                                                "year"))

  res <- get_param_values(lkup = lkups,
                          endpoint = "pip-grp",
                          version = version)
  expect_true(nrow(res) > 0)
  expect_equal(sort(unique(res$param_names)), c("country",
                                                "group_by",
                                                "povline",
                                                "reporting_level",
                                                "version",
                                                "welfare_type",
                                                "year"))
})



