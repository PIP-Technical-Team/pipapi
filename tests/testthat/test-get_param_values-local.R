# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))

res <- get_param_values(lkup = lkups, version = "latest_release")

test_that("get_param_values() returns a data.table", {
  expect_equal(class(res), c("data.table", "data.frame"))
})

test_that("get_param_values() returns expected columns", {
  expect_equal(colnames(res), c("param_names",
                                "param_values",
                                "param_boundaries",
                                "param_types"))
  expect_equal(unname(unlist(lapply(res, class))), rep("character", 4))
})
