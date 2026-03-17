# Pure unit tests for pip_grp() that don't require live data.
# Integration tests (regional aggregations, year selection, censoring, etc.)
# to be added in: test-integ-regional-agg.R (Step 4).

test_that("pip_grp returns empty response when no metadata found", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  tmp1 <- pip_grp("all", year = 2050, lkup = test_lkup, group_by = "none")
  tmp2 <- pip_grp("all", year = 2050, lkup = test_lkup, group_by = "wb")
  expect_equal(nrow(tmp1), 0L)
  expect_equal(nrow(tmp2), 0L)
})

test_that("pip_grp returned columns consistent across group_by values", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  tmp1 <- pip_grp("all", 2000, lkup = test_lkup, group_by = "none")
  tmp2 <- pip_grp("all", 2000, lkup = test_lkup, group_by = "wb")
  tmp3 <- pip_grp("all", 2050, lkup = test_lkup, group_by = "wb")
  expect_identical(names(tmp1), names(tmp2))
  expect_identical(names(tmp1), names(tmp3))
  expect_identical(sapply(tmp1, class), sapply(tmp2, class))
  expect_identical(sapply(tmp1, class), sapply(tmp3, class))
})

test_that("pip_grp returns CUSTOM region for group_by='none'", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  tmp <- pip_grp("all", year = 2000, group_by = "none", povline = 3.5, lkup = test_lkup)
  expect_equal(nrow(tmp), 1L)
  expect_identical(tmp$region_name, "CUSTOM")
  expect_identical(tmp$region_code, "CUSTOM")
})
