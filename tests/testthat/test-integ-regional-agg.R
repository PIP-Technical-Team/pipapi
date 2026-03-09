# Integration tests for pip_grp() (regional aggregations via fg_pip path)
# All tests require PIPAPI_DATA_ROOT_FOLDER_LOCAL + vintage TEST_VINTAGE.
#
# NOTE: local_mocked_bindings(get_caller_names = ...) is scoped inside each
# test_that() block so the mock does not leak across files.

# ── Output structure ──────────────────────────────────────────────────────────

test_that("pip_grp: output is data.table", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  out <- pip_grp("all", year = 2010, group_by = "wb", povline = 1.9,
                 lkup = test_lkup)
  expect_s3_class(out, "data.table")
})

test_that("pip_grp: empty response for future year", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  out <- pip_grp("all", year = 2099, group_by = "wb", lkup = test_lkup)
  expect_equal(nrow(out), 0L)
})

test_that("pip_grp: column schema is identical between empty and non-empty", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  tmp1 <- pip_grp("all", year = 2000, group_by = "wb",   lkup = test_lkup)
  tmp2 <- pip_grp("all", year = 2099, group_by = "wb",   lkup = test_lkup)
  tmp3 <- pip_grp("all", year = 2099, group_by = "none", lkup = test_lkup)
  expect_identical(names(tmp1), names(tmp2))
  expect_identical(names(tmp1), names(tmp3))
  expect_identical(sapply(tmp1, class), sapply(tmp2, class))
})

# ── WB regional aggregation ───────────────────────────────────────────────────

test_that("pip_grp: group_by='wb' for year=2010 returns 10 rows (8 regions + AFE/AFW + WLD)", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  out <- pip_grp("all", year = 2010, group_by = "wb", povline = 1.9,
                 lkup = test_lkup)
  # 8 standard WB regions + AFE + AFW + WLD = 10 in this vintage
  expect_equal(nrow(out), 10L)
})

test_that("pip_grp: WLD aggregate is present in group_by='wb' output", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  out <- pip_grp("all", year = 2010, group_by = "wb", povline = 1.9,
                 lkup = test_lkup)
  expect_true("WLD" %in% out$region_code)
})

test_that("pip_grp: headcount is between 0 and 1 for all regions", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  out <- pip_grp("all", year = 2010, group_by = "wb", povline = 1.9,
                 lkup = test_lkup)
  expect_true(all(out$headcount >= 0 & out$headcount <= 1, na.rm = TRUE))
})

test_that("pip_grp: pop_in_poverty = headcount * reporting_pop (rounded)", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  out <- pip_grp("all", year = 2010, group_by = "wb", povline = 1.9,
                 lkup = test_lkup)
  expected <- round(out$headcount * out$reporting_pop, 0)
  expect_equal(out$pop_in_poverty, expected)
})

# ── Custom aggregation (group_by = "none") ────────────────────────────────────

test_that("pip_grp: group_by='none' returns exactly 1 row labelled CUSTOM", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  out <- pip_grp("all", year = 2000, group_by = "none", povline = 3.5,
                 lkup = test_lkup)
  expect_equal(nrow(out), 1L)
  expect_equal(out$region_code, "CUSTOM")
  expect_equal(out$region_name, "CUSTOM")
})

# ── Single / multiple region selection ───────────────────────────────────────

test_that("pip_grp: single region selection returns 1 row for that region", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  # Use output region codes (SSF, not SSA — SSA is a query alias)
  out <- pip_grp("SSF", year = 2018, group_by = "wb", povline = 1.9,
                 lkup = test_lkup)
  expect_equal(nrow(out), 1L)
  expect_equal(out$region_code, "SSF")
})

test_that("pip_grp: multiple region selection returns correct number of rows", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  out <- pip_grp(c("SSF", "MEA"), year = 2018, group_by = "wb", povline = 1.9,
                 lkup = test_lkup)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$region_code, c("SSF", "MEA"))
})

# ── Year selection ────────────────────────────────────────────────────────────

test_that("pip_grp: year='all' returns one row per reference year for a single region", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  # LCN is the output region code for Latin America
  out <- pip_grp("LCN", year = "all", group_by = "wb", povline = 1.9,
                 lkup = test_lkup)
  n_ref <- length(unique(test_lkup$ref_lkup$reporting_year))
  expect_equal(nrow(out), n_ref)
})

# ── Monotonicity ──────────────────────────────────────────────────────────────

test_that("pip_grp: higher povline gives weakly higher global headcount", {
  skip_if_no_lkup()
  local_mocked_bindings(get_caller_names = function() c("pip_grp"))
  lo <- pip_grp("all", year = 2015, group_by = "wb", povline = 1.9,  lkup = test_lkup)
  hi <- pip_grp("all", year = 2015, group_by = "wb", povline = 3.65, lkup = test_lkup)
  wld_lo <- lo[region_code == "WLD", headcount]
  wld_hi <- hi[region_code == "WLD", headcount]
  expect_gte(wld_hi, wld_lo)
})
