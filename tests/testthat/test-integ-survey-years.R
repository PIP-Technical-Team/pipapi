# Integration tests for pip() with fill_gaps = FALSE (survey years / rg_pip path)
# All tests require PIPAPI_DATA_ROOT_FOLDER_LOCAL + vintage TEST_VINTAGE.
# They are silently skipped on machines without data.

# ── Output structure ──────────────────────────────────────────────────────────

test_that("pip survey: output is data.table", {
  skip_if_no_lkup()
  out <- pip("AGO", year = 2000, povline = 1.9, lkup = test_lkup)
  expect_s3_class(out, "data.table")
})

test_that("pip survey: empty response for future year", {
  skip_if_no_lkup()
  out <- pip("COL", year = 2099, lkup = test_lkup)
  expect_equal(nrow(out), 0L)
})

test_that("pip survey: column types are consistent across empty and non-empty responses", {
  skip_if_no_lkup()
  tmp1 <- pip("AGO", year = 2000, lkup = test_lkup)
  tmp2 <- pip("AGO", year = 2099, lkup = test_lkup)   # empty
  expect_identical(names(tmp1), names(tmp2))
  expect_identical(sapply(tmp1, class), sapply(tmp2, class))
})

# ── Year selection ────────────────────────────────────────────────────────────

test_that("pip survey: year='all' returns all survey years for a single country", {
  skip_if_no_lkup()
  out <- pip("AGO", year = "all", povline = 1.9, lkup = test_lkup)
  n_expected <- test_lkup$svy_lkup[country_code == "AGO", .N]
  expect_equal(nrow(out), n_expected)
})

test_that("pip survey: year='MRV' returns only the most-recent survey year", {
  skip_if_no_lkup()
  out <- pip("AGO", year = "MRV", povline = 1.9, lkup = test_lkup)
  mrv <- max(test_lkup$svy_lkup[country_code == "AGO", reporting_year])
  expect_equal(nrow(out), 1L)
  expect_equal(out$reporting_year, mrv)
})

test_that("pip survey: specific numeric year filters correctly", {
  skip_if_no_lkup()
  out <- pip("AGO", year = 2000, povline = 1.9, lkup = test_lkup)
  expect_equal(nrow(out), 1L)
  expect_equal(out$reporting_year, 2000L)
})

test_that("pip survey: multiple povline values are stacked correctly", {
  skip_if_no_lkup()
  out1 <- pip("AGO", year = 2000, povline = 1.9,   lkup = test_lkup)
  out2 <- pip("AGO", year = 2000, povline = 1.675, lkup = test_lkup)
  out3 <- pip("AGO", year = 2000, povline = c(1.675, 1.9), lkup = test_lkup)
  expect_equal(nrow(out3), 2L)
  expect_identical(rbind(out2, out1), out3)
})

# ── Welfare-type selection ────────────────────────────────────────────────────

test_that("pip survey: welfare_type='consumption' returns only consumption rows", {
  skip_if_no_lkup()
  out <- pip("all", year = "all", povline = 3.5, welfare_type = "consumption",
             lkup = test_lkup)
  expect_equal(unique(out$welfare_type), "consumption")
})

test_that("pip survey: welfare_type='income' returns only income rows", {
  skip_if_no_lkup()
  out <- pip("all", year = "all", povline = 3.5, welfare_type = "income",
             lkup = test_lkup)
  expect_equal(unique(out$welfare_type), "income")
})

test_that("pip survey: welfare_type='all' returns both consumption and income", {
  skip_if_no_lkup()
  out <- pip("all", year = "all", povline = 3.5, welfare_type = "all",
             lkup = test_lkup)
  expect_setequal(unique(out$welfare_type), c("consumption", "income"))
})

# ── Reporting-level selection ─────────────────────────────────────────────────

test_that("pip survey: reporting_level='national' returns only national rows", {
  skip_if_no_lkup()
  out <- pip("all", year = "all", povline = 3.5, reporting_level = "national",
             lkup = test_lkup)
  expect_true(all(out$reporting_level == "national"))
})

test_that("pip survey: reporting_level='urban' returns only urban rows", {
  skip_if_no_lkup()
  out <- pip("all", year = "all", povline = 3.5, reporting_level = "urban",
             lkup = test_lkup)
  expect_true(all(out$reporting_level == "urban"))
})

test_that("pip survey: reporting_level='rural' returns only rural rows", {
  skip_if_no_lkup()
  out <- pip("all", year = "all", povline = 3.5, reporting_level = "rural",
             lkup = test_lkup)
  expect_true(all(out$reporting_level == "rural"))
})

test_that("pip survey: reporting_level='all' returns national, urban, and rural", {
  skip_if_no_lkup()
  out <- pip("all", year = "all", povline = 3.5, reporting_level = "all",
             lkup = test_lkup)
  expect_setequal(unique(out$reporting_level), c("national", "rural", "urban"))
})

# ── CHN sub-national reporting ────────────────────────────────────────────────

test_that("pip survey: CHN 2019 with reporting_level='all' returns 3 rows", {
  skip_if_no_lkup()
  out <- pip("CHN", year = 2019, povline = 1.9, reporting_level = "all",
             lkup = test_lkup)
  expect_equal(nrow(out), 3L)
  expect_setequal(out$reporting_level, c("national", "rural", "urban"))
})

test_that("pip survey: CHN 2019 with reporting_level='national' returns 1 row", {
  skip_if_no_lkup()
  out <- pip("CHN", year = 2019, povline = 1.9, reporting_level = "national",
             lkup = test_lkup)
  expect_equal(nrow(out), 1L)
  expect_equal(out$reporting_level, "national")
})

# ── Distributional stats (survey years) ──────────────────────────────────────

test_that("pip survey: distributional stats are NA for interpolated years", {
  skip_if_no_lkup()
  # AGO 2004 is an interpolated year (between surveys at 2000 and 2008).
  # New-path behaviour: gini and mld are NA; median is interpolated (not NA).
  out <- pip("AGO", year = 2004, fill_gaps = TRUE, lkup = test_lkup)
  expect_equal(out$estimation_type, "interpolation")
  expect_true(is.na(out$gini))
  expect_true(is.na(out$mld))
  expect_false(is.na(out$median))  # median is filled via interpolation on new path
})

test_that("pip survey: extrapolated years have correct estimation_type and NA gini", {
  skip_if_no_lkup()
  # AGO 1981 is before the first survey — new-path extrapolation.
  # gini is NA (not copied from survey); median is extrapolated (non-NA, differs from survey).
  out_extrap <- pip("AGO", year = 1981, fill_gaps = TRUE, lkup = test_lkup)
  expect_equal(out_extrap$estimation_type, "extrapolation")
  expect_true(is.na(out_extrap$gini))
  expect_false(is.na(out_extrap$median))
})

# ── popshare ──────────────────────────────────────────────────────────────────

test_that("pip survey: popshare is consistent with povline for microdata country", {
  skip_if_no_lkup()
  povline <- 2.0
  pl <- pip("AGO", year = 2008, povline = povline, lkup = test_lkup)
  ps <- pip("AGO", year = 2008, popshare = pl$headcount, lkup = test_lkup)
  expect_equal(round(pl$headcount, 3), round(ps$headcount, 3))
  expect_equal(povline, round(ps$poverty_line, 2))
})

test_that("pip survey: CHN national uses 'group' distribution_type on new path", {
  skip_if_no_lkup()
  # New-path: CHN national is distribution_type 'group' (not 'aggregate').
  # popshare works for group distributions.
  pl <- pip("CHN", year = 2018, povline = 2.0, reporting_level = "national",
            lkup = test_lkup)
  ps <- pip("CHN", year = 2018, popshare = 0.5, reporting_level = "national",
            lkup = test_lkup)
  expect_equal(nrow(pl), 1L)
  expect_equal(pl$distribution_type, "group")
  expect_equal(nrow(ps), 1L)  # popshare works on new path for group distributions
})
