# Integration tests for pip() with fill_gaps = TRUE (lineup years / fg_pip path)
# All tests require PIPAPI_DATA_ROOT_FOLDER_LOCAL + vintage TEST_VINTAGE.

# ── Output structure ──────────────────────────────────────────────────────────

test_that("pip lineup: output is data.table", {
  skip_if_no_lkup()
  out <- pip("AGO", year = 2015, fill_gaps = TRUE, povline = 1.9, lkup = test_lkup)
  expect_s3_class(out, "data.table")
})

test_that("pip lineup: empty response for future year", {
  skip_if_no_lkup()
  out <- pip("COL", year = 2099, fill_gaps = TRUE, lkup = test_lkup)
  expect_equal(nrow(out), 0L)
})

test_that("pip lineup: column types consistent between empty and non-empty", {
  skip_if_no_lkup()
  tmp1 <- pip("AGO", year = 2015, fill_gaps = TRUE, lkup = test_lkup)
  tmp2 <- pip("AGO", year = 2099, fill_gaps = TRUE, lkup = test_lkup)
  expect_identical(names(tmp1), names(tmp2))
  expect_identical(sapply(tmp1, class), sapply(tmp2, class))
})

# ── Year coverage ─────────────────────────────────────────────────────────────

test_that("pip lineup: year='all' returns one row per ref year for a single country", {
  skip_if_no_lkup()
  out <- pip("AGO", year = "all", fill_gaps = TRUE, povline = 1.9,
             lkup = test_lkup)
  n_ref <- length(unique(test_lkup$ref_lkup$reporting_year))
  expect_equal(nrow(out), n_ref)
})

test_that("pip lineup: year='MRV' returns the most-recent ref year", {
  skip_if_no_lkup()
  out <- pip("AGO", year = "MRV", fill_gaps = TRUE, povline = 1.9,
             lkup = test_lkup)
  mrv <- max(test_lkup$ref_lkup[country_code == "AGO", reporting_year])
  expect_equal(nrow(out), 1L)
  expect_equal(out$reporting_year, mrv)
})

# ── Mixed distribution types ──────────────────────────────────────────────────

test_that("pip lineup: CHN 1993 with reporting_level='all' returns 3 rows (sub-national)", {
  skip_if_no_lkup()
  # CHN 1993 is a survey year with rural + urban + national in ref_lkup
  out <- pip("CHN", year = 1993, fill_gaps = TRUE, povline = 1.9,
             reporting_level = "all", lkup = test_lkup)
  expect_equal(nrow(out), 3L)
  expect_setequal(out$reporting_level, c("national", "rural", "urban"))
})

test_that("pip lineup: ZWE 2015 returns 1 row (mixed group/micro distribution)", {
  skip_if_no_lkup()
  out <- pip("ZWE", year = 2015, fill_gaps = TRUE, povline = 1.9,
             lkup = test_lkup)
  expect_equal(nrow(out), 1L)
})

# ── Estimation type labels ────────────────────────────────────────────────────

test_that("pip lineup: survey years in ref_lkup labelled as 'survey' estimation_type", {
  skip_if_no_lkup()
  # CHN 1993 is a survey year in ref_lkup for this vintage
  out <- pip("CHN", year = 1993, fill_gaps = TRUE, povline = 1.9,
             reporting_level = "national", lkup = test_lkup)
  expect_equal(out$estimation_type, "survey")
})

test_that("pip lineup: interpolated year has estimation_type 'interpolation'", {
  skip_if_no_lkup()
  out <- pip("AGO", year = 2004, fill_gaps = TRUE, povline = 1.9,
             lkup = test_lkup)
  expect_equal(out$estimation_type, "interpolation")
})

test_that("pip lineup: extrapolated year has estimation_type 'extrapolation'", {
  skip_if_no_lkup()
  out <- pip("AGO", year = 1985, fill_gaps = TRUE, povline = 1.9,
             lkup = test_lkup)
  expect_equal(out$estimation_type, "extrapolation")
})

# ── Distributional stats (new-path behaviour) ─────────────────────────────────

test_that("pip lineup: interpolated year has NA gini but non-NA median", {
  skip_if_no_lkup()
  out <- pip("AGO", year = 2004, fill_gaps = TRUE, lkup = test_lkup)
  expect_true(is.na(out$gini))
  expect_true(is.na(out$mld))
  expect_false(is.na(out$median))
})

test_that("pip lineup: extrapolated year has NA gini", {
  skip_if_no_lkup()
  out <- pip("AGO", year = 1985, fill_gaps = TRUE, lkup = test_lkup)
  expect_true(is.na(out$gini))
})

test_that("pip lineup: survey year has non-NA headcount and non-NA median", {
  skip_if_no_lkup()
  # CHN 1993 is a survey year in ref_lkup — structural stats should be non-NA
  out <- pip("CHN", year = 1993, fill_gaps = TRUE, povline = 1.9,
             reporting_level = "national", lkup = test_lkup)
  expect_false(is.na(out$headcount))
  expect_false(is.na(out$mean))
  expect_false(is.na(out$median))
})

# ── Multiple countries / all countries ───────────────────────────────────────

test_that("pip lineup: all countries year=2015 returns data.table with country_code column", {
  skip_if_no_lkup()
  out <- pip("all", year = 2015, fill_gaps = TRUE, povline = 1.9,
             lkup = test_lkup)
  expect_s3_class(out, "data.table")
  expect_true("country_code" %in% names(out))
  expect_gt(nrow(out), 50L)
})

test_that("pip lineup: welfare_type filter works with fill_gaps", {
  skip_if_no_lkup()
  out_c <- pip("all", year = 2015, fill_gaps = TRUE, welfare_type = "consumption",
               lkup = test_lkup)
  out_i <- pip("all", year = 2015, fill_gaps = TRUE, welfare_type = "income",
               lkup = test_lkup)
  expect_equal(unique(out_c$welfare_type), "consumption")
  expect_equal(unique(out_i$welfare_type), "income")
})

# ── Monotonicity ──────────────────────────────────────────────────────────────

test_that("pip lineup: higher povline gives weakly higher headcount (AGO 2015)", {
  skip_if_no_lkup()
  lo <- pip("AGO", year = 2015, fill_gaps = TRUE, povline = 1.9,  lkup = test_lkup)
  hi <- pip("AGO", year = 2015, fill_gaps = TRUE, povline = 3.65, lkup = test_lkup)
  expect_gte(hi$headcount, lo$headcount)
})
