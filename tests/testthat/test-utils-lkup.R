# test-utils-lkup.R
#
# Unit tests for lookup-table filtering helpers in utils-lkup.R.
# Functions: select_country(), select_reporting_level(), select_years(),
#            filter_lkup(), lkup_filter()
#
# All synthetic inline data — no external files required.

library(data.table)


# ---------------------------------------------------------------------------
# Helper: minimal svy_lkup-shaped data.table
# ---------------------------------------------------------------------------

.make_lkup_dt <- function() {
  data.table(
    country_code       = c("AAA", "AAA", "BBB", "BBB", "CCC"),
    reporting_year     = c(2000L, 2005L, 2000L, 2010L, 2010L),
    reporting_level    = c("national", "national", "national", "national", "urban"),
    welfare_type       = c("consumption", "consumption", "income", "income", "consumption"),
    survey_coverage    = c("national", "national", "national", "national", "urban"),
    is_used_for_aggregation = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    distribution_type  = c("micro", "micro", "micro", "micro", "aggregate"),
    region_code        = c("REG1", "REG1", "REG2", "REG2", "REG2"),
    pip_region_code    = c("REG1", "REG1", "REG2", "REG2", "REG2")
  )
}


# ===========================================================================
# 1) select_country()
# ===========================================================================

test_that("select_country: 'ALL' keeps every row", {
  lkup <- .make_lkup_dt()
  keep <- rep(TRUE, nrow(lkup))
  result <- select_country(lkup, keep, country = "ALL", valid_regions = character(0))
  expect_true(all(result))
})

test_that("select_country: single country code filters correctly", {
  lkup <- .make_lkup_dt()
  keep <- rep(TRUE, nrow(lkup))
  result <- select_country(lkup, keep, country = "AAA", valid_regions = character(0))
  expect_equal(sum(result), 2L)
  expect_true(all(lkup$country_code[result] == "AAA"))
})

test_that("select_country: multiple country codes union correctly", {
  lkup <- .make_lkup_dt()
  keep <- rep(TRUE, nrow(lkup))
  result <- select_country(lkup, keep, country = c("AAA", "BBB"),
                           valid_regions = character(0))
  expect_equal(sum(result), 4L)
})

test_that("select_country: unknown country code gives all FALSE", {
  lkup <- .make_lkup_dt()
  keep <- rep(TRUE, nrow(lkup))
  result <- select_country(lkup, keep, country = "ZZZ",
                           valid_regions = character(0))
  expect_true(all(!result))
})

test_that("select_country: region code selects matching rows via code columns", {
  lkup <- .make_lkup_dt()
  keep <- rep(TRUE, nrow(lkup))
  result <- select_country(lkup, keep, country = "REG1",
                           valid_regions = c("REG1", "REG2"))
  # AAA rows have region_code / pip_region_code == "REG1"
  expect_true(all(lkup$country_code[result] == "AAA"))
})

test_that("select_country: respects incoming keep mask", {
  lkup <- .make_lkup_dt()
  # Pre-filter: only keep first 3 rows
  keep <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  result <- select_country(lkup, keep, country = "BBB",
                           valid_regions = character(0))
  # BBB row 3 was in keep, row 4 was not
  expect_equal(sum(result), 1L)
  expect_equal(lkup$country_code[result], "BBB")
})


# ===========================================================================
# 2) select_reporting_level()
# ===========================================================================

test_that("select_reporting_level: 'all' leaves keep unchanged", {
  lkup  <- .make_lkup_dt()
  keep  <- rep(TRUE, nrow(lkup))
  result <- select_reporting_level(lkup, keep, reporting_level = "all")
  expect_identical(result, keep)
})

test_that("select_reporting_level: 'national' keeps national + aggregation rows", {
  lkup  <- .make_lkup_dt()
  # Mark row 5 (urban) as used for aggregation
  lkup$is_used_for_aggregation[5] <- TRUE
  keep  <- rep(TRUE, nrow(lkup))
  result <- select_reporting_level(lkup, keep, reporting_level = "national")
  # national rows: 1,2,3,4 + row 5 (aggregation=TRUE)
  expect_equal(sum(result), 5L)
})

test_that("select_reporting_level: 'national' excludes non-national non-aggregation rows", {
  lkup  <- .make_lkup_dt()
  keep  <- rep(TRUE, nrow(lkup))
  result <- select_reporting_level(lkup, keep, reporting_level = "national")
  # row 5 is urban, is_used_for_aggregation=FALSE -> excluded
  expect_false(result[5])
})

test_that("select_reporting_level: 'urban' matches on survey_coverage or reporting_level", {
  lkup <- .make_lkup_dt()
  keep <- rep(TRUE, nrow(lkup))
  result <- select_reporting_level(lkup, keep, reporting_level = "urban")
  # Only row 5 has survey_coverage/reporting_level == "urban"
  expect_equal(sum(result), 1L)
  expect_equal(lkup$reporting_level[result], "urban")
})

test_that("select_reporting_level: respects incoming keep mask", {
  lkup <- .make_lkup_dt()
  keep <- c(FALSE, FALSE, FALSE, FALSE, TRUE)
  result <- select_reporting_level(lkup, keep, reporting_level = "national")
  # row 5 kept=TRUE but reporting_level=urban, aggregation=FALSE -> dropped
  expect_true(all(!result))
})


# ===========================================================================
# 3) filter_lkup()
# ===========================================================================

test_that("filter_lkup: NULL popshare returns metadata unchanged", {
  lkup <- .make_lkup_dt()
  result <- filter_lkup(lkup, popshare = NULL)
  expect_equal(nrow(result), nrow(lkup))
})

test_that("filter_lkup: non-NULL popshare drops aggregate distribution rows", {
  lkup <- .make_lkup_dt()
  # Row 5 has distribution_type = "aggregate"
  result <- filter_lkup(lkup, popshare = 0.5)
  expect_equal(nrow(result), nrow(lkup) - 1L)
  expect_true(all(result$distribution_type != "aggregate"))
})

test_that("filter_lkup: all rows kept when no aggregates and popshare set", {
  lkup <- .make_lkup_dt()
  lkup$distribution_type <- "micro"   # no aggregate rows
  result <- filter_lkup(lkup, popshare = 0.5)
  expect_equal(nrow(result), nrow(lkup))
})


# ===========================================================================
# 4) lkup_filter()
# ===========================================================================

test_that("lkup_filter: country + welfare_type + reporting_level all filter together", {
  lkup <- .make_lkup_dt()
  result <- lkup_filter(
    lkup          = lkup,
    country       = "AAA",
    year          = "ALL",
    valid_regions = character(0),
    reporting_level = "all",
    welfare_type  = "consumption",
    data_dir      = NULL
  )
  expect_true(all(result$country_code == "AAA"))
  expect_true(all(result$welfare_type == "consumption"))
})

test_that("lkup_filter: year='ALL' keeps all matching years", {
  lkup <- .make_lkup_dt()
  result <- lkup_filter(
    lkup          = lkup,
    country       = "BBB",
    year          = "ALL",
    valid_regions = character(0),
    reporting_level = "all",
    welfare_type  = "all",
    data_dir      = NULL
  )
  expect_equal(sort(result$reporting_year), c(2000L, 2010L))
})

test_that("lkup_filter: specific year filters to that year only", {
  lkup <- .make_lkup_dt()
  result <- lkup_filter(
    lkup          = lkup,
    country       = "ALL",
    year          = "2010",
    valid_regions = character(0),
    reporting_level = "all",
    welfare_type  = "all",
    data_dir      = NULL
  )
  expect_true(all(result$reporting_year == 2010L))
  expect_equal(nrow(result), 2L)
})

test_that("lkup_filter: no rows match returns zero-row data.table", {
  lkup <- .make_lkup_dt()
  result <- lkup_filter(
    lkup          = lkup,
    country       = "ZZZ",
    year          = "ALL",
    valid_regions = character(0),
    reporting_level = "all",
    welfare_type  = "all",
    data_dir      = NULL
  )
  expect_equal(nrow(result), 0L)
})
