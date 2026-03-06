# test-additional_indicators.R
#
# Unit tests for additional_indicators.R
# Functions: get_additional_indicators(), get_additional_indicators_grp()
#
# All synthetic inline data — no external files required.

library(data.table)


# ---------------------------------------------------------------------------
# Helper: minimal pip-output data.table
# ---------------------------------------------------------------------------

.make_pip_dt <- function(n = 3) {
  data.table(
    headcount       = rep(0.4, n),
    poverty_gap     = rep(0.15, n),
    poverty_line    = rep(2.15, n),
    reporting_pop   = rep(1e6, n),
    decile1         = rep(0.03, n),
    decile2         = rep(0.04, n),
    decile3         = rep(0.05, n),
    decile4         = rep(0.06, n),
    decile5         = rep(0.07, n),
    decile6         = rep(0.08, n),
    decile7         = rep(0.09, n),
    decile8         = rep(0.10, n),
    decile9         = rep(0.11, n),
    decile10        = rep(0.37, n)
  )
}

.make_grp_dt <- function(n = 2) {
  data.table(
    headcount     = rep(0.3, n),
    poverty_gap   = rep(0.10, n),
    poverty_line  = rep(2.15, n),
    reporting_pop = rep(5e6, n)
  )
}


# ===========================================================================
# get_additional_indicators()
# ===========================================================================

test_that("get_additional_indicators: returns TRUE invisibly", {
  dt <- .make_pip_dt()
  result <- get_additional_indicators(dt)
  expect_true(result)
})

test_that("get_additional_indicators: adds expected new columns", {
  dt <- .make_pip_dt()
  get_additional_indicators(dt)
  expected_cols <- c(
    "bottom40", "pop_in_poverty", "average_shortfall",
    "total_shortfall", "income_gap_ratio", "palma_ratio", "p90p10_ratio"
  )
  expect_true(all(expected_cols %in% names(dt)))
})

test_that("get_additional_indicators: bottom40 = sum of deciles 1-4", {
  dt <- .make_pip_dt()
  get_additional_indicators(dt)
  expected <- dt$decile1 + dt$decile2 + dt$decile3 + dt$decile4
  expect_equal(dt$bottom40, expected)
})

test_that("get_additional_indicators: new_indicators_names attribute is set", {
  dt <- .make_pip_dt()
  get_additional_indicators(dt)
  new_names <- attr(dt, "new_indicators_names")
  expect_false(is.null(new_names))
  expect_true(length(new_names) > 0L)
})

test_that("get_additional_indicators: modifies dt in place (same reference)", {
  dt <- .make_pip_dt()
  original_ptr <- data.table::address(dt)
  get_additional_indicators(dt)
  expect_equal(data.table::address(dt), original_ptr)
})

test_that("get_additional_indicators: pop_in_poverty is non-negative", {
  dt <- .make_pip_dt()
  get_additional_indicators(dt)
  expect_true(all(dt$pop_in_poverty >= 0, na.rm = TRUE))
})


# ===========================================================================
# get_additional_indicators_grp()
# ===========================================================================

test_that("get_additional_indicators_grp: returns TRUE invisibly", {
  dt <- .make_grp_dt()
  result <- get_additional_indicators_grp(dt)
  expect_true(result)
})

test_that("get_additional_indicators_grp: adds expected new columns", {
  dt <- .make_grp_dt()
  get_additional_indicators_grp(dt)
  expected_cols <- c(
    "pop_in_poverty", "average_shortfall",
    "total_shortfall", "income_gap_ratio"
  )
  expect_true(all(expected_cols %in% names(dt)))
})

test_that("get_additional_indicators_grp: does NOT add palma_ratio or p90p10_ratio", {
  dt <- .make_grp_dt()
  get_additional_indicators_grp(dt)
  expect_false("palma_ratio"  %in% names(dt))
  expect_false("p90p10_ratio" %in% names(dt))
})

test_that("get_additional_indicators_grp: new_indicators_names attribute is set", {
  dt <- .make_grp_dt()
  get_additional_indicators_grp(dt)
  new_names <- attr(dt, "new_indicators_names")
  expect_false(is.null(new_names))
  expect_true(length(new_names) > 0L)
})
