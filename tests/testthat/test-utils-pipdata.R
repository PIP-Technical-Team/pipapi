# test-utils-pipdata.R
#
# Unit tests for attribute-helper functions in utils-pipdata.R.
# All synthetic inline data — no external files required.

library(data.table)


# Helper: build a minimal survey data.table with all needed attributes --------

.make_svy_dt <- function(
  n_rural = 4L,
  n_urban = 6L,
  country_code   = "TST",
  reporting_year = 2000L,
  dist_stats = NULL
) {
  n <- n_rural + n_urban
  dt <- data.table(
    welfare = seq_len(n),
    weight  = rep(1L, n)
  )
  attr(dt, "reporting_level_rows") <- list(
    reporting_level = c("rural", "urban"),
    rows            = c(n_rural, n_rural + n_urban)
  )
  attr(dt, "country_code")   <- country_code
  attr(dt, "reporting_year") <- reporting_year
  attr(dt, "dist_stats")     <- dist_stats
  dt
}


# add_attributes_as_columns_vectorized() -------------------------------------

test_that("add_attributes_as_columns_vectorized adds reporting_level column", {
  dt  <- .make_svy_dt()
  out <- add_attributes_as_columns_vectorized(dt)
  expect_true("reporting_level" %in% names(out))
})

test_that("reporting_level values match segment specification", {
  dt  <- .make_svy_dt(n_rural = 4L, n_urban = 6L)
  out <- add_attributes_as_columns_vectorized(dt)
  expect_equal(out$reporting_level, c(rep("rural", 4L), rep("urban", 6L)))
})

test_that("country_code and reporting_year columns are added", {
  dt  <- .make_svy_dt(country_code = "XYZ", reporting_year = 2010L)
  out <- add_attributes_as_columns_vectorized(dt)
  expect_true(all(out$country_code == "XYZ"))
  expect_true(all(out$reporting_year == 2010L))
})

test_that("file column is paste0(country_code, '_', reporting_year)", {
  dt  <- .make_svy_dt(country_code = "XYZ", reporting_year = 2010L)
  out <- add_attributes_as_columns_vectorized(dt)
  expect_true(all(out$file == "XYZ_2010"))
})

test_that("mean and median columns added when dist_stats is provided", {
  ds <- list(
    mean   = list(rural = 1.5, urban = 4.0),
    median = list(rural = 1.2, urban = 3.8)
  )
  dt  <- .make_svy_dt(dist_stats = ds)
  out <- add_attributes_as_columns_vectorized(dt)
  expect_true(all(c("mean", "median") %in% names(out)))
  expect_equal(out$mean[1], 1.5)       # first rural row
  expect_equal(out$mean[5], 4.0)       # first urban row
})

test_that("add_attributes_as_columns_vectorized aborts when row counts mismatch", {
  dt <- data.table(welfare = 1:5, weight = rep(1, 5))
  attr(dt, "reporting_level_rows") <- list(
    reporting_level = "national",
    rows            = 10L           # 10 != nrow(dt) = 5
  )
  attr(dt, "country_code")   <- "TST"
  attr(dt, "reporting_year") <- 2000L
  expect_error(
    add_attributes_as_columns_vectorized(dt),
    class = "rlang_error"
  )
})


# add_attributes_as_columns_multi() ------------------------------------------

test_that("add_attributes_as_columns_multi adds reporting_level column", {
  dt  <- .make_svy_dt()
  out <- add_attributes_as_columns_multi(dt)
  expect_true("reporting_level" %in% names(out))
})

test_that("add_attributes_as_columns_multi correct segment values", {
  dt  <- .make_svy_dt(n_rural = 3L, n_urban = 5L)
  out <- add_attributes_as_columns_multi(dt)
  expect_equal(out$reporting_level, c(rep("rural", 3L), rep("urban", 5L)))
})

test_that("add_attributes_as_columns_multi aborts on missing attribute", {
  dt <- data.table(welfare = 1:5)
  expect_error(
    add_attributes_as_columns_multi(dt),
    class = "rlang_error"
  )
})

test_that("add_attributes_as_columns_multi aborts when rows length != level length", {
  dt <- data.table(welfare = 1:6)
  attr(dt, "reporting_level_rows") <- list(
    reporting_level = c("rural", "urban"),
    rows            = c(3L)          # length 1 vs 2 levels
  )
  attr(dt, "country_code")   <- "TST"
  attr(dt, "reporting_year") <- 2000L
  expect_error(
    add_attributes_as_columns_multi(dt),
    class = "rlang_error"
  )
})

test_that("add_attributes_as_columns_multi aborts when last row != nrow(dt)", {
  dt <- data.table(welfare = 1:6)
  attr(dt, "reporting_level_rows") <- list(
    reporting_level = c("rural", "urban"),
    rows            = c(3L, 8L)   # 8 != 6
  )
  attr(dt, "country_code")   <- "TST"
  attr(dt, "reporting_year") <- 2000L
  expect_error(
    add_attributes_as_columns_multi(dt),
    class = "rlang_error"
  )
})


# assign_stat() ---------------------------------------------------------------

test_that("assign_stat broadcasts a scalar to all rows", {
  dt <- data.table(x = 1:5)
  assign_stat(dt, lev = rep("national", 5), counts = rep(1L, 5),
              stat = 3.14, colname = "mean")
  expect_true(all(dt$mean == 3.14))
})

test_that("assign_stat maps named list to levels", {
  dt <- data.table(x = 1:6)
  assign_stat(dt,
              lev    = c("rural", "urban"),
              counts = c(3L, 3L),
              stat   = list(rural = 1.0, urban = 5.0),
              colname = "mean")
  expect_equal(dt$mean[1:3], rep(1.0, 3))
  expect_equal(dt$mean[4:6], rep(5.0, 3))
})

test_that("assign_stat with NULL stat leaves column untouched", {
  dt <- data.table(x = 1:3)
  result <- assign_stat(dt, lev = "national", counts = 3L,
                        stat = NULL, colname = "mean")
  expect_false("mean" %in% names(dt))
  expect_identical(result, dt)
})

test_that("assign_stat aborts when stat has no names and length > 1", {
  dt <- data.table(x = 1:4)
  expect_error(
    assign_stat(dt,
                lev    = c("rural", "urban"),
                counts = c(2L, 2L),
                stat   = c(1.0, 5.0),  # unnamed, length > 1
                colname = "mean"),
    regexp = "names"
  )
})

test_that("assign_stat accepts named vector (not list)", {
  dt <- data.table(x = 1:4)
  assign_stat(dt,
              lev    = c("rural", "urban"),
              counts = c(2L, 2L),
              stat   = c(rural = 2.0, urban = 8.0),
              colname = "mean")
  expect_equal(dt$mean[1:2], rep(2.0, 2))
  expect_equal(dt$mean[3:4], rep(8.0, 2))
})

test_that("assign_stat aborts when a level is missing from stat names", {
  dt <- data.table(x = 1:4)
  expect_error(
    assign_stat(dt,
                lev    = c("rural", "urban"),
                counts = c(2L, 2L),
                stat   = list(rural = 1.0),  # urban missing
                colname = "mean"),
    regexp = "missing"
  )
})
