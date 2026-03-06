# test-compute_fgt.R
#
# Unit tests for compute_fgt(), compute_fgt_dt(), and process_dt().
# All synthetic data — no external files required.

library(data.table)


# Helper: expected FGT values -----------------------------------------------
# For a uniform distribution welfare = 1:10, weight = rep(1, 10), povline = 5:
#   - poor individuals: welfare 1, 2, 3, 4  (below strict threshold)
#   - headcount  = 4/10  = 0.4
#   - poverty gap= mean((5 - 1:4)/5) / 1   ... weighted average over all obs
#                = sum((5-1:4)/5) / 10 = (4/5+3/5+2/5+1/5)/10 = 2/10 = 0.2
#   - fgt2       = sum(((5-1:4)/5)^2) / 10
#                = ((0.8^2 + 0.6^2 + 0.4^2 + 0.2^2)) / 10
#                = (0.64 + 0.36 + 0.16 + 0.04) / 10 = 1.2/10 = 0.12
#   - watts      = sum(log(5/1:4)) / 10

.wf <- 1:10
.wt <- rep(1, 10)
.pl <- 5

.expected_hc  <- 4 / 10
.expected_pg  <- sum((5 - 1:4) / 5) / 10
.expected_fgt2 <- sum(((5 - 1:4) / 5)^2) / 10
.expected_watts <- sum(log(5 / (1:4))) / 10


# compute_fgt() ---------------------------------------------------------------

test_that("compute_fgt returns a list with named elements", {
  res <- compute_fgt(.wf, .wt, .pl)
  expect_type(res, "list")
  expect_named(res, c("headcount", "poverty_gap", "poverty_severity",
                       "watts", "povline"), ignore.order = TRUE)
})

test_that("compute_fgt headcount is correct for simple uniform case", {
  res <- compute_fgt(.wf, .wt, .pl)
  expect_equal(res$headcount, .expected_hc, tolerance = 1e-9)
})

test_that("compute_fgt poverty_gap is correct for simple uniform case", {
  res <- compute_fgt(.wf, .wt, .pl)
  expect_equal(res$poverty_gap, .expected_pg, tolerance = 1e-9)
})

test_that("compute_fgt poverty_severity is correct for simple uniform case", {
  res <- compute_fgt(.wf, .wt, .pl)
  expect_equal(res$poverty_severity, .expected_fgt2, tolerance = 1e-9)
})

test_that("compute_fgt watts is correct for simple uniform case", {
  res <- compute_fgt(.wf, .wt, .pl)
  expect_equal(res$watts, .expected_watts, tolerance = 1e-9)
})

test_that("compute_fgt headcount = 0 when all welfare above poverty line", {
  res <- compute_fgt(rep(20, 5), rep(1, 5), povlines = 10)
  expect_equal(res$headcount, 0)
  expect_equal(res$poverty_gap, 0)
  expect_equal(res$watts, 0)
})

test_that("compute_fgt headcount = 1 when all welfare below poverty line", {
  res <- compute_fgt(rep(1, 5), rep(1, 5), povlines = 10)
  expect_equal(res$headcount, 1)
})

test_that("compute_fgt respects weights (all poor but one has zero weight)", {
  # welfare = c(1, 20), weights = c(1, 0) -> 100% poor
  res <- compute_fgt(c(1, 20), c(1, 0), povlines = 5)
  expect_equal(res$headcount, 1)
})


# compute_fgt_dt() ------------------------------------------------------------

test_that("compute_fgt_dt returns a data.table", {
  dt <- data.table(
    welfare          = 1:10,
    weight           = rep(1, 10),
    reporting_level  = "national",
    file             = "TST_2000"
  )
  res <- compute_fgt_dt(dt, welfare = "welfare", weight = "weight",
                        povlines = 5)
  expect_s3_class(res, "data.table")
})

test_that("compute_fgt_dt result has expected FGT columns", {
  dt <- data.table(
    welfare         = 1:10,
    weight          = rep(1, 10),
    reporting_level = "national",
    file            = "TST_2000"
  )
  res <- compute_fgt_dt(dt, welfare = "welfare", weight = "weight",
                        povlines = 5)
  expect_true(all(c("headcount", "poverty_gap", "poverty_severity", "watts")
                  %in% names(res)))
})

test_that("compute_fgt_dt headcount matches hand-calculated value", {
  dt <- data.table(
    welfare         = .wf,
    weight          = .wt,
    reporting_level = "national",
    file            = "TST_2000"
  )
  res <- compute_fgt_dt(dt, welfare = "welfare", weight = "weight",
                        povlines = .pl)
  expect_equal(res$headcount, .expected_hc, tolerance = 1e-9)
})

test_that("compute_fgt_dt mean_and_med=TRUE adds mean and median columns", {
  dt <- data.table(
    welfare         = .wf,
    weight          = .wt,
    reporting_level = "national",
    file            = "TST_2000",
    mean           = 5.5,   # pre-computed mean required by mean_and_med=TRUE
    median         = 5.0,
    country_code   = "TST",
    reporting_year = 2000L
  )
  res <- compute_fgt_dt(dt, welfare = "welfare", weight = "weight",
                        povlines = .pl, mean_and_med = TRUE)
  expect_true(all(c("mean", "median") %in% names(res)))
})

test_that("compute_fgt_dt handles multiple poverty lines", {
  dt <- data.table(
    welfare         = .wf,
    weight          = .wt,
    reporting_level = "national",
    file            = "TST_2000"
  )
  res <- compute_fgt_dt(dt, welfare = "welfare", weight = "weight",
                        povlines = c(3, 5, 7))
  expect_equal(nrow(res), 3L)
  expect_equal(sort(res$povline), c(3, 5, 7))
})


# process_dt() ----------------------------------------------------------------

test_that("process_dt returns a data.table with expected columns", {
  dt <- data.table(
    welfare         = rep(1:10, 2),
    weight          = rep(1, 20),
    reporting_level = rep(c("national", "urban"), each = 10),
    file            = rep(c("AAA_2000", "AAA_2000"), each = 10)
  )
  res <- process_dt(dt, povline = 5)
  expect_s3_class(res, "data.table")
  expect_true("headcount" %in% names(res))
})

test_that("process_dt groups by id_var and reporting_level", {
  dt <- data.table(
    welfare         = c(1:5, 6:10),
    weight          = rep(1, 10),
    reporting_level = rep("national", 10),
    file            = c(rep("AAA_2000", 5), rep("BBB_2010", 5))
  )
  res <- process_dt(dt, povline = 4)
  expect_equal(nrow(res), 2L)
  expect_true(all(c("AAA_2000", "BBB_2010") %in% res[["file"]]))
})

test_that("process_dt output povline equals input povline", {
  dt <- data.table(
    welfare         = .wf,
    weight          = .wt,
    reporting_level = "national",
    file            = "TST_2000"
  )
  res <- process_dt(dt, povline = .pl)
  expect_equal(unique(res$povline), .pl)
})
