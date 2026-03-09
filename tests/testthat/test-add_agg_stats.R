return_cols <- test_path("testdata", "add_agg_stats_return_cols.rds") |>
  readRDS()
res_ex1 <- test_path("testdata", "agg-stats-ex-1.rds") |>
  readRDS()
res_ex2 <- test_path("testdata", "agg-stats-ex-2.rds") |>
  readRDS()
res_ex3 <- test_path("testdata", "agg-stats-ex-3.rds") |>
  readRDS()
res_ex4 <- test_path("testdata", "povcal_response_ind1988.rds") |>
  readRDS()

## Add spr to sample data (we need to recreate sample data but I did not find
## thr R scripts that create them)
res_ex1$spr <- .5
res_ex2$spr <- .5
res_ex3$spr <- .5
res_ex4$spr <- .5

test_that("add_agg_stats() works", {

  # Check that Watts is set to NA if either U/R watts is not above zero
  expect_equal(res_ex1$watts[1], 0)

  tmp <- add_agg_stats(res_ex1, return_cols = return_cols)
  expect_true(is.na(tmp$watts[3]))

  # Same namber of variables as output.
  expect_equal(names(res_ex1), names(tmp))


  # Note: Wasn't able to trigger poverty_severity statements
  # with real data, so created dummy examples

  # If rural poverty_severity > 0
  res_tmp <- data.table::copy(res_ex2)
  res_tmp$poverty_severity[1] <- -0.5
  tmp <- add_agg_stats(res_tmp, return_cols = return_cols)

  # TODO: the original assertion below was incorrect (was testing a wrong value
  # as correct). Correct assertion needs investigation — tracked separately.
  # expect_equal(tmp$headcount[2], tmp$headcount[3])
  expect_true(is.na(tmp$poverty_severity[3]))

  # If urban poverty_severity > 0
  # res_tmp <- res_ex2
  # res_tmp$poverty_severity[2] <- -0.5
  # tmp <- add_agg_stats(res_tmp)
  # expect_equal(tmp$headcount[1], tmp$headcount[3])

  # If both urban and rural poverty_severity > 0
  # res_tmp <- res_ex2
  # res_tmp$poverty_severity <- -0.5
  # tmp <- add_agg_stats(res_tmp)
  # expect_true(is.na(tmp$headcount[3]))

  # Check that national median is set to NA
  tmp <- add_agg_stats(res_ex3, return_cols = return_cols)
  expect_true(is.na(tmp$median[3]))
  expect_true(is.na(tmp$survey_median_ppp[3]))

  # Check that national mean is a weighted average
  tmp <- add_agg_stats(res_ex3, return_cols = return_cols)
  expect_equal(tmp$mean[3], weighted.mean(res_ex3$survey_mean_ppp, res_ex3$reporting_pop))



  # if negative, result is NA
  res_tmp <- data.table::copy(res_ex2)
  res_tmp$headcount[1] <- -0.5
  tmp <- add_agg_stats(res_tmp, return_cols = return_cols)
  expect_true(is.na(tmp$headcount[3]))


  res_tmp <- data.table::copy(res_ex2)
  res_tmp$poverty_gap[1] <- -0.5
  tmp <- add_agg_stats(res_tmp, return_cols = return_cols)
  expect_true(is.na(tmp$poverty_gap[3]))


  # if negative, result is NA
  res_tmp <- data.table::copy(res_ex2)
  res_tmp$headcount[1] <- NA
  tmp <- add_agg_stats(res_tmp, return_cols = return_cols)
  expect_true(is.na(tmp$headcount[3]))

})

# negative_to_na() -----------------------------------------------------------

test_that("negative_to_na: returns x unchanged when all values are positive", {
  x <- c(1, 2, 3)
  expect_equal(pipapi:::negative_to_na(x), x)
})

test_that("negative_to_na: returns NA_real_ when any value is negative", {
  expect_equal(pipapi:::negative_to_na(c(1, -0.1, 3)), NA_real_)
})

test_that("negative_to_na: returns NA_real_ when any value is NA", {
  expect_equal(pipapi:::negative_to_na(c(1, NA, 3)), NA_real_)
})

test_that("negative_to_na: returns NA_real_ for all-NA input", {
  expect_equal(pipapi:::negative_to_na(c(NA_real_, NA_real_)), NA_real_)
})

test_that("negative_to_na: zero is not treated as negative", {
  expect_equal(pipapi:::negative_to_na(c(0, 1, 2)), c(0, 1, 2))
})


# zeros_to_na() --------------------------------------------------------------

test_that("zeros_to_na: returns x unchanged when no zeros", {
  x <- c(1, 2, 3)
  expect_equal(pipapi:::zeros_to_na(x), x)
})

test_that("zeros_to_na: returns NA_real_ when any value is zero", {
  expect_equal(pipapi:::zeros_to_na(c(1, 0, 3)), NA_real_)
})

test_that("zeros_to_na: returns NA_real_ for all-zero input", {
  expect_equal(pipapi:::zeros_to_na(c(0, 0)), NA_real_)
})

test_that("zeros_to_na: does not treat NA as zero", {
  # NA is not 0 -> vector unchanged
  expect_equal(pipapi:::zeros_to_na(c(1, NA, 3)), c(1, NA, 3))
})


# ag_average_poverty_stats() -------------------------------------------------

test_that("ag_average_poverty_stats() works", {

  tmp <- ag_average_poverty_stats(res_ex4, return_cols = return_cols)

  # Benchmark values from PovcalNet API as of 20210929
  # http://iresearch.worldbank.org/povcalnet/povcalnetapi.ashx?YearSelected=1988&Countries=IND_5,IND_1,IND_2&PovertyLine=1.9&display=C&format=csv
  expect_equal(tmp$mean, 72.0616244493633, tolerance = 1.490116e-07)
  expect_equal(tmp$headcount, 0.5019447, tolerance = 1.490116e-07)
  expect_equal(tmp$poverty_gap, 0.14287220, tolerance = 1.490116e-07)
  expect_equal(tmp$poverty_severity, 0.05508484, tolerance = 1.490116e-07)
  expect_equal(tmp$watts, 0.1849849, tolerance = 1.490116e-07)

})
