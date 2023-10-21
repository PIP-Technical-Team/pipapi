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

  # This test is wrong. It is testing as correct something that should
  # not be the case.
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

test_that("all objects are correctly passed and used", {
  root <- rprojroot::is_r_package
  tmp <- lintr::lint(filename = root$find_file("R/add_agg_stats.R"),
                     linters = lintr::object_usage_linter())

  expect_equal(length(tmp), 0)
})
