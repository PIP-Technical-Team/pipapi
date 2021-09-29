res_ex1 <- readRDS("../testdata/agg-stats-ex-1.rds")
res_ex2 <- readRDS("../testdata/agg-stats-ex-2.rds")
res_ex3 <- readRDS("../testdata/povcal_response_ind1988.rds")

test_that("add_agg_stats() works", {

  # Check that Watts is set to NA if either U/R watts is not above zero
  expect_equal(res_ex1$watts[1], 0)
  tmp <- add_agg_stats(res_ex1)
  expect_true(is.na(tmp$watts[3]))

  # Note: Wasn't able to trigger poverty_severity statements
  # with real data, so created dummy examples

  # If rural poverty_severity > 0
  res_ex3 <- res_ex2
  res_ex3$poverty_severity[1] <- -0.5
  tmp <- add_agg_stats(res_ex3)
  expect_equal(tmp$headcount[2], tmp$headcount[3])

  # If urban poverty_severity > 0
  res_ex3 <- res_ex2
  res_ex3$poverty_severity[2] <- -0.5
  tmp <- add_agg_stats(res_ex3)
  expect_equal(tmp$headcount[1], tmp$headcount[3])

  # If both urban and rural poverty_severity > 0
  res_ex3 <- res_ex2
  res_ex3$poverty_severity <- -0.5
  tmp <- add_agg_stats(res_ex3)
  expect_true(is.na(tmp$headcount[3]))
})

test_that("ag_average_poverty_stats() works", {

  tmp <- ag_average_poverty_stats(res_ex3)

  # Benchmark values from PovcalNet API as of 20210929
  # http://iresearch.worldbank.org/povcalnet/povcalnetapi.ashx?YearSelected=1988&Countries=IND_5,IND_1,IND_2&PovertyLine=1.9&display=C&format=csv
  expect_equal(tmp$survey_mean_ppp, 72.0616244493633, tolerance = 1.490116e-07)
  expect_equal(tmp$headcount, 0.5019447, tolerance = 1.490116e-07)
  expect_equal(tmp$poverty_gap, 0.14287220, tolerance = 1.490116e-07)
  expect_equal(tmp$poverty_severity, 0.05508484, tolerance = 1.490116e-07)
  expect_equal(tmp$watts, 0.1849849, tolerance = 1.490116e-07)
})
