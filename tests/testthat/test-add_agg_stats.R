res_ex1 <- readRDS('../testdata/agg-stats-ex-1.rds')
res_ex2 <- readRDS('../testdata/agg-stats-ex-2.rds')

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
