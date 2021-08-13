lkups <- pipapi:::clean_api_data(
  data_folder_root = Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))

test_that("add_agg_stats() works", {

  # Check that Watts is set to NA if either U/R watts is not above zero
  res <- rg_pip(country = 'CHN',
                year = '1996',
                povline = 0.05,
                welfare_type = 'all',
                aggregate = FALSE,
                reporting_level = 'all',
                ppp = NULL,
                popshare = NULL,
                lkup = lkups)
  expect_equal(res$watts[1], 0)
  tmp <- add_agg_stats(res)
  expect_true(is.na(tmp$watts[3]))

  # Note: Wasn't able to trigger poverty_severity statements
  # with real data, so created dummy examples
  res <- rg_pip(country = 'CHN',
                year = '1996',
                povline = 1.9,
                welfare_type = 'all',
                aggregate = FALSE,
                reporting_level = 'all',
                ppp = NULL,
                popshare = NULL,
                lkup = lkups)

  # If rural poverty_severity > 0
  res2 <- res
  res2$poverty_severity[1] <- -0.5
  tmp <- add_agg_stats(res2)
  expect_equal(tmp$headcount[2], tmp$headcount[3])

  # If urban poverty_severity > 0
  res2 <- res
  res2$poverty_severity[2] <- -0.5
  tmp <- add_agg_stats(res2)
  expect_equal(tmp$headcount[1], tmp$headcount[3])

  # If both urban and rural poverty_severity > 0
  res2 <- res
  res2$poverty_severity <- -0.5
  tmp <- add_agg_stats(res2)
  expect_true(is.na(tmp$headcount[3]))

})
