# constants
censored <-
  test_path("testdata", "/censored.rds") |>
  readRDS()

censored2 <-
  test_path("testdata", "/censored-2.rds") |>
  readRDS()

reg_agg <-
  test_path("testdata", "/ohi-sample.rds") |>
  readRDS()

chn <-
  test_path("testdata", "/chn-2016.rds") |>
  readRDS()

test_that("censor_rows() removes entire row when statistic is 'all'", {

  # Country table
  res <- censor_rows(chn, censored, type = "countries")
  expect_equal(nrow(res), 0)
  expect_equal(names(chn), names(res))

  # Region table
  res <- censor_rows(reg_agg, censored, type = "regions")
  expect_equal(nrow(res), 3)
  expect_false(all(censored$region$reporting_year %in%
                     res$reporting_year))

  expect_equal(reg_agg$reporting_pop[1:3], res$reporting_pop[1:3])
  expect_equal(reg_agg$headcount[1:3], res$headcount[1:3])
  expect_equal(reg_agg$poverty_gap[1:3], res$poverty_gap[1:3])
  expect_equal(reg_agg$poverty_severity[1:3], res$poverty_severity[1:3])
  expect_equal(reg_agg$watts[1:3], res$watts[1:3])
  expect_equal(reg_agg$mean[1:3], res$mean[1:3])
  expect_equal(reg_agg$pop_in_poverty[1:3], res$pop_in_poverty[1:3])

})

test_that("censor_rows() sets specific stats to NA_real_", {

  # Country table
  res <- censor_rows(chn, censored2, type = "countries")
  expect_equal(nrow(res), 3)

  # Check that stats are correctly set to NA
  expect_equal(unique(res$headcount), NA_real_)
  expect_equal(unique(res$gini), NA_real_)
  expect_equal(unique(res$mld), NA_real_)

  # Check that other stats idn't change
  expect_equal(res[,!c("headcount", "mld", "gini")],
               chn[,!c("headcount","mld", "gini")])

  # Region table
  res <- censor_rows(reg_agg, censored2, type = "regions")
  expect_equal(nrow(res), 5)

  # Check that stats are correctly set to NA
  expect_equal(unique(res$watts[4:5]), NA_real_)
  expect_equal(unique(res$headcount[4:5]), NA_real_)
  expect_equal(unique(res$mean[4:5]), NA_real_)

  # Check that other stats (for same indicators) didn't change
  expect_equal(reg_agg$headcount[1:3], res$headcount[1:3])
  expect_equal(reg_agg$watts[1:3], res$watts[1:3])
  expect_equal(reg_agg$mean[1:3], res$mean[1:3])

  # Check that other stats (for other indicators) didn't change
  expect_equal(reg_agg$reporting_pop[1:5], res$reporting_pop[1:5])
  expect_equal(reg_agg$poverty_gap[1:5], res$poverty_gap[1:5])
  expect_equal(reg_agg$poverty_severity[1:5], res$poverty_severity[1:5])
  expect_equal(reg_agg$pop_in_poverty[1:5], res$pop_in_poverty[1:5])
})

test_that("censor_rows() returns early when there no censoring observations", {
  tmp <- list(region = data.frame(
    region_code = character(0),
    reporting_year = numeric(0),
    statistic = character(0)
  ))
  res <- censor_rows(reg_agg, tmp, type = "regions")
  expect_equal(res, reg_agg)
})
