# Constants
censored_table <- data.frame(
  country_code = c(rep('XYZ', 3), 'WLD'),
  survey_acronym = c(rep('HBS', 3), NA),
  reporting_year = c(2000, 2000, 2008, 2018),
  welfare_type = c(rep('consumption', 3), NA),
  statistic = c('mld', 'gini', 'median', 'headcount')
)
censored_table$id <-
  sprintf('%s_%s_%s',
          censored_table$country_code,
          censored_table$reporting_year,
          censored_table$welfare_type)
df <- data.frame(
  country_code = c(rep('XYZ', 3), 'WLD'),
  survey_acronym = c(rep('HBS', 3), NA),
  reporting_year = c(2000, 2008, 2018, 2018),
  welfare_type = c(rep('consumption', 3), NA),
  median = c(2.5, 3, 2, 10),
  gini = c(0.5, 0.4, 0.45, 0.4),
  polarization = c(0.5, 0.4, 0.45, 0.5),
  mld = c(0.5, 0.4, 0.45, 0.51),
  headcount = c(3, 2.5, 2, 400)
)

test_that("collapse_rows() works correctly", {
  res <- censor_rows(df, censored_table)
  expect_identical(dim(res), dim(df))
  expect_equal(res$gini, c(NA, 0.40, 0.45, 0.40))
  expect_equal(res$median, c(2.5, NA, 2.0, 10.0))
  expect_equal(res$mld, c(NA, 0.4, 0.45, 0.51))
  expect_equal(res$headcount, c(3, 2.5, 2, NA))
  expect_equal(res$polarization, c(0.5, 0.4, 0.45, 0.50))
})
