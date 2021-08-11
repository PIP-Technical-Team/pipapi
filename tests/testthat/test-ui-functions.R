# constants
dt_lac <- readRDS('../testdata/pip_lac_resp.RDS')
dt_sas <- readRDS('../testdata/pip_sas_resp.RDS')

test_that("cp_pov_mrv_select_countries() works as expected", {

  # Selected country pertains to top 5
  res <- cp_pov_mrv_select_countries(dt_lac, 'COL')
  expect_equal(nrow(res), 11)
  expect_identical(res$country_code,
                   c('URY', 'CHL', 'DOM', 'PRY', 'CRI', 'BOL',
                     'ECU', 'LCA', 'BRA', 'COL', 'HND'))
  expect_true(!is.unsorted(res$headcount))

  # Selected country does not pertain to top 5 or bottom 5
  res <- cp_pov_mrv_select_countries(dt_lac, 'MEX')
  expect_equal(nrow(res), 11)
  expect_identical(res$country_code,
                   c('URY', 'CHL', 'DOM', 'SLV', 'ARG', 'MEX',
                     'PER', 'BOL', 'BRA', 'COL', 'HND'))
  expect_true(!is.unsorted(res$headcount))

  # Selected country does not pertain to bottom 5
  res <- cp_pov_mrv_select_countries(dt_lac, 'CHL')
  expect_equal(nrow(res), 11)
  expect_identical(res$country_code,
                   c('URY', 'CHL', 'DOM', 'PRY', 'CRI', 'PAN',
                     'ECU', 'LCA', 'BRA', 'COL', 'HND'))
  expect_true(!is.unsorted(res$headcount))

  # Less than 12 countries in orginal response
  res <- cp_pov_mrv_select_countries(dt_sas, 'PAK')
  expect_equal(res, dt_sas[order(headcount)])

})
