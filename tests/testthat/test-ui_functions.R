# constants
lkups <- pipapi:::clean_api_data(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))
lkups$pl_lkup <- lkups$pl_lkup[sample(nrow(lkups$pl_lkup), 10)]

dt_lac <- readRDS('../testdata/pip_lac_resp.RDS')
dt_sas <- readRDS('../testdata/pip_sas_resp.RDS')


test_that("ui_cp_poverty_charts() works as expected", {
  dl <- ui_cp_poverty_charts(country = 'AGO',
                             povline = 1.9,
                             year_range = 2016:2019,
                             pop_units = 1e6, lkup = lkups)
  expect_identical(names(dl), c('pov_trend', 'pov_mrv'))
})

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

  # Less than 12 countries in original response
  res <- cp_pov_mrv_select_countries(dt_sas, 'PAK')
  expect_equal(res, dt_sas[order(headcount)])

})

test_that("ui_cp_ki_headcount() works as expected", {

  df <- ui_cp_ki_headcount(country = 'AGO', povline = 1.9, lkup = lkups)
  expect_identical(names(df), c('country_code', 'reporting_year',
                                'poverty_line', 'headcount'))

})

test_that("ui_cp_key_indicators() works as expected", {

  # A single poverty line
  dl <- ui_cp_key_indicators(country = 'AGO', povline = 1.9, lkup = lkups)
  expect_identical(names(dl),
                   c('headcount', 'headcount_national', 'mpm_headcount',
                     'population', 'gni', 'gdp', 'shared_prosperity'))
  expect_identical(names(dl$headcount), '1.9')

  # All poverty lines
  dl <- ui_cp_key_indicators(country = 'AGO', lkup = lkups)
  expect_identical(names(dl),
                   c('headcount', 'headcount_national', 'mpm_headcount',
                     'population', 'gni', 'gdp', 'shared_prosperity'))
  expect_identical(names(dl$headcount), lkups$pl_lkup$name)

})

test_that("ui_cp_charts() works as expected", {

  # A single poverty line
  dl <- ui_cp_charts(country = 'AGO', povline = 1.9, lkup = lkups)
  expect_identical(names(dl), c('pov_charts', 'ineq_trend',
                                'ineq_bar', 'mpm', 'sp'))
  expect_identical(names(dl$pov_charts), '1.9')
  expect_identical(names(dl$pov_charts$`1.9`), c('pov_trend', 'pov_mrv'))

  # All poverty lines
  dl <- ui_cp_charts(country = 'AGO', lkup = lkups)
  expect_identical(names(dl), c('pov_charts', 'ineq_trend',
                                'ineq_bar', 'mpm', 'sp'))
  expect_identical(names(dl$pov_charts), lkups$pl_lkup$name)
  expect_identical(names(dl$pov_charts[[1]]), c('pov_trend', 'pov_mrv'))

})




