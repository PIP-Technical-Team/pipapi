# constants
lkups <- pipapi:::clean_api_data(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))
lkups$pl_lkup <- lkups$pl_lkup[sample(nrow(lkups$pl_lkup), 10)]

dt_lac <- readRDS('../testdata/pip_lac_resp.RDS')
dt_sas <- readRDS('../testdata/pip_sas_resp.RDS')

test_that("ui_hp_stacked() works as expected", {
  lkups2 <- lkups
  lkups2$ref_lkup <- lkups2$ref_lkup[country_code == 'CHN']
  res <- ui_hp_stacked(povline = 1.9, lkup = lkups2)
  expect_identical(names(res),
                   c("region_code", "reporting_year",
                     "poverty_line", "pop_in_poverty"))
  expect_identical(unique(res$region_code), c('EAP', 'WLD'))
})

test_that("ui_hp_countries() works as expected", {
  res <- ui_hp_countries(country = c("AGO", "CIV"), povline = 1.9, lkup = lkups)
  expect_identical(names(res),
                   c("region_code", "country_code",
                     "reporting_year", "poverty_line",
                     "reporting_pop", "pop_in_poverty"))
  expect_true(all(res$pop_in_poverty < 50))
  check <- lkups$svy_lkup[country_code %in% c("AGO", "CIV")]$reporting_year
  expect_equal(res$reporting_year, check)
})

test_that("ui_pc_charts() works as expected", {

  # Regular query (fill_gaps = FALSE)
  res <- ui_pc_charts(country = "AGO", povline = 1.9, lkup = lkups)
  expect_equal(nrow(res), nrow(lkups$svy_lkup[country_code == "AGO"]))
  expect_equal(length(names(res)), 36)

  # Regular query (fill_gaps = TRUE)
  res <- ui_pc_charts(country = "AGO", povline = 1.9, fill_gaps = TRUE, lkup = lkups)
  expect_equal(nrow(res), length(unique(lkups$ref_lkup$reporting_year)))
  expect_equal(length(names(res)), 11)

  # Group by
  res <- ui_pc_charts(country = "AGO", group_by = "wb", povline = 1.9, lkup = lkups)
  res2 <- pip(country = "AGO", group_by = "wb", povline = 1.9, lkup = lkups)
  expect_equal(res, res2)

})

test_that("ui_cp_poverty_charts() works as expected", {
  dl <- ui_cp_poverty_charts(country = 'AGO',
                             povline = 1.9,
                             pop_units = 1e6,
                             lkup = lkups)
  expect_identical(names(dl), c('pov_trend', 'pov_mrv'))
})

test_that("cp_pov_mrv_select_values() works as expected", {

  # Top 5
  set.seed(42); v <- round(runif(20), 5); h <- 0.91481
  out <- cp_pov_mrv_select_values(v, h)
  v <- sort(v)
  expect_equal(out, c(v[1:5], v[15:20]))

  # Bottom 5
  set.seed(42); v <- round(runif(20), 5); h <- 0.25543
  out <- cp_pov_mrv_select_values(v, h)
  v <- sort(v)
  expect_equal(out, c(v[1:6], v[16:20]))

  # Neither bottom 5 nor top 5
  set.seed(42); v <- round(runif(20), 5); h <- 0.56033
  out <- cp_pov_mrv_select_values(v, h)
  v <- sort(v); x <- which(v == h)
  expect_equal(out, c(v[1:3], v[(x - 2):(x + 2)], v[18:20]))

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
