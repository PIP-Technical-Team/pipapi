test_that("ui_cp_download uses only the precomputed Country Profile Gini", {
  countries <- c("MATCH", "HC_ONLY", "FLAT_ONLY", "CANONICAL_NA")

  hc <- data.table::data.table(
    country_code = c("MATCH", "HC_ONLY", "CANONICAL_NA"),
    reporting_year = c(2020L, 2021L, 2022L),
    poverty_line = 1.9,
    headcount = c(0.1, 0.2, 0.3),
    gini = c(0.11, 0.22, 0.33)
  )

  flat_cp <- data.table::data.table(
    country_code = c("MATCH", "FLAT_ONLY", "CANONICAL_NA"),
    reporting_year = c(2020L, 2023L, 2022L),
    gini = c(0.44, 0.55, NA_real_),
    headcount_national = c(10, 20, 30)
  )

  lkup <- list(
    svy_lkup = data.table::data.table(
      country_code = countries,
      display_cp = 1L
    ),
    cp_lkups = list(flat = list(flat_cp = flat_cp))
  )

  local_mocked_bindings(
    ui_cp_ki_headcount = function(country, year, povline, lkup) {
      hc[country_code == country]
    },
    .package = "pipapi"
  )

  out <- ui_cp_download(country = "ALL", povline = 1.9, lkup = lkup)

  expect_equal(out[country_code == "MATCH", gini], 0.44)
  expect_true(is.na(out[country_code == "CANONICAL_NA", gini]))
  expect_true(is.na(out[country_code == "HC_ONLY", gini]))
  expect_equal(out[country_code == "FLAT_ONLY", gini], 0.55)

  expect_equal(sum(names(out) == "gini"), 1L)
  expect_false(any(c("gini.x", "gini.y") %in% names(out)))
  expect_setequal(out$country_code, countries)
  expect_equal(nrow(out), 4L)

  expect_equal(out[country_code == "MATCH", headcount], 0.1)
  expect_equal(out[country_code == "HC_ONLY", headcount], 0.2)
  expect_equal(out[country_code == "CANONICAL_NA", headcount], 0.3)
  expect_true(is.na(out[country_code == "FLAT_ONLY", headcount]))

  expect_equal(out[country_code == "MATCH", headcount_national], 0.1)
  expect_equal(out[country_code == "FLAT_ONLY", headcount_national], 0.2)
  expect_equal(out[country_code == "CANONICAL_NA", headcount_national], 0.3)
})
