# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

# constants
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkups <- lkups$versions_paths[[lkups$latest_release]]

set.seed(42)
lkups$pl_lkup <- lkups$pl_lkup[sample(nrow(lkups$pl_lkup), 10)]
lkups2 <- lkups
lkups2$svy_lkup <- lkups2$svy_lkup[country_code %in% c('AGO', 'ZWE')]
lkups2$ref_lkup <- lkups2$ref_lkup[country_code %in% c('AGO', 'ZWE')]

test_that("ui_hp_stacked() works as expected", {
  res                <- ui_hp_stacked(povline = 1.9, lkup = lkups2)
  countries_selected <- lkups2$svy_lkup[, unique(country_code)]
  tmp                <- lkups2$aux_files$country_list[country_code %in% countries_selected]

  var_code <- c("country_code", "region_code", "world_code")

  regions_of_countries <-
    tmp[, ..var_code] |>
    melt(id.vars = "country_code") |>
    {\(.) .[, value] }() |>
    unique()


  expect_identical(
    names(res),
    c(
      "region_code", "reporting_year",
      "poverty_line", "pop_in_poverty"
    )
  )
  expect_identical(unique(res$region_code) |>
                     sort(),
                   regions_of_countries |>
                     sort())
  expect_true(all(res$pop_in_poverty == floor(res$pop_in_poverty))) # No decimals for population numbers
})

test_that("ui_hp_countries() works as expected", {
  res <- ui_hp_countries(country = c("AGO", "CIV"), povline = 1.9, lkup = lkups)
  expect_identical(
    names(res),
    c(
      "region_code", "country_code",
      "reporting_year", "poverty_line",
      "reporting_pop", "pop_in_poverty"
    )
  )
  expect_true(all(res$pop_in_poverty < 50))
  check <- lkups$svy_lkup[country_code %in% c("AGO", "CIV")]$reporting_year
  expect_equal(res$reporting_year, check)
})
