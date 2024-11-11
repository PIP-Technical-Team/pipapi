# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")

skip_if(data_dir == "")

latest_version <-
  available_versions(data_dir) |>
  max()

lkups <- create_versioned_lkups(data_dir,
                                vintage_pattern = latest_version)
lkups <- lkups$versions_paths[[lkups$latest_release]]


set.seed(42)
lkups$pl_lkup <- lkups$pl_lkup[sample(nrow(lkups$pl_lkup), 10)]
lkups2 <- lkups
lkups2$svy_lkup <- lkups2$svy_lkup[country_code %in% c('AGO', 'ZWE')]
lkups2$ref_lkup <- lkups2$ref_lkup[country_code %in% c('AGO', 'ZWE')]

test_that("ui_hp_stacked() works as expected", {

  with_mocked_bindings({
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

  },
  get_caller_names = function() c("pip_grp")
  )


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

test_that("ui_svy_meta() works as expected", {
  expected_names <-
    c(
      "country_code",
      "country_name",
      "reporting_year" ,
      "survey_year",
      "surveyid_year",
      "survey_title",
      "survey_conductor",
      "survey_coverage",
      "welfare_type",
      "distribution_type",
      "metadata"
    )
  expected_metadata <- c(
    #"surveyid_year",
    "survey_acronym",
    "year_start",
    "year_end",
    "authoring_entity_name",
    "abstract",
    "collection_dates_cycle",
    "collection_dates_start",
    "collection_dates_end",
    "sampling_procedure",
    "collection_mode",
    "coll_situation",
    "weight",
    "cleaning_operations"
  )

  res <- ui_svy_meta(country = "AGO", lkup = lkups)
  expect_equal(unique(res$country_code), "AGO")
  expect_equal(names(res),
               expected_names)

  expect_equal(
    names(res$metadata[[1]]),
    expected_metadata
  )

  res <- ui_svy_meta(country = "all", lkup = lkups)

  expect_true(all(unique(res$country_code) %in%
                    lkups$query_controls$country$values))

  expect_equal(names(res),
               expected_names)

  expect_equal(
    names(res$metadata[[1]]),
    expected_metadata
  )

})


