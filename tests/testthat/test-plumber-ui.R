# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")

library(callr)
library(httr)

# Setup by starting APIs
root_path <- "http://localhost"
data_folder_root <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER")
# CAUTION: data_folder_root is also hard-coded on line 14 below. (passing data_folder_root fails)
# MAKE SURE BOTH ARE IN SYNC

api1 <- callr::r_session$new()

Sys.sleep(5)

api1$call(function() {
  # Use double assignment operator so the lkups object is available in the global
  # environment of the background R session, so it is available for the API
  lkups <<- pipapi:::clean_api_data(
    data_folder_root = Sys.getenv("PIPAPI_DATA_ROOT_FOLDER")
  )
  plbr_file <- system.file("plumber", "v1", "plumber.R", package = "pipapi")
  pr <- plumber::plumb(plbr_file)
  pr$run(port = 8000)
})


Sys.sleep(10)

test_that("Poverty line endpoint is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/poverty-lines")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(names(tmp_resp[[1]]), c("name", "poverty_line", "is_default", "is_visible"))
})

test_that("Indicators master endpoint is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/indicators")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(
    names(tmp_resp[[1]]),
    c(
      "page",
      "wdi_code",
      "indicator_code",
      "indicator_name",
      "scale_data",
      "scale_display",
      "number_of_decimals",
      "indicator_definition_short",
      "indicator_definition_long",
      "key_indicator_template",
      "category",
      "is_sensitive_to_povline",
      "symbol",
      "sort_order",
      "pip_sort_order",
      "tags",
      "from_year",
      "to_year"
    )
  )
})

test_that("Countries endpoint is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/countries")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(names(tmp_resp[[1]]), c(
    "region_code",
    "country_code",
    "country_name",
    "income_group",
    "iso2_code"
  ))
})

test_that("CPI endpoint is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/cpi")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  skip("Needs to be changed to tidy format")
  expect_equal(names(tmp_resp[[1]]), c(
    "country_code",
    "country_name",
    "region_code"
  ))
})

test_that("Homepage country charts endpoint is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/hp-countries")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_identical(
    names(tmp_resp[[1]]),
    c(
      "region_code", "country_code",
      "reporting_year", "poverty_line",
      "reporting_pop", "pop_in_poverty"
    )
  )
})

test_that("Poverty calculator chart endpoint is working for survey years", {
  # Send API request
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/pc-charts?country=AGO&year=2008&povline=1.9&group_by=none"
  )

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(names(tmp_resp[[1]]), c(
    "country_code", "reporting_year", "welfare_type",
    "pop_data_level", "median", "gini",
    "polarization", "mld", "decile1",
    "decile2", "decile3", "decile4",
    "decile5", "decile6", "decile7",
    "decile8", "decile9", "decile10",
    "region_code", "survey_coverage",
    "survey_comparability", "comparable_spell",
    "survey_year",
    "survey_mean_lcu", "survey_mean_ppp",
    "reporting_pop", "ppp", "cpi",
    "distribution_type", "is_interpolated",
    "poverty_line", "mean", "headcount",
    "poverty_gap", "poverty_severity", "watts"
  ))
})

test_that("Poverty calculator chart endpoint is working for imputed years", {
  # Send API request
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/pc-charts?country=AGO&year=2008&povline=1.9&fill_gaps=true"
  )

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(names(tmp_resp[[1]]), c(
    "country_code", "reporting_year",
    "poverty_line", "mean", "headcount",
    "poverty_gap", "poverty_severity",
    "watts", "region_code",
    "reporting_pop", "is_interpolated"
  ))
})

test_that("Poverty calculator chart endpoint is working for regional aggregates", {
  # Send API request
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/pc-charts?country=AGO&year=2008&povline=1.9&group_by=wb"
  )

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(names(tmp_resp[[1]]), c(
    "region_code", "reporting_year",
    "reporting_pop", "poverty_line",
    "headcount", "poverty_gap",
    "poverty_severity", "watts",
    "pop_in_poverty"
  ))
})

test_that("Poverty calculator chart endpoint is working for survey years", {
  # Send API request
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/pc-download?country=AGO&year=2008&povline=1.9&group_by=none"
  )

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(class(tmp_resp), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(names(tmp_resp), c(
    "country_code", "reporting_year", "welfare_type",
    "pop_data_level", "median", "gini",
    "polarization", "mld", "decile1",
    "decile2", "decile3", "decile4",
    "decile5", "decile6", "decile7",
    "decile8", "decile9", "decile10",
    "region_code", "survey_coverage",
    "survey_comparability", "comparable_spell",
    "survey_year",
    "survey_mean_lcu", "survey_mean_ppp",
    "reporting_pop", "ppp", "cpi",
    "distribution_type", "is_interpolated",
    "poverty_line", "mean", "headcount",
    "poverty_gap", "poverty_severity", "watts"
  ))
})

test_that("Poverty calculator chart endpoint is working for imputed years", {
  # Send API request
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/pc-download?country=AGO&year=2008&povline=1.9&fill_gaps=true"
  )

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(class(tmp_resp), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(names(tmp_resp), c(
    "country_code", "reporting_year",
    "poverty_line", "mean", "headcount",
    "poverty_gap", "poverty_severity",
    "watts", "region_code",
    "reporting_pop", "is_interpolated"
  ))
})

test_that("Poverty calculator download endpoint is working for regional aggregates", {
  # Send API request
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/pc-download?country=AGO&year=2008&povline=1.9&group_by=wb"
  )

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(class(tmp_resp), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(names(tmp_resp), c(
    "region_code", "reporting_year",
    "reporting_pop", "poverty_line",
    "headcount", "poverty_gap",
    "poverty_severity", "watts",
    "pop_in_poverty"
  ))
})

test_that("Country profile key indicators endpoint is working", {
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/cp-key-indicators?country=ALB&povline=1.9"
  )
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  tmp_resp <- tmp_resp[[1]]

  # KI 1
  expect_equal(
    names(tmp_resp$headcount_national[[1]]),
    c("country_code", "reporting_year", "headcount_national")
  )

  # KI 2
  expect_equal(
    names(tmp_resp$headcount[[1]]),
    c("country_code", "reporting_year", "poverty_line", "headcount")
  )

  # KI 3
  expect_equal(
    names(tmp_resp$mpm_headcount[[1]]),
    c("country_code", "reporting_year", "mpm_headcount")
  )

  # KI 4
  expect_equal(
    names(tmp_resp$shared_prosperity[[1]]),
    c("country_code", "year_range", "share_below_40", "share_total")
  )

  # KI 5
  expect_equal(
    names(tmp_resp$reporting_pop[[1]]),
    c("country_code", "reporting_year", "reporting_pop")
  )

  # KI 6
  expect_equal(
    names(tmp_resp$gni[[1]]),
    c("country_code", "reporting_year", "gni", "latest")
  )

  # KI 7
  expect_equal(
    names(tmp_resp$gdp_growth[[1]]),
    c("country_code", "reporting_year", "gdp_growth", "latest")
  )
})

test_that("Country profile charts endpoint is working", {
  r <- httr::GET(root_path, port = 8000, path = "api/v1/cp-charts?country=ALB&povline=1.9")
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  tmp_resp <- tmp_resp[[1]]

  # Chart 1 (poverty trend)
  expect_equal(
    names(tmp_resp$pov_charts[[1]]$pov_trend[[1]]),
    c(
      "country_code", "reporting_year", "poverty_line",
      "survey_acronym", "welfare_type", "survey_comparability",
      "comparable_spell", "headcount", "pop_in_poverty"
    )
  )

  # Chart 2 (poverty mrv in region)
  expect_equal(
    names(tmp_resp$pov_charts[[1]]$pov_mrv[[1]]),
    c(
      "country_code", "reporting_year",
      "poverty_line", "headcount",
      "sort_order"
    )
  )

  # Chart 3 (inequality gini/theil)
  expect_equal(
    names(tmp_resp$ineq_trend[[2]]),
    c(
      "country_code", "reporting_year",
      "survey_acronym", "welfare_type",
      "survey_comparability",
      "comparable_spell",
      "gini", "theil"
    )
  )

  # Chart 4 (inequality distribution)
  expect_equal(
    names(tmp_resp$ineq_bar[[1]]),
    c(
      "country_code", "reporting_year",
      "welfare_type", "gender",
      "agegroup", "education",
      "distribution",
      "poverty_share_by_group"
    )
  )

  # Chart 5 (MPM)
  expect_equal(
    names(tmp_resp$mpm[[1]]),
    c(
      "country_code", "reporting_year",
      "welfare_type",
      "mpm_education_attainment",
      "mpm_education_enrollment",
      "mpm_electricity",
      "mpm_sanitation",
      "mpm_water",
      "mpm_monetary",
      "mpm_headcount"
    )
  )

  # Chart 6 (SP)
  expect_equal(
    names(tmp_resp$sp[[1]]),
    c(
      "country_code", "year_range",
      "welfare_type", "distribution",
      "shared_prosperity"
    )
  )
})

test_that("Survey metadata endpoint is working", {

  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/survey-metadata")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(
    names(tmp_resp[[1]]),
    c(
      "country_code", "surveyid_year", "survey_acronym",
      "survey_title", "year_start", "year_end",
      "authoring_entity_name", "abstract",
      "collection_dates_cycle", "collection_dates_start",
      "collection_dates_end", "coverage",
      "sampling_procedure", "collection_mode",
      "coll_situation", "weight", "cleaning_operations"
    )
  )
})

# Kill process
api1$kill()
