# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "" ||
          Sys.getenv("PIPAPI_TEST_PLUMBER") != "TRUE")


library(callr)
library(httr)

# Setup by starting APIs
root_path <- "http://localhost"
api1 <- callr::r_session$new(options =  r_session_options(user_profile = FALSE))
Sys.sleep(5)
api1$call(function() {
  # Use double assignment operator so the lkups object is available in the global
  # environment of the background R session, so it is available for the API
  lkups <<- pipapi::create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
  pipapi::start_api(port = 8000)
})
Sys.sleep(20)

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
      "scale_factor",
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

test_that("Aux endpoint is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/aux?table=countries")

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

test_that("Homepage country charts endpoint is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/hp-countries")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_identical(
    names(tmp_resp[[1]]),
    c(
      "region_code", "country_code",
      "year", "poverty_line",
      "pop", "pop_in_poverty"
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
    "country_code", "year", "welfare_type",
    "reporting_level", "median", "gini",
    "polarization", "mld", "decile1",
    "decile2", "decile3", "decile4",
    "decile5", "decile6", "decile7",
    "decile8", "decile9", "decile10",
    "region_code", "survey_coverage",
    "survey_comparability", "comparable_spell",
    "welfare_time", "pop", "ppp", "cpi",
    "distribution_type", "is_interpolated",
    "poverty_line", "mean", "headcount",
    "poverty_gap", "poverty_severity", "watts",
    "pop_in_poverty"
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
    "country_code", "year",
    "poverty_line", "mean", "headcount",
    "poverty_gap", "poverty_severity",
    "watts", "region_code",
    "pop", "is_interpolated",
    "pop_in_poverty"
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
    "region_code", "year",
    "pop", "poverty_line",
    "headcount", "poverty_gap",
    "poverty_severity", "watts",
    "mean", "pop_in_poverty"
  ))
})

test_that("Poverty calculator download endpoint is working for survey years", {
  # Send API request
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/pc-download?country=AGO&year=2008&povline=1.9&group_by=none"
  )

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(class(tmp_resp), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(names(tmp_resp), c(
    "country_code", "year", "welfare_type",
    "reporting_level", "median", "gini",
    "polarization", "mld", "decile1",
    "decile2", "decile3", "decile4",
    "decile5", "decile6", "decile7",
    "decile8", "decile9", "decile10",
    "region_code", "survey_coverage",
    "survey_comparability", "comparable_spell",
    "welfare_time", "pop", "ppp", "cpi",
    "distribution_type", "is_interpolated",
    "poverty_line", "mean", "headcount",
    "poverty_gap", "poverty_severity", "watts",
    "pop_in_poverty"
  ))
})

test_that("Poverty calculator download endpoint is working for imputed years", {
  # Send API request
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/pc-download?country=AGO&year=2008&povline=1.9&fill_gaps=true"
  )

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(class(tmp_resp), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(names(tmp_resp), c(
    "country_code", "year",
    "poverty_line", "mean", "headcount",
    "poverty_gap", "poverty_severity",
    "watts", "region_code",
    "pop", "is_interpolated",
    "pop_in_poverty"
  ))
})

test_that("Poverty calculator download endpoint is working for regional aggregates", {
  # Send API request
  r <- httr::GET(root_path,
    port = 8000,
    path = "api/v1/pc-download?country=AGO&year=2008&povline=1.9&group_by=wb"
  )

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8", show_col_types = FALSE)
  expect_equal(class(tmp_resp), c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))
  expect_equal(names(tmp_resp), c(
    "region_code", "year",
    "pop", "poverty_line",
    "headcount", "poverty_gap",
    "poverty_severity", "watts",
    "mean", "pop_in_poverty"
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
    c("country_code", "year", "headcount_national")
  )

  # KI 2
  expect_equal(
    names(tmp_resp$headcount[[1]]),
    c("country_code", "year", "poverty_line", "headcount")
  )

  # KI 3
  expect_equal(
    names(tmp_resp$mpm_headcount[[1]]),
    c("country_code", "year", "mpm_headcount")
  )

  # KI 4
  expect_equal(
    names(tmp_resp$shared_prosperity[[1]]),
    c("country_code", "year_range", "share_below_40", "share_total")
  )

  # KI 5
  expect_equal(
    names(tmp_resp$pop[[1]]),
    c("country_code", "year", "pop")
  )

  # KI 6
  expect_equal(
    names(tmp_resp$gni[[1]]),
    c("country_code", "year", "gni", "latest")
  )

  # KI 7
  expect_equal(
    names(tmp_resp$gdp_growth[[1]]),
    c("country_code", "year", "gdp_growth", "latest")
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
      "country_code", "year", "poverty_line",
      "survey_acronym", "welfare_type", "survey_comparability",
      "comparable_spell", "headcount", "pop_in_poverty"
    )
  )

  # Chart 2 (poverty mrv in region)
  expect_equal(
    names(tmp_resp$pov_charts[[1]]$pov_mrv[[1]]),
    c(
      "country_code", "year",
      "poverty_line", "headcount",
      "sort_order"
    )
  )

  # Chart 3 (inequality gini/theil)
  expect_equal(
    names(tmp_resp$ineq_trend[[2]]),
    c(
      "country_code", "year",
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
      "country_code", "year",
      "survey_coverage",
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
      "country_code", "year",
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
  expect_equal(names(tmp_resp[[1]]),
               c("country_code", "year" ,
                 "survey_title", "survey_conductor",  "survey_coverage",
                 "welfare_type",    "distribution_type", "metadata"))
  expect_equal(
    names(tmp_resp[[1]]$metadata[[1]]),
    c(
      "surveyid_year", "survey_acronym",
      "year_start", "year_end",
      "authoring_entity_name", "abstract",
      "collection_dates_cycle", "collection_dates_start",
      "collection_dates_end", #"survey_coverage",
      "sampling_procedure", "collection_mode",
      "coll_situation", "weight", "cleaning_operations"
    )
  )
})

# Kill process
api1$kill()
