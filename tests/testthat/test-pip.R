# Disable until a full set of anonymous package data has been created
# skip("Disable until a full set of anonymous package data has been created")

# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")

# files <- sub("[.]fst", "", list.files("../testdata/app_data/20210401/survey_data/"))
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
lkup <- lkups$versions_paths[[lkups$latest_release]]

test_that("Reporting level filtering is working", {
  reporting_levels <- c("national", "urban", "rural", "all")
  tmp <- lapply(reporting_levels,
                function(x) {
                  pip(country = "CHN",
                      year = "2008",
                      povline = 1.9,
                      popshare = NULL,
                      welfare_type = "all",
                      reporting_level = x,
                      fill_gaps = FALSE,
                      ppp = 10,
                      lkup = lkup,
                      debug = FALSE)
                })
  names(tmp) <- reporting_levels

  expect_equal(nrow(tmp$national), 1)
  expect_equal(tmp$national$reporting_level, "national")

  expect_equal(nrow(tmp$urban), 1)
  expect_equal(tmp$urban$reporting_level, "urban")

  expect_equal(nrow(tmp$rural), 1)
  expect_equal(tmp$rural$reporting_level, "rural")

  expect_equal(nrow(tmp$all), 3)
  expect_equal(sort(tmp$all$reporting_level), c("national", "rural", "urban"))
  })

# Use only test data
# lkup$svy_lkup <- lkup$svy_lkup[(cache_id %in% files | country_code == "AGO")]
# lkup$ref_lkup <- lkup$ref_lkup[(cache_id %in% files | country_code == "AGO")]

# Check output type ----
test_that("output type is correct", {
  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkup
  )

  expect_equal(class(tmp), c("data.table", "data.frame"))
})

# Check empty response
test_that("empty response is returned if no metadata is found", {
  tmp <- pip("COL", year = 2050, lkup = lkup)
  expect_equal(nrow(tmp), 0)
  tmp <- pip("COL", year = 2050, lkup = lkup, fill_gaps = TRUE)
  expect_equal(nrow(tmp), 0)
})

# Check response columns
test_that("returned columns are the same for all non-group_by queries", {
  tmp1 <- pip('AGO', 2000, lkup = lkup)
  tmp2 <- pip('AGO', 2010, lkup = lkup, fill_gaps = TRUE)
  tmp3 <- pip('AGO', 2050, lkup = lkup)
  expect_identical(names(tmp1), names(tmp2))
  expect_identical(names(tmp1), names(tmp3))
  # skip("collapsed columns (e.g. survey_year, cpi) are converted to character")
  expect_identical(sapply(tmp1, class), sapply(tmp2, class))
  expect_identical(sapply(tmp1, class), sapply(tmp3, class))
})

# Check selections ----

## Year -----
test_that("year selection is working", {

  # All years for a single country
  tmp <- pip(
    country = "AGO",
    year = "all",
    povline = 1.9,
    lkup = lkup
  )
  check <- sum(lkup$svy_lkup$country_code == "AGO")
  expect_equal(nrow(tmp), check)

  # Most recent year for a single country
  tmp <- pip(
    country = "AGO",
    year = "mrv",
    povline = 1.9,
    lkup = lkup
  )
  check <- max(lkup$svy_lkup[country_code == "AGO"]$reporting_year)
  expect_equal(tmp$reporting_year, sum(check))

  # Most recent year for a single country (w/ fill_gaps)
  tmp <- pip(
    country = "AGO",
    year = "mrv",
    povline = 1.9,
    fill_gaps = TRUE,
    lkup = lkup
  )
  check <- max(lkup$ref_lkup$reporting_year)
  expect_equal(tmp$reporting_year, check)

  # Most recent year for all countries
  tmp <- pip(
    country = "all",
    year = "mrv",
    povline = 1.9,
    lkup = lkup
  )
  check <- max(lkup$svy_lkup$reporting_year)
  expect_equal(unique(tmp$reporting_year), check)

})

## Welfare type ----
test_that("welfare_type selection are correct", {
  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkup,
    welfare_type = "all"
  )

  expect_equal(sort(unique(tmp$welfare_type)), c("consumption", "income"))

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkup,
    welfare_type = "consumption"
  )

  expect_equal(unique(tmp$welfare_type), "consumption")

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkup,
    welfare_type = "income"
  )

  expect_equal(unique(tmp$welfare_type), "income")
})

## Reporting level ----
test_that("reporting_level selection are correct", {
  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkup,
    reporting_level = "all"
  )

  expect_equal(sort(unique(tmp$reporting_level)), c("national", "rural", "urban"))

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkup,
    reporting_level = "national"
  )

  expect_equal(sort(unique(tmp$reporting_level)), c("national"))

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkup,
    reporting_level = "rural"
  )

  expect_equal(sort(unique(tmp$reporting_level)), c("rural"))

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkup,
    reporting_level = "urban"
  )

  expect_equal(sort(unique(tmp$reporting_level)), c("urban"))
})

# Check aggregation ----
test_that("Aggregation is working", {
  skip("Aggregation not correctly implemented")
  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkup
  )
  expect_equal(nrow(tmp), 1)
})

# Check imputation ----
test_that("Imputation is working", {

  n_ref_years <- length(unique(lkup$ref_lkup$reporting_year))

  tmp <- pip(
    country = "AGO",
    year = "all",
    povline = 3.5,
    fill_gaps = TRUE,
    lkup = lkup
  )
  # Why is this correct? E.g. tmp %>% group_by(country_code) %>% summarise(n = n())
  expect_equal(nrow(tmp), n_ref_years)
  # expect_equal(nrow(tmp), 182)
})

test_that("Imputation is working for mixed distributions aggregate / micro", {
  tmp <- pip(
    country = "IND",
    year = 1993,
    povline = 1.9,
    fill_gaps = TRUE,
    lkup = lkup
  )

  expect_equal(nrow(tmp), 3)
  # expect_equal(tmp$headcount[tmp$reporting_level == "national"], 0.4794678)
  # expect_equal(tmp$headcount[tmp$reporting_level == "rural"], 0.5366117)
  # expect_equal(tmp$headcount[tmp$reporting_level == "urban"], 0.3184304)
  # expect_equal(tmp$mean[tmp$reporting_level == "national"], 73.6233776262657 * 12 / 365)
})

test_that("Imputation is working for mixed distributions group / micro", {
  tmp <- pip(
    country = "ZWE",
    year = 2015,
    povline = 1.9,
    fill_gaps = TRUE,
    lkup = lkup
  )

  expect_equal(nrow(tmp), 1)
  # expect_equal(tmp$headcount, 0.2867193)
  # expect_equal(tmp$mean, 134.504825993006 * 12 / 365)
})

## extrapolation ----
test_that("imputation is working for extrapolated aggregate distribution", {
  tmp <- pip(
    country = "CHN",
    year = 1988,
    povline = 1.9,
    fill_gaps = TRUE,
    lkup = lkup
  )

  expect_equal(nrow(tmp), 3)
  # expect_equal(tmp$headcount[tmp$reporting_level == "national"], 0.5339021)
  # expect_equal(tmp$headcount[tmp$reporting_level == "rural"], 0.6549765)
  # expect_equal(tmp$headcount[tmp$reporting_level == "urban"], 0.1701744)
  # expect_equal(tmp$mean[tmp$reporting_level == "national"], 62.5904793524725 * 12 / 365)
})

test_that("Distributional stats are correct for interpolated/extrapolated reporting years",{

  # Extrapolation (one year)
  tmp1 <- pip("AGO", year = 1981, fill_gaps = TRUE, lkup = lkup)
  tmp2 <- pip("AGO", year = 2000, fill_gaps = FALSE, lkup = lkup)
  expect_equal(tmp1$gini, tmp2$gini)
  expect_equal(tmp1$median, tmp2$median)
  expect_equal(tmp1$mld, tmp2$mld)
  expect_equal(tmp1$decile10, tmp2$decile10)

  # Interpolation (one year)
  tmp1 <- pip("AGO", year = 2004, fill_gaps = TRUE, lkup = lkup)
  expect_equal(tmp1$gini, NA_real_)
  expect_equal(tmp1$median ,NA_real_)
  expect_equal(tmp1$mld, NA_real_)
  expect_equal(tmp1$decile10, NA_real_)

  # Extrapolation (multiple years)
  tmp1 <- pip("AGO", year = 1981:1999, fill_gaps = TRUE, lkup = lkup)
  expect_equal(unique(tmp1$gini), tmp2$gini)
  expect_equal(unique(tmp1$median), tmp2$median)
  expect_equal(unique(tmp1$mld), tmp2$mld)
  expect_equal(unique(tmp1$decile10), tmp2$decile10)

  # Interpolation (mulitiple year)
  tmp1 <- pip("AGO", year = 2001:2007, fill_gaps = TRUE, lkup = lkup)
  expect_equal(unique(tmp1$gini), NA_real_)
  expect_equal(unique(tmp1$median), NA_real_)
  expect_equal(unique(tmp1$mld), NA_real_)
  expect_equal(unique(tmp1$decile10), NA_real_)

})


# Check regional aggregations ----
test_that("Regional aggregations are working", {
  tmp <- pip_grp(
    country = "all",
    year = "2010",
    group_by = "wb",
    povline = 3.5,
    lkup = lkup
  )

  expect_equal(nrow(tmp), 8)
})

# Check pop_share ----
test_that("pop_share option is working", {
  tmp <- pip(
    country = "AGO",
    year = 2000,
    popshare = .2,
    lkup = lkup
  )

  expect_equal(nrow(tmp), 1)
})

test_that("pop_share option is returning consisten results for single microdata distributions", {
  # Average poverty line
  povline <- 2.0

  pl <- pip(
    country = "AGO",
    year = 2008,
    povline = povline,
    lkup = lkup
  )

  ps <- pip(
    country = "AGO",
    year = 2008,
    popshare = pl$headcount,
    lkup = lkup
  )

  expect_equal(round(pl$headcount, 3), round(ps$headcount, 3))
  expect_equal(povline, round(ps$poverty_line, 2))
  # Low poverty line
  # Fails for lower poverty lines
  povline <- .3

  pl <- pip(
    country = "AGO",
    year = 2008,
    povline = povline,
    lkup = lkup
  )

  ps <- pip(
    country = "AGO",
    year = 2008,
    popshare = pl$headcount,
    lkup = lkup
  )

  expect_equal(round(pl$headcount, 3), round(ps$headcount, 3))
  expect_equal(povline, round(ps$poverty_line, 2))

  # High poverty line
  # Fails for higher poverty lines
  povline <- 33

  pl <- pip(
    country = "AGO",
    year = 2008,
    povline = povline,
    lkup = lkup
  )

  ps <- pip(
    country = "AGO",
    year = 2008,
    popshare = pl$headcount,
    lkup = lkup
  )

  expect_equal(round(pl$headcount, 2), round(ps$headcount, 2))
  expect_equal(povline, round(ps$poverty_line, 0))
})

test_that("pop_share option is returning consisten results for single grouped distributions", {
  # Average poverty line
  povline <- 2.0
  country <- "MNG"
  year <- 1995

  pl <- pip(
    country = country,
    year = year,
    povline = povline,
    lkup = lkup
  )

  ps <- pip(
    country = country,
    year = year,
    popshare = pl$headcount,
    lkup = lkup
  )

  expect_equal(round(pl$headcount, 3), round(ps$headcount, 3))
  expect_equal(povline, round(ps$poverty_line, 2))
  # Low poverty line
  # Fails for lower poverty lines
  povline <- .8

  pl <- pip(
    country = country,
    year = year,
    povline = povline,
    lkup = lkup
  )

  ps <- pip(
    country = country,
    year = year,
    popshare = pl$headcount,
    lkup = lkup
  )

  expect_equal(round(pl$headcount, 3), round(ps$headcount, 3))
  expect_equal(povline, round(ps$poverty_line, 2))

  # High poverty line
  # Fails for higher poverty lines
  povline <- 20

  pl <- pip(
    country = country,
    year = year,
    povline = povline,
    lkup = lkup
  )

  ps <- pip(
    country = country,
    year = year,
    popshare = pl$headcount,
    lkup = lkup
  )

  expect_equal(round(pl$headcount, 2), round(ps$headcount, 2))
  expect_equal(povline, round(ps$poverty_line, 0))
})

test_that("pop_share option is returning consisten results for single aggregate distributions", {
  skip("popshare not working for aggregate distributions")
  # Average poverty line
  povline <- 2.0
  country <- "CHN"
  year <- 2018

  pl <- pip(
    country = country,
    year = year,
    povline = povline,
    reporting_level = "national",
    lkup = lkup
  )

  ps <- pip(
    country = country,
    year = year,
    popshare = pl$headcount,
    reporting_level = "national",
    lkup = lkup
  )

  expect_equal(round(pl$headcount, 3), round(ps$headcount, 3))
  expect_equal(povline, round(ps$poverty_line, 2))
  # Low poverty line
  # Fails for lower poverty lines
  povline <- .9

  pl <- pip(
    country = country,
    year = year,
    povline = povline,
    reporting_level = "national",
    lkup = lkup
  )

  ps <- pip(
    country = country,
    year = year,
    popshare = pl$headcount,
    reporting_level = "national",
    lkup = lkup
  )

  expect_equal(round(pl$headcount, 3), round(ps$headcount, 3))
  expect_equal(povline, round(ps$poverty_line, 2))

  # High poverty line
  # Fails for higher poverty lines
  povline <- 20

  pl <- pip(
    country = country,
    year = year,
    povline = povline,
    reporting_level = "national",
    lkup = lkup
  )

  ps <- pip(
    country = country,
    year = year,
    popshare = pl$headcount,
    reporting_level = "national",
    lkup = lkup
  )

  expect_equal(round(pl$headcount, 2), round(ps$headcount, 2))
  expect_equal(povline, round(ps$poverty_line, 0))
})

#Check pip country name case insensitive

test_that("pip country name case insensitive", {
  skip("Code to handle mixed casing has been moved to API filter level")
  #Run it on pip-fake-data
  tmp1 <- pip(country = "nga",year = "ALL", povline = 1.9, lkup = lkup)
  tmp2 <- pip(country = "NGA",year = "all", povline = 1.9, lkup = lkup)
  tmp3 <- pip(country = "All",year = "ALL", povline = 1.9, lkup = lkup)
  tmp4 <- pip(country = "chn",year = "1981", povline = 1.9, lkup = lkup)
  tmp5 <- pip(country = "chn",year = "ALL", povline = 1.9, lkup = lkup)

  expect_equal(nrow(tmp1), 1)
  expect_equal(nrow(tmp2), 1)
  expect_equal(nrow(tmp3), 22)
  expect_equal(nrow(tmp4), 3)
  expect_equal(nrow(tmp5), 6)
})


#Better error message when more than one data set is passed.

test_that("error when more than one dataset is passed", {

  expect_error(pip(country = "all", year = "all", povline = 1.9, lkup = lkups),
               "You are probably passing more than one dataset as lkup argument.
  Try passing a single one by subsetting it lkup <- lkups$versions_paths$dataset_name_PROD",
  fixed = TRUE)
})
