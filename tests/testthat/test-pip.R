# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")

files <- sub("[.]fst", "", list.files("../testdata/app_data/20210401/survey_data/"))
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
lkups <- lkups$versions_paths$latest_release


test_that("Reporting level filtering is working", {
  reporting_levels <- c("national", "urban", "rural", "all")
  tmp <- lapply(reporting_levels,
                function(x) {
                  pip(country="CHN",
                      year="2008",
                      povline=1.9,
                      popshare=NULL,
                      welfare_type = "all",
                      reporting_level = x,
                      fill_gaps = FALSE,
                      ppp = 10,
                      lkup = lkups,
                      debug = FALSE)
                })
  names(tmp) <- reporting_levels

  expect_equal(nrow(tmp$national), 1)
  expect_equal(tmp$national$pop_data_level, "national")

  expect_equal(nrow(tmp$urban), 1)
  expect_equal(tmp$urban$pop_data_level, "urban")

  expect_equal(nrow(tmp$rural), 1)
  expect_equal(tmp$rural$pop_data_level, "rural")

  expect_equal(nrow(tmp$all), 3)
  expect_equal(sort(tmp$all$pop_data_level), c("national", "rural", "urban"))
  })








# Use only test data
lkups$svy_lkup <- lkups$svy_lkup[(cache_id %in% files | country_code == "AGO")]
lkups$ref_lkup <- lkups$ref_lkup[(cache_id %in% files | country_code == "AGO")]

# Check output type ----
test_that("output type is correct", {
  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkups
  )

  expect_equal(class(tmp), c("data.table", "data.frame"))
})

# Check empty response
test_that("empty response is returned if no metadata is found", {
  tmp <- pip("COL", year = 2050, lkup = lkups)
  expect_equal(nrow(tmp), 0)
  tmp <- pip("COL", year = 2050, lkup = lkups, fill_gaps = TRUE)
  expect_equal(nrow(tmp), 0)
})

# Check response columns
test_that("returned columns are the same for all non-group_by queries", {
  tmp1 <- pip('AGO', 2000, lkup = lkups)
  tmp2 <- pip('AGO', 2010, lkup = lkups, fill_gaps = TRUE)
  tmp3 <- pip('AGO', 2050, lkup = lkups)
  expect_identical(names(tmp1), names(tmp2))
  expect_identical(names(tmp1), names(tmp3))
  skip("collapsed columns (e.g. survey_year, cpi) are converted to character")
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
    lkup = lkups
  )
  check <- sum(lkups$svy_lkup$country_code == "AGO")
  expect_equal(nrow(tmp), check)

  # Most recent year for a single country
  tmp <- pip(
    country = "AGO",
    year = "mrv",
    povline = 1.9,
    lkup = lkups
  )
  check <- max(lkups$svy_lkup[country_code == "AGO"]$reporting_year)
  expect_equal(tmp$reporting_year, sum(check))

  # Most recent year for a single country (w/ fill_gaps)
  tmp <- pip(
    country = "AGO",
    year = "mrv",
    povline = 1.9,
    fill_gaps = TRUE,
    lkup = lkups
  )
  check <- max(lkups$ref_lkup$reporting_year)
  expect_equal(tmp$reporting_year, check)
})

## Welfare type ----
test_that("welfare_type selection are correct", {
  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkups,
    welfare_type = "all"
  )

  expect_equal(sort(unique(tmp$welfare_type)), c("consumption", "income"))

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkups,
    welfare_type = "consumption"
  )

  expect_equal(unique(tmp$welfare_type), "consumption")

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkups,
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
    lkup = lkups,
    reporting_level = "all"
  )

  expect_equal(sort(unique(tmp$pop_data_level)), c("national", "rural", "urban"))

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkups,
    reporting_level = "national"
  )

  expect_equal(sort(unique(tmp$pop_data_level)), c("national"))

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkups,
    reporting_level = "rural"
  )

  expect_equal(sort(unique(tmp$pop_data_level)), c("rural"))

  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    lkup = lkups,
    reporting_level = "urban"
  )

  expect_equal(sort(unique(tmp$pop_data_level)), c("urban"))
})

# Check aggregation ----
test_that("Aggregation is working", {
  skip("Aggregation not correctly implemented")
  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    aggregate = TRUE,
    lkup = lkups
  )
  expect_equal(nrow(tmp), 1)
})

# Check imputation ----
test_that("Imputation is working", {
  tmp <- pip(
    country = "all",
    year = "all",
    povline = 3.5,
    fill_gaps = TRUE,
    lkup = lkups
  )
  # Why is this correct? E.g. tmp %>% group_by(country_code) %>% summarise(n = n())
  expect_equal(nrow(tmp), 195)
  # expect_equal(nrow(tmp), 182)
})

test_that("Imputation is working for mixed distributions aggregate / micro", {
  tmp <- pip(
    country = "IND",
    year = 1993,
    povline = 3.5,
    fill_gaps = TRUE,
    lkup = lkups
  )

  expect_equal(nrow(tmp), 3)
})

# Check regional aggregations
test_that("Regional aggregations are working", {
  tmp <- pip(
    country = "all",
    year = "2000",
    group_by = "wb",
    povline = 3.5,
    lkup = lkups
  )

  expect_equal(nrow(tmp), 3) # Should be changed if lkups are updated. Full set of regions is 8.
})

# Check pop_share
test_that("pop_share option is working", {
  tmp <- pip(
    country = "AGO",
    year = 2000,
    popshare = .2,
    lkup = lkups
  )

  expect_equal(nrow(tmp), 1)
})

