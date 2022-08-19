# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

# Constants
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkups <- lkups$versions_paths[[lkups$latest_release]]
censored <- readRDS("../testdata/censored.rds")

# Check pip_grp against current implementation
# TO BE REMOVED ONCE pip() group_by OPTION is FULLY DEPRECATED
test_that("output from pip_grp is the same as output from pip", {
  out_pip <- pip(
    country = "all",
    year = "all",
    group_by = "wb",
    povline = 1.9,
    lkup = lkups
  )

  out_pip_grp <- pip_grp(
    country = "all",
    year = "all",
    group_by = "wb",
    povline = 1.9,
    lkup = lkups
  )

  expect_equal(class(out_pip), class(out_pip_grp))
  expect_equal(names(out_pip), names(out_pip_grp))
  expect_equal(nrow(out_pip), nrow(out_pip_grp))
  expect_equal(out_pip, out_pip_grp)
})


# Check output type
test_that("output type is correct", {
  tmp <- pip_grp(
    country = "all",
    year = 2000,
    group_by = "wb",
    povline = 1.9,
    lkup = lkups
  )
  expect_equal(class(tmp), c("data.table", "data.frame"))
})

# Check empty response
test_that("empty response is returned if no metadata is found", {
  tmp <- pip_grp("all", year = 2050, lkup = lkups, group_by = "none")
  expect_equal(nrow(tmp), 0)
  tmp <- pip_grp("all", year = 2050, lkup = lkups, group_by = "wb")
  expect_equal(nrow(tmp), 0)
})

# Check response columns
test_that("returned columns are the same for all queries", {
  tmp1 <- pip_grp('all', 2000, lkup = lkups, group_by = "none")
  tmp2 <- pip_grp('all', 2000, lkup = lkups, group_by = "wb")
  tmp3 <- pip_grp('all', 2050, lkup = lkups, group_by = "wb")
  tmp4 <- pip_grp('all', 2050, lkup = lkups, group_by = "none")
  expect_identical(names(tmp1), names(tmp2))
  expect_identical(names(tmp1), names(tmp3))
  expect_identical(names(tmp1), names(tmp4))
  expect_identical(sapply(tmp1, class), sapply(tmp2, class))
  expect_identical(sapply(tmp1, class), sapply(tmp3, class))
})

# Check response names
test_that("returned column names are correct", {
  cols <- c('region_name', 'region_code', 'reporting_year', 'reporting_pop', 'poverty_line',
            'headcount', 'poverty_gap', 'poverty_severity', 'watts', 'mean', 'pop_in_poverty')
  tmp1 <- pip_grp('all', 2000, lkup = lkups, group_by = "none")
  tmp2 <- pip_grp('all', 2000, lkup = lkups, group_by = "wb")
  expect_identical(names(tmp1), cols)
  expect_identical(names(tmp2), cols)
})

# Check custom region name
test_that("returned region_name and region_code is correct for custom aggregations", {
  tmp1 <- pip_grp('all', 2000, lkup = lkups, group_by = "none")
  expect_identical(tmp1$region_name, 'CUSTOM')
  expect_identical(tmp1$region_code, 'CUSTOM')
})

# Year selection
test_that("year selection is working", {

  # All years for a single country
  tmp <- pip_grp(
    country = "AGO",
    year = "all",
    povline = 1.9,
    lkup = lkups
  )
  check <- length(unique(lkups$ref_lkup$reporting_year))
  expect_equal(nrow(tmp), check)

  # Most recent year for a single country
  tmp <- pip_grp(
    country = "AGO",
    year = "mrv",
    povline = 1.9,
    lkup = lkups
  )
  check <- max(lkups$ref_lkup$reporting_year)
  expect_equal(tmp$reporting_year, check)

  # Most recent year for all countries
  # Should return the most recent for each country
  # Therefore we expect having more than one year in the response
  # Not a great unit test... To be improved
  tmp <- pip_grp(
    country = "all",
    year = "mrv",
    povline = 1.9,
    lkup = lkups
  )

  expect_true(length(unique(tmp$reporting_year)) > 1)

})

# Regional aggregations
test_that("Regional aggregations are working", {
  # Check WB regional aggregation
  tmp <- pip_grp(
    country = "all",
    year = 2000,
    group_by = "wb",
    povline = 3.5,
    lkup = lkups,
    censor = FALSE
  )
  expect_equal(nrow(tmp), 8)

  # Check custom regional aggregation
  tmp <- pip_grp(
    country = "all",
    year = 2000,
    group_by = "none",
    povline = 3.5,
    lkup = lkups
  )
  expect_equal(nrow(tmp), 1)
  expect_equal(tmp$region_code, 'CUSTOM')
})

# Censoring
test_that("Censoring for regional aggregations is working", {
  lkups2 <- lkups
  censored <- list(
    regions = data.frame(
      region_code = "SSA",
      reporting_year = 2019,
      statistic = "all",
      id = "SSA_2019"
    ))
  lkups2$censored <- censored
  tmp <- pip_grp(
    country = "all",
    year = 2019,
    group_by = "wb",
    povline = 1.9,
    lkup = lkups2
  )
  # expect_equal(nrow(tmp), 7)
  id <- paste0(tmp$region_code, "_", tmp$reporting_year)
  expect_true(!censored$region$id %in% id)
})

# region selection
test_that("region selection is working for single region", {
  region <- "SSA"

  out <- pip_grp(
    country = region,
    year = 2018,
    group_by = "wb",
    povline = 1.9,
    lkup = lkups
  )

  expect_equal(nrow(out), length(region))
  expect_equal(out$region_code, region)
})

test_that("region selection is working for multiple regions", {
  region <- c("SSA", "MNA")

  out <- pip_grp(
    country = region,
    year = 2018,
    group_by = "wb",
    povline = 1.9,
    lkup = lkups
  )

  expect_equal(nrow(out), length(region))
  expect_equal(sort(out$region_code), sort(region))
})

test_that("region selection is working for all countries", {
  region <- "all"
  expected_region_values <- lkups$query_controls$region$values
  expected_region_values <- expected_region_values[expected_region_values != "all"]

  out <- pip_grp(
    country = region,
    year = 2010,
    group_by = "wb",
    povline = 1.9,
    lkup = lkups
  )

  expect_equal(nrow(out), length(expected_region_values))
  expect_equal(sort(out$region_code), sort(expected_region_values))
})

test_that("region selection is working for multiple regions and country from other region", {
  # ideally "COL" should be dropped
  # but for the time being, all countries are being selected
  # So this selection will effectively return country = "all"
  region <- c("SSA", "MNA", "COL")
  expected_region_values <- lkups$query_controls$region$values
  expected_region_values <- expected_region_values[expected_region_values != "all"]

  out <- pip_grp(
    country = region,
    year = 2010,
    group_by = "wb",
    povline = 1.9,
    lkup = lkups
  )

  expect_equal(nrow(out), length(expected_region_values))
  expect_equal(sort(out$region_code), sort(expected_region_values))
})
