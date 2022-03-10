# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")

# Constants
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
lkups <- lkups$versions_paths[[lkups$latest_release]]
censored <- readRDS("../testdata/censored.RDS")

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
  expect_identical(names(tmp1), names(tmp2))
  expect_identical(names(tmp1), names(tmp3))
  expect_identical(sapply(tmp1, class), sapply(tmp2, class))
  expect_identical(sapply(tmp1, class), sapply(tmp3, class))
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
  tmp <- pip_grp(
    country = "all",
    year = "mrv",
    povline = 1.9,
    lkup = lkups
  )
  check <- max(lkups$ref_lkup$reporting_year)
  expect_equal(tmp$reporting_year, check)

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
