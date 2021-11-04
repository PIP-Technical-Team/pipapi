skip_if(Sys.getenv('WBPIP_RUN_LOCAL_TESTS') != "TRUE")
# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
lkups <- lkups$versions_paths$latest_release

# aggregated distribution ----
## Extrapolation ----
test_that("Imputation is working for extrapolated aggregated distribution", {
  tmp <- fg_pip(
    country = "CHN",
    year = 1988,
    povline = 1.9,
    popshare = NULL,
    welfare_type = "all",
    reporting_level = "all",
    ppp = NULL,
    debug = FALSE,
    lkup = lkups
  )

  expect_equal(nrow(tmp), 2)

  tmp <- fg_pip(
    country = "CHN",
    year = 1988,
    povline = 1.9,
    popshare = NULL,
    welfare_type = "all",
    reporting_level = "national",
    ppp = NULL,
    debug = FALSE,
    lkup = lkups
  )

  expect_equal(nrow(tmp), 2)
})

## Interpolation ----
test_that("Imputation is working for interpolated mixed distribution", {
  tmp <- fg_pip(
    country = "IND",
    year = 1993,
    povline = 1.9,
    popshare = NULL,
    welfare_type = "all",
    reporting_level = "all",
    ppp = NULL,
    debug = FALSE,
    lkup = lkups
  )

  expect_equal(nrow(tmp), 2)

  tmp <- fg_pip(
    country = "IND",
    year = 1993,
    povline = 1.9,
    popshare = NULL,
    welfare_type = "all",
    reporting_level = "national",
    ppp = NULL,
    debug = FALSE,
    lkup = lkups
  )

  expect_equal(nrow(tmp), 2)
})

test_that("Imputation is working for interpolated aggregate distribution", {
  tmp <- fg_pip(
    country = "CHN",
    year = 2000,
    povline = 1.9,
    popshare = NULL,
    welfare_type = "all",
    reporting_level = "all",
    ppp = NULL,
    debug = FALSE,
    lkup = lkups
  )

  expect_equal(nrow(tmp), 2)

  tmp <- fg_pip(
    country = "CHN",
    year = 2000,
    povline = 1.9,
    popshare = NULL,
    welfare_type = "all",
    reporting_level = "national",
    ppp = NULL,
    debug = FALSE,
    lkup = lkups
  )

  expect_equal(nrow(tmp), 2)
})
