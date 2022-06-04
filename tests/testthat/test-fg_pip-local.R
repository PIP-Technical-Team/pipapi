skip_if(Sys.getenv('WBPIP_RUN_LOCAL_TESTS') != "TRUE")
# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
lkups <-lkups$versions_paths[[lkups$latest_release]]

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


test_that("Check classes of function fg_assign_nas_values_to_dup_cols", {
  tmp <- data.table::data.table(a = rnorm(5), b = letters[1:5], c = 1:5, d = rnorm(5))
  tmp <- fg_assign_nas_values_to_dup_cols(df, c('a', 'b', 'c'))

  expect_type(tmp$a, "double")
  expect_type(tmp$b, "character")
  expect_type(tmp$c, "integer")
})


test_that("Test fg_standardize_cache_id", {
  x <- c("CHN_1981_CRHS-CUHS_D2_INC_GROUP", "CHN_1981_CRHS-CUHS_D2_INC_GROUP",
         "CHN_1981_CRHS-CUHS_D2_INC_GROUP", "CHN_1981_CRHS-CUHS_D2_INC_GROUP",
         "CHN_1981_CRHS-CUHS_D2_INC_GROUP", "CHN_1981_CRHS-CUHS_D2_INC_GROUP")
  y <- fg_standardize_cache_id(x, x, '')

  expect_length(y, length(x))
  expect_identical(x, y)
})


test_that("fg_remove_duplicates test", {
  df <- data.table::data.table(cache_id = c("CHN_1981_CRHS-CUHS_D2_INC_GROUP", "CHN_1981_CRHS-CUHS_D2_INC_GROUP",
                                            "CHN_1981_CRHS-CUHS_D2_INC_GROUP", "CHN_1981_CRHS-CUHS_D2_INC_GROUP",
                                            "CHN_1981_CRHS-CUHS_D2_INC_GROUP", "CHN_1981_CRHS-CUHS_D2_INC_GROUP"),
                               data_interpolation_id = c("CHN_1981_CRHS-CUHS_D2_INC_GROUP_rural", "CHN_1981_CRHS-CUHS_D2_INC_GROUP_rural",
                                                         "CHN_1981_CRHS-CUHS_D2_INC_GROUP_rural", "CHN_1981_CRHS-CUHS_D2_INC_GROUP_urban",
                                                         "CHN_1981_CRHS-CUHS_D2_INC_GROUP_urban", "CHN_1981_CRHS-CUHS_D2_INC_GROUP_urban"),
                               reporting_level = c("rural", "rural", "rural", "urban", "urban", "urban"))

  res <- fg_remove_duplicates(df, c('data_interpolation_id'))
  expect_equal(nrow(res), 2)
  expect_type(tmp$data_interpolation_id, "character")
})
