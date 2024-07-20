# skip_if(Sys.getenv('WBPIP_RUN_LOCAL_TESTS') != "TRUE")
# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.

data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")

skip_if(data_dir == "")

latest_version <-
  available_versions(data_dir) |>
  max()

lkups <- create_versioned_lkups(data_dir,
                                vintage_pattern = latest_version)
lkup <- lkups$versions_paths[[lkups$latest_release]]

local_mocked_bindings(
  get_caller_names = function() c("else")
)

# aggregated distribution ----
## Extrapolation ----
test_that("Imputation is working for extrapolated aggregated distribution", {
  tmp <- fg_pip(
    country         = "CHN",
    year            = 1988,
    povline         = 1.9,
    popshare        = NULL,
    welfare_type    = "all",
    reporting_level = "all",
    ppp             = NULL,
    lkup            = lkup
  )

  expect_equal(nrow(tmp), 2)

  tmp <- fg_pip(
    country         = "CHN",
    year            = 1988,
    povline         = 1.9,
    popshare        = NULL,
    welfare_type    = "all",
    reporting_level = "national",
    ppp             = NULL,
    lkup            = lkup
  )

  expect_equal(nrow(tmp), 2)
})

## Interpolation ----
test_that("Imputation is working for interpolated mixed distribution", {
  tmp <- fg_pip(
    country         = "IND",
    year            = 1993,
    povline         = 1.9,
    popshare        = NULL,
    welfare_type    = "all",
    reporting_level = "all",
    ppp             = NULL,
    lkup            = lkup
  )

  expect_equal(nrow(tmp), 2)

  tmp <- fg_pip(
    country         = "IND",
    year            = 1993,
    povline         = 1.9,
    popshare        = NULL,
    welfare_type    = "all",
    reporting_level = "national",
    ppp             = NULL,
    lkup            = lkup
  )

  expect_equal(nrow(tmp), 2)
})

test_that("Imputation is working for interpolated aggregate distribution", {
  tmp <- fg_pip(
    country         = "CHN",
    year            = 2000,
    povline         = 1.9,
    popshare        = NULL,
    welfare_type    = "all",
    reporting_level = "all",
    ppp             = NULL,
    lkup            = lkup
  )

  expect_equal(nrow(tmp), 2)

  tmp <- fg_pip(
    country         = "CHN",
    year            = 2000,
    povline         = 1.9,
    popshare        = NULL,
    welfare_type    = "all",
    reporting_level = "national",
    ppp             = NULL,
    lkup            = lkup
  )

  expect_equal(nrow(tmp), 2)
})


test_that("Check classes of function fg_assign_nas_values_to_dup_cols", {
  df <- data.table::data.table(a = rnorm(5), b = letters[1:5], c = 1:5, d = rnorm(5))
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
  expect_type(res$data_interpolation_id, "character")
})


# SPL and median --------------
tmp <- fg_pip(
  country         = "ALL",
  year            = "ALL",
  povline         = 2.15,
  popshare        = NULL,
  welfare_type    = "all",
  reporting_level = "all",
  ppp             = NULL,
  lkup            = lkup
)

# dt <- pip(country =  "ALL",
#           lkup = lkup,
#           povline         = 2.15,
#           fill_gaps = TRUE)
# setDT(dt)


censored <- lkup$censored$countries


## no unexpected NAs ------------
test_that("NAs only in censored data", {


  ### SPR ---------------
  expect_equal(
    tmp[is.na(spr)][!censored,
                       on = c("country_code",
                              "reporting_year",
                              "reporting_level",
                              "welfare_type")] |>
      nrow(),
    expected = 0)

})

## Duplicates -------------
test_that("median does not have duplicates", {

  ### by reporting level----------------
    anyDuplicated(tmp[!is.na(median),
             c("country_code",
               "reporting_year",
               "welfare_type",
               # "reporting_level",
               "median")]) |>
  expect_equal(0)

  ### by welfare type -------------
    anyDuplicated(tmp[!is.na(median),
             c("country_code",
               "reporting_year",
               # "welfare_type",
               "reporting_level",
               "median")]) |>
    expect_equal(0)

})

test_that("SPR does not have duplicates", {

  ### by reporting level----------------
    anyDuplicated(tmp[!is.na(spr),
             c("country_code",
               "reporting_year",
               "welfare_type",
               # "reporting_level",
               "spr")]) |>
  expect_equal(0)

  ### by welfare type -------------
    anyDuplicated(tmp[!is.na(spr),
             c("country_code",
               "reporting_year",
               # "welfare_type",
               "reporting_level",
               "spr")]) |>
    expect_equal(0)

})


test_that("SPL is the same by reporting level", {


  no_na <-
    tmp[!is.na(spl),
      c("country_code",
        "reporting_year",
        "welfare_type",
        "reporting_level",
        "spl")
      ]

  no_na[, .N, by = c("country_code",
                     "reporting_year",
                     "welfare_type",
                     "reporting_level",
                     "spl")][,N] |>
    expect_equal(
      no_na[, .N, by = c("country_code",
                         "reporting_year",
                         "welfare_type",
                         "reporting_level")][,N]
    )
})


