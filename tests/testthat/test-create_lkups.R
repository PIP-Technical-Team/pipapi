dirs_names <- c("00000001",
                "20210401",
                "20210920_2011_02_02_PROD",
                "20220317_2011_02_02_INT",
                "20220317_2017_01_01_INT",
                "20220408",
                "20220408_2011_02_02_PROD")


test_that("pattern list is created correctly", {

  vp <- get_vintage_pattern_regex()

  expect_equal(object = vp,
               expected =  list(
                 vintage_pattern = "\\d{8}_\\d{4}_\\d{2}_\\d{2}_(PROD|TEST|INT)$",
                 prod_regex = "PROD$",
                 int_regex = "INT$",
                 test_regex = "TEST$")
               )


  vp <- get_vintage_pattern_regex(vintage_pattern = "\\.*",
                                  int_regex = "foo$")

  expect_equal(object = vp,
               expected =  list(
                 vintage_pattern = "\\.*",
                 prod_regex = "PROD$",
                 int_regex = "foo$",
                 test_regex = "TEST$")
               )


})


test_that("create vintanger pattern call is working fine", {

  # test NULL
  vintage_pattern <- NULL
  cvp <- create_vintage_pattern_call(vintage_pattern)
  vp  <- get_vintage_pattern_regex()
  expect_equal(cvp, vp)

  # Test list
  vintage_pattern <- list("r.*", "", "^hjkhj\\.d")
  cvp <- create_vintage_pattern_call(vintage_pattern)
  expect_equal(cvp, list(vintage_pattern = "r.*",
                         prod_regex = "",
                         int_regex = "^hjkhj\\.d",
                         test_regex = "TEST$"))


  # Test unnamed character vector
  vintage_pattern <- c("r.*", "", "^hjkhj\\.d")
  cvp <- create_vintage_pattern_call(vintage_pattern)
  expect_equal(cvp, list(vintage_pattern = "r.*",
                         prod_regex = "",
                         int_regex = "^hjkhj\\.d",
                         test_regex = "TEST$"))


  # test named chacter vector
  vintage_pattern <- c(vintage_pattern = "r.*", test_regex = "", int_regex =  "^hjkhj\\.d")
  cvp <- create_vintage_pattern_call(vintage_pattern)
  expect_equal(cvp, list(vintage_pattern = "r.*",
                         prod_regex = "PROD$",
                         int_regex = "^hjkhj\\.d",
                         test_regex = ""))

})




vintage_patterns <- get_vintage_pattern_regex()

test_that("id_valid_dirs correctly identifies valid directories", {

  out <- id_valid_dirs(dirs_names = dirs_names,
                       vintage_pattern = vintage_patterns$vintage_pattern)

  expect_equal(out, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
})


test_that("sort_versions correctly orders available versions", {
  versions <- c("20210920_2011_02_02_PROD",
                "20220317_2011_02_02_INT",
                "20220317_2017_01_01_INT",
                "20220408_2011_02_02_PROD")
  expected_sorted_versions <- c("20220408_2011_02_02_PROD",
                                "20210920_2011_02_02_PROD",
                                "20220317_2017_01_01_INT",
                                "20220317_2011_02_02_INT")

  out <- sort_versions(versions = versions,
                       prod_regex = vintage_patterns$prod_regex,
                       int_regex  = vintage_patterns$int_regex,
                       test_regex = vintage_patterns$test_regex)

  expect_equal(out, expected_sorted_versions)
})

# test_that("all objects are correctly passed and used", {
#   root <- rprojroot::is_r_package
#   tmp <- lintr::lint(filename = root$find_file("R/create_lkups.R"),
#                      linters = lintr::object_usage_linter())
#
#   expect_equal(length(tmp), 0)
# })


# ifel_isnull() --------------------------------------------------------------

test_that("ifel_isnull: returns y when x is NULL", {
  expect_equal(pipapi:::ifel_isnull(NULL, "default"), "default")
})

test_that("ifel_isnull: returns x when x is not NULL", {
  expect_equal(pipapi:::ifel_isnull("value", "default"), "value")
})

test_that("ifel_isnull: works with numeric types", {
  expect_equal(pipapi:::ifel_isnull(NULL, 42L), 42L)
  expect_equal(pipapi:::ifel_isnull(0, 42L), 0)
})


# use_new_lineup_version() ---------------------------------------------------

test_that("use_new_lineup_version: returns FALSE for dates before threshold", {
  expect_false(use_new_lineup_version("20250401_2021_01_02_PROD"))
  expect_false(use_new_lineup_version("20250501_2021_01_02_PROD")) # threshold is >
})

test_that("use_new_lineup_version: returns TRUE for dates after threshold", {
  expect_true(use_new_lineup_version("20250502_2021_01_02_PROD"))
  expect_true(use_new_lineup_version("20250930_2021_01_02_PROD"))
  expect_true(use_new_lineup_version("20260101_2021_01_02_PROD"))
})

test_that("use_new_lineup_version: is vectorised", {
  x <- c("20250401_2021_01_02_PROD", "20250930_2021_01_02_PROD")
  result <- use_new_lineup_version(x)
  expect_equal(result, c(FALSE, TRUE))
})

test_that("use_new_lineup_version: TEST_VINTAGE triggers new path", {
  expect_true(use_new_lineup_version(TEST_VINTAGE))
})


# id_valid_dirs() edge cases -------------------------------------------------

test_that("id_valid_dirs: all-invalid names returns all FALSE", {
  out <- id_valid_dirs(dirs_names = c("foo", "bar", "123"),
                       vintage_pattern = vintage_patterns$vintage_pattern)
  expect_true(all(!out))
})

test_that("id_valid_dirs: empty input returns empty logical", {
  out <- id_valid_dirs(dirs_names = character(0),
                       vintage_pattern = vintage_patterns$vintage_pattern)
  expect_equal(length(out), 0L)
  expect_type(out, "logical")
})

test_that("id_valid_dirs: TEST suffix is valid", {
  out <- id_valid_dirs(dirs_names = "20220317_2011_02_02_TEST",
                       vintage_pattern = vintage_patterns$vintage_pattern)
  expect_true(out)
})


# sort_versions() edge cases -------------------------------------------------

test_that("sort_versions: PROD versions sorted newest-first", {
  versions <- c("20210101_2011_01_01_PROD", "20220101_2011_01_01_PROD")
  out <- sort_versions(versions,
                       prod_regex = vintage_patterns$prod_regex,
                       int_regex  = vintage_patterns$int_regex,
                       test_regex = vintage_patterns$test_regex)
  expect_equal(out[1], "20220101_2011_01_01_PROD")
})

test_that("sort_versions: PROD before INT before TEST", {
  versions <- c("20220101_2011_01_01_INT",
                "20220101_2011_01_01_TEST",
                "20220101_2011_01_01_PROD")
  out <- sort_versions(versions,
                       prod_regex = vintage_patterns$prod_regex,
                       int_regex  = vintage_patterns$int_regex,
                       test_regex = vintage_patterns$test_regex)
  expect_equal(out[1], "20220101_2011_01_01_PROD")
  expect_equal(out[2], "20220101_2011_01_01_INT")
  expect_equal(out[3], "20220101_2011_01_01_TEST")
})

test_that("sort_versions: empty input returns empty character", {
  out <- sort_versions(character(0),
                       prod_regex = vintage_patterns$prod_regex,
                       int_regex  = vintage_patterns$int_regex,
                       test_regex = vintage_patterns$test_regex)
  expect_equal(length(out), 0L)
})


# create_return_cols() -------------------------------------------------------

test_that("create_return_cols: returns a named list", {
  result <- create_return_cols(
    pip     = list(cols = c("headcount", "poverty_gap")),
    pip_grp = list(cols = c("region_code"))
  )
  expect_type(result, "list")
  expect_named(result, c("pip", "pip_grp"))
})

test_that("create_return_cols: preserves inner structure", {
  cols <- c("headcount", "mean")
  result <- create_return_cols(pip = list(cols = cols))
  expect_equal(result$pip$cols, cols)
})

test_that("create_return_cols: empty call returns empty list", {
  result <- create_return_cols()
  expect_equal(length(result), 0L)
})
