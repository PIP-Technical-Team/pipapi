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

test_that("all objects are correctly passed and used", {
  root <- rprojroot::is_r_package
  tmp <- lintr::lint(filename = root$find_file("R/create_lkups.R"),
                     linters = lintr::object_usage_linter())

  expect_equal(length(tmp), 0)
})
