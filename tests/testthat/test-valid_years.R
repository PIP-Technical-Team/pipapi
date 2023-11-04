skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

# constants
tdir <- fs::dir_ls(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))[1]
tmp <- valid_years(tdir)

test_that("valid_years returns expected output type", {
  expect_equal(class(tmp), "list")
  expect_equal(length(tmp), 2)
  expect_equal(names(tmp), c("valid_survey_years", "valid_interpolated_years"))
})

test_that("valid_years returns years as numerical values", {
  # Survey years
  expect_equal(class(tmp$valid_survey_years), "numeric")
  # Interpolated years
  expect_equal(class(tmp$valid_interpolated_years), "numeric")
})

test_that("there is no gaps between interpolated years", {
  gap <- unique(tmp$valid_interpolated_years - (tmp$valid_interpolated_years - 1))
  expect_equal(length(gap), 1)
  expect_equal(gap, 1)
})

test_that("all objects are correctly passed and used", {
  root <- rprojroot::is_r_package
  tmp <- lintr::lint(filename = root$find_file("R/valid_years.R"),
                     linters = lintr::object_usage_linter())

  # NSE in data.table causing two lines to be flagged
  expect_equal(length(tmp), 0)
})
