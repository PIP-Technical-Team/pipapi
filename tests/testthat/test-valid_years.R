skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

# constants
tdir <- fs::dir_ls(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))[1]

test_that("valid_years returns expected output type", {

  tmp <- valid_years(tdir)
  expect_equal(class(tmp), "list")
  expect_equal(length(tmp), 2)
  expect_equal(names(tmp), c("valid_survey_years", "valid_interpolated_years"))
})

test_that("valid_years correctly returns valid years", {
  tmp <- valid_years(tdir)
  # Survey years
  expect_equal(min(tmp$valid_survey_years), 1967)
  expect_equal(max(tmp$valid_survey_years), 2021)
  expect_equal(length(tmp$valid_survey_years), 50)
  # Interpolated years
  expect_equal(tmp$valid_interpolated_years, 1981:2019)
})

