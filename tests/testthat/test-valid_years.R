test_that("valid_years returns expected output type", {
  tmp <- valid_years('../testdata/app_data/20210401/')
  expect_equal(class(tmp), "list")
  expect_equal(length(tmp), 2)
  expect_equal(names(tmp), c("valid_survey_years", "valid_interpolated_years"))
})

test_that("valid_years correctly returns valid years", {
  tmp <- valid_years('../testdata/app_data/20210401/')
  # Survey years
  expect_equal(min(tmp$valid_survey_years), 1967)
  expect_equal(max(tmp$valid_survey_years), 2021)
  expect_equal(length(tmp$valid_survey_years), 50)
  # Interpolated years
  expect_equal(tmp$valid_interpolated_years, 1981:2019)
})

