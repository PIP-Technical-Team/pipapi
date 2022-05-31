test_that("valid_years correctly returns the valid years", {
  expect_equal(valid_years('../pip-fake-data/20200101_2011_01_01_PROD/'), 1981:2019)
  expect_equal(valid_years('../pip-fake-data/20211212_2011_01_01_PROD/'), 1981:2019)
})

