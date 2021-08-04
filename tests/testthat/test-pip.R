lkups <- pipapi:::clean_api_data(
  data_folder_root = "../testdata/20210401/")

# Check output type ----
test_that("output type is correct", {

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = lkups)

  expect_equal(class(tmp), c("data.table", "data.frame"))
})

# Check selections ----

## Welfare type ----
test_that("welfare_type selection are correct", {

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = lkups,
             welfare_type = "all")

  expect_equal(sort(unique(tmp$welfare_type)), c("consumption", "income"))

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = lkups,
             welfare_type = "consumption")

  expect_equal(unique(tmp$welfare_type), "consumption")

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = lkups,
             welfare_type = "income")

  expect_equal(unique(tmp$welfare_type), "income")
})
## Reporting level ----
test_that("reporting_level selection are correct", {

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = lkups,
             reporting_level = "all")

  expect_equal(sort(unique(tmp$pop_data_level)), c("national", "rural", "urban"))

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = lkups,
             reporting_level = "national")

  expect_equal(sort(unique(tmp$pop_data_level)), c("national"))

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = lkups,
             reporting_level = "rural")

  expect_equal(sort(unique(tmp$pop_data_level)), c("rural"))

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = lkups,
             reporting_level = "urban")

  expect_equal(sort(unique(tmp$pop_data_level)), c("urban"))
})

# Check aggregation ----
test_that("Aggregation is working", {
  skip("Aggregation not correctly implemented")
  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             aggregate = TRUE,
             lkup = lkups)
  expect_equal(nrow(tmp), 1)
})


# Check imputation ----
test_that("Imputation is working", {
  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             fill_gaps = TRUE,
             lkup = lkups)

  expect_equal(nrow(tmp), 182)
})


# Check regional aggregations
test_that("Regional aggregations are working", {
  tmp <- pip(country = "all",
             year    = 2008,
             group_by = "wb",
             povline = 3.5,
             lkup = lkups)

  expect_equal(nrow(tmp), 3) # Should be changed if lkups are updated. Full set of regions is 8.
})

# Check pop_share
test_that("pop_share option is working", {
  tmp <- pip(country = "AGO",
             year    = 2000,
             popshare = .2,
             lkup = lkups)

  expect_equal(nrow(tmp), 1)
})
