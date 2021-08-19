library(callr)
library(httr)

# Setup by starting APIs
root_path <- "http://localhost"
data_folder_root <- Sys.getenv('PIPAPI_DATA_ROOT_FOLDER')
# CAUTION: data_folder_root is also hard-coded on line 14 below. (passing data_folder_root fails)
# MAKE SURE BOTH ARE IN SYNC

api1 <- callr::r_session$new()

Sys.sleep(5)

api1$call(function() {
  # Use double assignment operator so the lkups object is available in the global
  # environment of the background R session, so it is available for the API
  lkups <<- pipapi:::clean_api_data(
    data_folder_root = Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))

  plbr_file <- system.file("plumber", "v1", "plumber.R", package = "pipapi")
  pr <- plumber::plumb(plbr_file)
  pr$run(port = 8000)})


Sys.sleep(10)

test_that("API is alive", {
  expect_true(api1$is_alive())
})


test_that("API is running", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/health-check")

  # Check response
  expect_equal(r$status_code, 200)
  expect_equal(httr::content(r, encoding = "UTF-8"), list("PIP API is running"))
})

test_that("Data folder path is correctly set up", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/info")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(names(tmp_resp), c("valid_query_parameters", "packages_version", "data_versions"))
  expect_equal(tmp_resp$data_versions[[1]], paste0(data_folder_root, "estimations"))
})

test_that("Necessary objects are available in the API environment", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/get-available-objects")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(tmp_resp$global[[1]], "lkups")
})

test_that("Basic PIP request is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2018")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(tmp_resp[[1]]$reporting_year, 2018)
})

test_that("Interpolated PIP request is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2012&fill_gaps=true")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(tmp_resp[[1]]$reporting_year, 2012)
})

test_that("Serializer formats are working", {

  # Check json
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2000&format=json")
  expect_equal(httr::http_type(r),"application/json")

  # Check that default is json
  r2 <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2000")
  expect_equal(httr::http_type(r), httr::http_type(r2))
  expect_equal(httr::content(r, encoding = "UTF-8"), httr::content(r2, encoding = "UTF-8"))

  # Check csv
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2000&format=csv")
  expect_equal(httr::headers(r)$`content-type`, "text/csv; charset=UTF-8")

  # Check rds
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2000&format=rds")
  expect_equal(httr::http_type(r), "application/rds")

})

test_that("Indicator names are correct", {
  # Send pip API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2018")
  # Check response
  pip_resp <- httr::content(r, encoding = "UTF-8")

  # Retrieve indicators master
  r <- httr::GET(root_path, port = 8000, path = "api/v1/indicators")
  # Check response
  ind_resp <- httr::content(r, encoding = "UTF-8")
  ind_code <- purrr::map_chr(ind_resp, "indicator_code")
  expect_equal(sum(names(pip_resp[[1]]) %in% ind_code), 21)
})

# Kill process
api1$kill()


