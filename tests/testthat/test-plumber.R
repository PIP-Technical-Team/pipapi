# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "" ||
          Sys.getenv("PIPAPI_TEST_PLUMBER") != "TRUE")

lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
lkups <- lkups$versions_paths[[lkups$latest_release]]

library(callr)
library(httr)
library(jsonlite)

# Setup by starting APIs
root_path <- "http://localhost"
api1 <- callr::r_session$new(options = callr::r_session_options(user_profile = FALSE))
Sys.sleep(5)
api1$call(function() {
  # Use double assignment operator so the lkups object is available in the global
  # environment of the background R session, so it is available for the API
  lkups <<- pipapi::create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
  pipapi::start_api(port = 8000)
})
Sys.sleep(30)

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

test_that("/pip-info returns correctly formatted response", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip-info")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(names(tmp_resp),
               c("available_data_versions", "package_versions", "r_version",
                 "server_os", "server_time"))
})

test_that("Necessary objects are available in the API environment", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/get-available-objects")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  skip("get-available-objects endpoint has been disabled")
  expect_equal(tmp_resp$global[[3]], "lkups")
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

test_that("Aggregated PIP request is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip-grp?country=all&year=2012&group_by=wb")

  # Check response
  # tmp_resp <- httr::content(r, encoding = "UTF-8")
  tmp_resp <- fromJSON(rawToChar(r$content))
  expect_equal(unique(tmp_resp$reporting_year), 2012)
  expect_equal(nrow(tmp_resp), 8)
})

test_that("/pip?country=all is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=all")
  # Check response
  tmp_resp <- fromJSON(rawToChar(r$content))
  expect_gte(nrow(tmp_resp), 2000)
})

test_that("/pip-grp?country=all is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip-grp?country=all")
  # Check response
  tmp_resp <- fromJSON(rawToChar(r$content))
  expect_equal(nrow(tmp_resp),
               length(unique(lkups$ref_lkup$reporting_year)))
})

test_that("/pip returns a 404 HTTP response if an invalid parameter argument is specified", {
  # Invalid country code
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=KSV")
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(r$status_code, 404)
  expect_identical(names(tmp_resp), c("error", "details"))

  # Invalid year
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2050")
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(r$status_code, 404)
  expect_identical(names(tmp_resp), c("error", "details"))
})

test_that("/pip-grp returns a 404 HTTP response if an invalid parameter argument is specified", {
  # Invalid country code
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip-grp?country=KSV")
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(r$status_code, 404)
  expect_identical(names(tmp_resp), c("error", "details"))

  # Invalid year
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip-grp?country=AGO&year=2050")
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(r$status_code, 404)
  expect_identical(names(tmp_resp), c("error", "details"))
})

test_that("/pip-grp returns a 400 HTTP response if country=all is used together with a predefined subgroup", {
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip-grp?country=AGO&group_by=wb")
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(r$status_code, 400)
  expect_identical(names(tmp_resp), c("error", "details"))
})

test_that("Serializer formats are working", {
  # Check json
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2000&format=json")
  expect_equal(httr::http_type(r), "application/json")

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


test_that("Serializer formats are working when both counntry & year = 'all'", {
  # Check json
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=all&year=all&format=json")
  expect_equal(httr::http_type(r), "application/json")

  # Check that default is json
  r2 <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=all&year=all")
  expect_equal(httr::http_type(r), httr::http_type(r2))
  expect_equal(httr::content(r, encoding = "UTF-8"), httr::content(r2, encoding = "UTF-8"))

  # Check csv
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=all&year=all&format=csv")
  expect_equal(httr::headers(r)$`content-type`, "text/csv; charset=UTF-8")

  # Check rds
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=all&year=all&format=rds")
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
