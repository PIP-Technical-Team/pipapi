# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "" ||
          Sys.getenv("PIPAPI_TEST_PLUMBER") != "TRUE")

library(callr)
library(httr)
library(future)
library(future.callr)
library(future.apply)

# Set plan
future::plan("multisession", workers = 2) # n workers for unit tests script

# Setup by starting APIs
root_path <- "http://localhost"
api1 <- future.callr::callr(function(){
  lkups <<- pipapi::create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
  pipapi::start_api(port = 8000)
}, workers = 2) # n workers for API
Sys.sleep(20)


test_that("API is running", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/health-check")

  # Check response
  expect_equal(r$status_code, 200)
  expect_equal(httr::content(r, encoding = "UTF-8"), list("PIP API is running"))
})

test_that("Parallel processing is avaliable", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/n-workers")

  # Check response
  expect_equal(r$status_code, 200)
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(tmp_resp$n_workers, 2)
  expect_equal(tmp_resp$n_free_workers, 2)
  expect_equal(tmp_resp$cores, future::availableCores())
})

test_that("Async parallel processing works for /pip", {
  # Send API request
  paths <- c("api/v1/pip?country=all&year=all", "api/v1/pip?country=ALB&year=2008")
  r <- future_lapply(paths, function(x) httr::GET(root_path, port = 8000, path = x))

  # Check response
  expect_equal(r[[1]]$status_code, 200)
  expect_equal(r[[2]]$status_code, 200)

  # Test that small query completes before all query
  expect_gt(r[[1]]$date, r[[2]]$date)
})

test_that("Async parallel processing works for /pip-grp", {
  # Send API request
  paths <- c("api/v1/pip-grp?country=all&year=all", "api/v1/pip-grp?country=ALB&year=2008")
  r <- future_lapply(paths, function(x) httr::GET(root_path, port = 8000, path = x))

  # Check response
  expect_equal(r[[1]]$status_code, 200)
  expect_equal(r[[2]]$status_code, 200)

  # Test that small query completes before all query
  expect_gt(r[[1]]$date, r[[2]]$date)
})


test_that("Async parallel processing works for /hp-stacked", {
  # Send API request
  paths <- c("api/v1/hp-stacked", "api/v1/health-check")
  r <- future_lapply(paths, function(x) httr::GET(root_path, port = 8000, path = x))

  # Check response
  expect_equal(r[[1]]$status_code, 200)
  expect_equal(r[[2]]$status_code, 200)

  # Test that small query completes before all query
  expect_gt(r[[1]]$date, r[[2]]$date)
})

test_that("Async parallel processing works for /pc-regional", {
  # Send API request
  paths <- c("api/v1/pc-regional-aggregates", "api/v1/health-check")
  r <- future_lapply(paths, function(x) httr::GET(root_path, port = 8000, path = x))

  # Check response
  expect_equal(r[[1]]$status_code, 200)
  expect_equal(r[[2]]$status_code, 200)

  # Test that small query completes before all query
  expect_gt(r[[1]]$date, r[[2]]$date)
})

test_that("Async parallel processing works for /pc-charts", {
  # Send API request
  paths <- c("api/v1/pc-charts?country=all&year=all", "api/v1/pc-charts?country=ALB&year=all")
  r <- future_lapply(paths, function(x) httr::GET(root_path, port = 8000, path = x))

  # Check response
  expect_equal(r[[1]]$status_code, 200)
  expect_equal(r[[2]]$status_code, 200)

  # Test that small query completes before all query
  expect_gt(r[[1]]$date, r[[2]]$date)
})

test_that("Async parallel processing works for /cp-charts", {
  # Send API request
  paths <- c("api/v1/cp-charts?country=all", "api/v1/cp-charts?country=ALB")
  r <- future_lapply(paths, function(x) httr::GET(root_path, port = 8000, path = x))

  # Check response
  expect_equal(r[[1]]$status_code, 200)
  expect_equal(r[[2]]$status_code, 200)

  # Test that small query completes before all query
  expect_gt(r[[1]]$date, r[[2]]$date)
})


# Close workers by switching plan
future::plan(future::sequential)

# Kill process
api1$kill()
