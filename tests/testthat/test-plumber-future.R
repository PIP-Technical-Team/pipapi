# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "" ||
          Sys.getenv("PIPAPI_TEST_PLUMBER") != "TRUE")

#library(callr)
library(httr)
library(future)
library(future.apply)
library(future.callr)


# Set plan
future::plan("multisession", workers = 2) # n workers for unit tests script

# Setup by starting APIs
root_path <- "http://localhost"
api1 <- future.callr::callr(function() {
  library(pipapi)
  lkups <<- pipapi::create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
  pipapi::start_api(port = 8000)
}, workers = 2, # n workers for API
   #globals = list(lkups = lkups),
   #packages = c("pipapi")
)

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
  expect_equal(tmp_resp$n_workers, 2) # api1$workers
  expect_equal(tmp_resp$n_free_workers, 2) # api1$workers
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


test_that("Serialization works in parallel mode'", {
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

# Close workers by switching plan
future::plan(future::sequential)

# Kill process
rm(api1)
# api1$kill()
