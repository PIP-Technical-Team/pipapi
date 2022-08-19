# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "" ||
          Sys.getenv("PIPAPI_TEST_PLUMBER") != "TRUE")

library(callr)
library(httr)

# Setup external R session
api1 <- callr::r_session$new(options = callr::r_session_options(user_profile = FALSE))
Sys.sleep(2)
api1$run(function() Sys.setenv("PIPAPI_APPLY_CACHING" = "TRUE"))
api1$run(function() Sys.setenv("PIPAPI_CACHE_MAX_SIZE" = 1024^2))
api1$run(function() Sys.setenv("R_USER_CACHE_DIR" = tempdir()))

# Check env vars
test_that("Environment is set up correctly", {
  expect_identical(api1$run(function() Sys.getenv("PIPAPI_APPLY_CACHING")), "TRUE")
  expect_identical(api1$run(function() Sys.getenv("PIPAPI_CACHE_MAX_SIZE")), "1048576")
  expect_true(api1$run(function() Sys.getenv("R_USER_CACHE_DIR")) != Sys.getenv("R_USER_CACHE_DIR"))
})

# Start external API process
root_path <- "http://localhost"
api1$call(function() {
  library(pipapi)
  # Use double assignment operator so the lkups object is available in the global
  # environment of the background R session, so it is available for the API
  lkups <<- pipapi::create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
  pipapi::start_api(port = 8000)
}, package = TRUE)
Sys.sleep(25)

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

test_that("Caching setup is correct", {
  r <- httr::GET(root_path, port = 8000, path = "api/v1/cache-info")
  tmp <- httr::content(r, encoding = "UTF-8")

  # Check response
  expect_equal(r$status_code, 200)

  # Check caching settings
  expect_true(tmp$dir != Sys.getenv("R_USER_CACHE_DIR")) # unit tests should have a separate cache dir
  expect_true(grepl("cache.log$", tmp$logfile)) # log file should be present
  expect_false(tmp$destroy_on_finalize) # don't delete on garbage collection
  expect_equal(tmp$n_items, 0)          # initial caching directory should be empty
  expect_equal(tmp$max_size, 1024^2) # defined w/ PIPAPI_CACHE_MAX_SIZE above
  expect_equal(tmp$max_n, "Inf")     # no max number of items
  expect_equal(tmp$max_age, "Inf")   # no max age
  expect_equal(tmp$evict, "lru")     # lru evict policy should be used
  expect_equal(tmp$prune_rate, 20)   # default prune rate

})

test_that("Caching is activated for /pip", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2000")
  r2 <- httr::GET(root_path, port = 8000, path = "api/v1/cache-info")

  # Check response
  expect_equal(r$status_code, 200)
  expect_equal(httr::content(r2, encoding = "UTF-8")$n_items, 1)
})

test_that("Caching is activated for /pip-grp", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip-grp?country=AGO&year=2000")
  r2 <- httr::GET(root_path, port = 8000, path = "api/v1/cache-info")

  # Check response
  expect_equal(r$status_code, 200)
  expect_equal(httr::content(r2, encoding = "UTF-8")$n_items, 2)
})

test_that("/cache-log is working correctly", {
  # Send API requests
  r1 <- readLines("http://localhost:8000/api/v1/cache-log")
  r2 <- httr::GET(root_path, port = 8000, path = "api/v1/cache-log")
  r3 <- httr::GET(root_path, port = 8000, path = "api/v1/cache-info")

  # Check response
  tmp <- readLines(httr::content(r3, encoding = "UTF-8")$logfile)
  expect_equal(r2$status_code, 200)  # success
  expect_equal(nrow(r1), nrow(tmp))  # same number of lines
})

test_that("/cache-reset is working correctly", {

  # Send request to make sure that at least one item is in the cache
  r <- httr::GET(root_path, port = 8000, path = "api/v1/pip?country=AGO&year=2000")

  # Reset cache
  r <- httr::GET(root_path, port = 8000, path = "api/v1/cache-reset")
  tmp <- httr::content(r, encoding = "UTF-8")

  # Check response
  expect_equal(r$status_code, 200)
  expect_equal(tmp$status, "success")
  expect_equal(tmp$msg, 'Cache cleared.')

  # Check that cache dir is empty after reset
  r <- httr::GET(root_path, port = 8000, path = "api/v1/cache-info")
  tmp <- httr::content(r, encoding = "UTF-8")
  expect_equal(tmp$n_items, 0)

})

# Kill process
api1$kill()
