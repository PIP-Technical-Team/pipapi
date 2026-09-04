test_that("cache-keys returns an empty response when caching is disabled", {
  skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")
  skip_if(Sys.getenv("PIPAPI_TEST_PLUMBER") != "TRUE")

  api <- callr::r_session$new(
    options = callr::r_session_options(user_profile = FALSE)
  )
  withr::defer(api$kill())

  api$run(function() Sys.setenv("PIPAPI_APPLY_CACHING" = "FALSE"))
  api$call(
    function() {
      library(pipapi)
      lkups <<- pipapi::create_versioned_lkups(
        Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")
      )
      pipapi::start_api(port = 8002)
    },
    package = TRUE
  )
  Sys.sleep(25)

  response <- httr::GET(
    "http://localhost",
    port = 8002,
    path = "api/v1/cache-keys"
  )

  expect_equal(response$status_code, 200)
  expect_length(httr::content(response, encoding = "UTF-8"), 0L)
})
