library(callr)
library(httr)

# Setup by starting APIs
root_path <- "http://localhost"
data_folder_root <- "C:/Users/wb499754/OneDrive - WBG/my_packages/pipapi/TEMP/output/20210401/"
# CAUTION: data_folder_root is also hard-coded on line 14 below. (passing data_folder_root fails)
# MAKE SURE BOTH ARE IN SYNC

api1 <- callr::r_session$new()

Sys.sleep(5)

api1$call(function() {
  # Use double assignment operator so the lkups object is available in the global
  # environment of the background R session, so it is available for the API
  lkups <<- pipapi:::clean_api_data(
    data_folder_root = "C:/Users/wb499754/OneDrive - WBG/my_packages/pipapi/TEMP/output/20210401/")

  plbr_file <- system.file("plumber", "v1", "plumber.R", package = "pipapi")
  pr <- plumber::plumb(plbr_file)
  pr$run(port = 8000)})


Sys.sleep(10)

test_that("Poverty line endpoint is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/poverty-lines")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(names(tmp_resp[[1]]), c("poverty_line","is_default", "is_visible"))
})

test_that("Indicators master endpoint is working", {
  # Send API request
  r <- httr::GET(root_path, port = 8000, path = "api/v1/indicators")

  # Check response
  tmp_resp <- httr::content(r, encoding = "UTF-8")
  expect_equal(names(tmp_resp[[1]]), c("indicator_code",
                                       "indicator_name",
                                       "scale_data",
                                       "scale_display",
                                       "number_of_decimals",
                                       "indicator_definition_short",
                                       "indicator_definition_long",
                                       "key_indicator_template",
                                       "is_sensitive_to_povline",
                                       "symbol",
                                       "sort_order",
                                       "tags"))
})

# Kill process
api1$kill()


