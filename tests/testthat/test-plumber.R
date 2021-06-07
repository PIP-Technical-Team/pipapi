# Setup by starting APIs
root_path <- "http://localhost"
# CAUTION: TESTS WILL CURRENTLY FAILS DUE TO PATH ISSUES WITH THE DATA FOLDER
# IN `plumber.R#8: lkups <- pipapi:::clean_api_data(data_folder_root = "../../../TEMP/output/20210401/")`
# api1 <- callr::r_bg(
#   function() {
#     plbr_file <- system.file("plumber", "v1", "plumber.R", package = "pipapi")
#     pr <- plumber::plumb(plbr_file)
#     pr$run(port = 8000)
#   }
# )
#
# Sys.sleep(10)

# test_that("API is alive", {
#   skip('API fails to build in background process')
#   expect_true(api1$is_alive())
# })
#
# test_that("Health check works", {
#   # Send API request
#   r <- httr::GET(root_path, port = 8000, path = "api/v1/health-check")
#
#   # Check response
#   skip('API fails to build in background process')
#   expect_equal(r$status_code, 200)
#   skip('API fails to build in background process')
#   expect_equal(httr::content(r, encoding = "UTF-8"), list("PIP API is running"))
# })
#
# teardown({
#   api1$kill()
# })

