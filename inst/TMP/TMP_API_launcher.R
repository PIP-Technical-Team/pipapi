lkups <- pipapi:::clean_api_data(
  data_folder_root = Sys.getenv('DATA_FOLDER_ROOT'))

start_api(api_version = "v1",
          port = 80,
          host = "0.0.0.0")

# test_that("Plumber working directi", {
#   # Send API request
#   r <- httr::GET(root_path, port = 8000, path = "api/v1/get-root")
#
#   # Check response
#   r$status_code
#   httr::content(r, encoding = "UTF-8")
#
#   tmp <- function() {
#     x <<- 3
#   }
