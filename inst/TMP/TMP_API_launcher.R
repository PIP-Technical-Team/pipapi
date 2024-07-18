# install()
# Make sure the last version of the package is installed to
# work properly
library(pipapi)
# devtools::load_all(".")
if (Sys.info()[["user"]] == "wb384996") {
  force <- FALSE
  if (!"lkups" %in% ls() || isTRUE(force)) {
    data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
      fs::path()
    fs::dir_ls(data_dir, recurse = FALSE)
  }


  latest_version <-
    pipapi:::available_versions(data_dir) |>
    max()

  lkups <- create_versioned_lkups(data_dir,
                                  vintage_pattern = latest_version)
  # lkup <- lkups$versions_paths[[lkups$latest_release]]

  start_api(port = 8080)
  # start_api(port = 3711)

} else {
  lkups <- pipapi::create_versioned_lkups(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))
  start_api(api_version = "v1",
            port = 80,
            host = "0.0.0.0")

}



