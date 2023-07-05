# lkups <- pipapi::create_versioned_lkups(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))
force <- FALSE
if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)
}


lkups <- create_versioned_lkups(data_dir = data_dir,
                                vintage_pattern = "^20230328")

# lkup <-  lkups$versions_paths$`20230328_2011_02_02_PROD`
# lkup <-  lkups$versions_paths[[lkups$latest_release]]



# start_api(api_version = "v1",
#           port = 80,
#           host = "0.0.0.0")

start_api(port = 8080)
