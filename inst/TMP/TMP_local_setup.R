# Set-up to test locally within a given branch

# 1. Install the package in given branch if you need to, then hashtag and run as source.
#library(devtools)
#devtools::install_github("PIP-Technical-Team/pipapi@no-store")

# 2. Load the packages
library(pipapi)
library(fastverse)

# 3. Prepare lkup
force <- FALSE
if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)
}


latest_version <-
  pipapi:::available_versions(data_dir) |>
  max()

lkups <- pipapi:::create_versioned_lkups(data_dir,
                                         vintage_pattern = latest_version)
lkup <- lkups$versions_paths[[lkups$latest_release]]

# 4. Start the api using start_api()

api_version <- "v1"
version_path <- sprintf(
  "plumber/%s/plumber.R",
  api_version
)
api_path <- system.file(version_path, package = "pipapi")
api <- source(api_path)

host <- "0.0.0.0"
port <- 8080
plumber::pr_run(api$value, host = host, port = port)

#pipapi:::start_api(api_version = "v1", port = 8080)


