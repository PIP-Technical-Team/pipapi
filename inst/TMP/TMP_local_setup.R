# Set-up to test locally within a given branch

# 1. Install the package in given branch ----
library(devtools)
devtools::install_github("PIP-Technical-Team/pipapi@no-store") # this is for the given branch @no-store

# 2. Load the packages ----
library(pipapi)
library(fastverse)

# 3. Set Env Variables ----
## Note: (This won't run unless you are in the Remote Computer)
Sys.setenv(
  PIP_ROOT_DIR = "//wbgmsddg001/pip/pipapi_data",
  PIPAPI_DATA_ROOT_FOLDER_LOCAL =fs::path("e:/PIP/pipapi_data/"),
  PIPAPI_APPLY_CACHING = FALSE
)

# 4. Prepare lkup(s) ----
## Note: we set to latest release ----
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
pipapi:::start_api(api_version = "v1", port = 8080)

# 5. Move to postman to test API itself.
# 6. Move to pipr to test specific features.
