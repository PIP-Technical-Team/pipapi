# SETUP  -------------

devtools::load_all(".")
library(fastverse)
force <- FALSE

root_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path()
fs::dir_ls(root_dir, recurse = FALSE)

#
latest_version <-
  available_versions(root_dir) |>
  max()

# lkups <- create_versioned_lkups(root_dir,
#                                 vintage_pattern = "^202509.+2017.+(PROD)$")
lkups <- create_versioned_lkups(root_dir,
                                vintage_pattern = latest_version)

# lkup <- lkups$versions_paths[[lkups$versions[[2]]]]
ver_to_use <- lkups$latest_release # this is important. You need this object below

lkup <- lkups$versions_paths[[ver_to_use]]



# DEGUB -------------


options(pipapi.query_live_data = FALSE)
getOption("pipapi.query_live_data")

reset_cache(lkup = lkup)

tictoc::tic()
sv <- pip(country = "ALL",
          year = "ALL",
          povline = lkup$pl_lkup$poverty_line,
          lkup = lkup,
          fill_gaps = FALSE)
tictoc::toc()


tictoc::tic()
fg <- pip(country = "ALL",
          year = "ALL",
          povline = lkup$pl_lkup$poverty_line,
          lkup = lkup,
          fill_gaps = TRUE)
tictoc::toc()


# copy cache to TFS folder
ori_cache <- fs::path(lkup$data_root, "cache.duckdb")
dest_cache <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_SERVER") |>
  fs::path(ver_to_use, "cache.duckdb")

if (fs::file_exists(ori_cache)) {
  fs::file_copy(ori_cache, dest_cache, overwrite = TRUE)
}
