devtools::load_all(".")
force <- FALSE
if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)
}


latest_version <-
  pipapi:::available_versions(data_dir) |>
  max()

latest_version <- NULL
latest_version <- "20240627_2017_01_02_PROD"
lkups <- create_versioned_lkups(data_dir,
                                vintage_pattern = latest_version)

lkup <- lkups$versions_paths[[lkups$latest_release]]


# reset_cache(lkup = lkup)



# 1.
debugonce(return_if_exists)
debugonce(subset_lkup)
pip(country = "all", year = 2000, lkup = lkup)

# 2.
pip(country = "AGO", year = 2000, lkup = lkup)


pip(country = "all", year = "all", lkup = lkup)


pip(country = "IND", year = 2018, lkup = lkup)

pip(country = "IND", year = "all", lkup = lkup)
