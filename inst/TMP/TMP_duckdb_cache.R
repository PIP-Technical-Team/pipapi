# SETUP  -------------

devtools::load_all(".")
library(fastverse)

root_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path()
fs::dir_ls(root_dir, recurse = FALSE)

#
latest_version <-
  available_versions(root_dir) |>
  max()

lkups <- create_versioned_lkups(
  root_dir,
  vintage_pattern = "^20260324.+(PROD)$"
)

# lkups <- create_versioned_lkups(root_dir,
#                                 vintage_pattern = latest_version)

# DEGUB -------------

options(pipapi.query_live_data = FALSE)
getOption("pipapi.query_live_data")

for (ver_to_use in lkups$versions) {
  lkup <- lkups$versions_paths[[ver_to_use]]

  reset_cache(lkup = lkup)
  povlines <- get_aux_table(lkup$data_root, "poverty_lines") |>
    _[, poverty_line]

  tictoc::tic()
  sv <- pip(
    country = "ALL",
    year = "ALL",
    povline = povlines,
    lkup = lkup,
    fill_gaps = FALSE
  )
  sv_time <- tictoc::toc(quiet = TRUE)
  sv_elapsed <- round(sv_time$toc - sv_time$tic, 1)

  tictoc::tic()
  fg <- pip(
    country = "ALL",
    year = "ALL",
    povline = povlines,
    lkup = lkup,
    fill_gaps = TRUE
  )
  fg_time <- tictoc::toc(quiet = TRUE)
  fg_elapsed <- round(fg_time$toc - fg_time$tic, 1)

  msg <- paste0(
    "Finished caching for: ",
    ver_to_use,
    "\n",
    "Survey years: ",
    sv_elapsed,
    "s | Lineup years: ",
    fg_elapsed,
    "s | ",
    "Total: ",
    sv_elapsed + fg_elapsed,
    "s"
  )
  pushoverr::pushover(msg)
}
