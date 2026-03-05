# tests/testdata/generate_snapshots.R
#
# PURPOSE: Generate snapshot .rds files for regression testing.
# Run this script manually whenever the data version changes and you want to
# update the baseline. Requires PIPAPI_DATA_ROOT_FOLDER_LOCAL to be set.
#
# Usage (from project root):
#   source("tests/testdata/generate_snapshots.R")

library(pipapi)
library(fs)

# --- Setup -------------------------------------------------------------------

data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")
if (data_dir == "") {
  stop("PIPAPI_DATA_ROOT_FOLDER_LOCAL is not set. Cannot generate snapshots.")
}

lkups <- create_versioned_lkups(data_dir = fs::path(data_dir))
lkup  <- lkups$versions_paths[[lkups$latest_release]]

snap_dir <- fs::path("tests", "testdata", "snapshots")
fs::dir_create(snap_dir)

# Record the data version used to generate these snapshots
writeLines(
  c(
    paste("Generated:", Sys.time()),
    paste("Data version:", lkups$latest_release),
    paste("pipapi version:", as.character(packageVersion("pipapi")))
  ),
  fs::path(snap_dir, "snapshot_manifest.txt")
)

# --- Helper ------------------------------------------------------------------

save_snap <- function(expr, name) {
  message("Generating: ", name)
  result <- tryCatch(
    force(expr),
    error = function(e) {
      warning("FAILED generating ", name, ": ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(result)) {
    saveRDS(result, fs::path(snap_dir, paste0(name, ".rds")))
    message("  -> saved (", nrow(result), " rows)")
  }
}

# --- Snapshots ---------------------------------------------------------------

# 1. Single country, single survey year
save_snap(
  pip("AGO", year = 2000, povline = 1.9, lkup = lkup),
  "snap_pip_ago_2000"
)

# 2. Single country, all survey years
save_snap(
  pip("AGO", year = "ALL", povline = 1.9, lkup = lkup),
  "snap_pip_ago_all"
)

# 3. Single country, fill gaps (lineup years)
save_snap(
  pip("AGO", year = "ALL", povline = 1.9, fill_gaps = TRUE, lkup = lkup),
  "snap_pip_ago_fg"
)

# 4. All countries, single year
save_snap(
  pip("ALL", year = 2015, povline = 1.9, lkup = lkup),
  "snap_pip_all_2015"
)

# 5. Multi-reporting-level country (national/rural/urban)
save_snap(
  pip("CHN", year = 2018, povline = 1.9, reporting_level = "all", lkup = lkup),
  "snap_pip_chn_2018"
)

# 6. Aggregation via pip_agg (new pathway)
save_snap(
  pip_agg("ALL", year = 2015, povline = 1.9, group_by = "wb", lkup = lkup),
  "snap_agg_all_2015"
)

# 7. Multiple poverty lines
save_snap(
  pip("AGO", year = 2000, povline = c(1.9, 3.65, 6.85), lkup = lkup),
  "snap_pip_ago_multi_pl"
)

# 8. Popshare
save_snap(
  pip("AGO", year = 2000, popshare = 0.2, lkup = lkup),
  "snap_pip_ago_popshare"
)

message("\nDone. Snapshots saved to: ", snap_dir)
message("Review snapshot_manifest.txt to confirm the data version.")
