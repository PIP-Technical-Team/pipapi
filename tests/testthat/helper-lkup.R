# tests/testthat/helper-lkup.R
#
# PURPOSE: Shared test infrastructure for all testthat files in pipapi.
#
# This file is sourced automatically by testthat before every test file.
# It provides:
#
#   TEST_VINTAGE      — the pinned data vintage used by all integration tests.
#                       Change this ONE constant when a new PROD release is used.
#
#   test_data_dir     — path to the local PIP data root (from env var).
#                       Empty string if the env var is not set.
#
#   test_lkup         — a fully-constructed lkup list built from TEST_VINTAGE.
#                       NULL when test_data_dir is not available.
#
#   skip_if_no_lkup() — convenience skip helper.  Call at the top of any test
#                       that requires test_lkup (i.e. integration tests).
#
# Usage in integration test files:
#
#   test_that("my integration test", {
#     skip_if_no_lkup()
#     result <- pip("AGO", year = 2000, povline = 1.9, lkup = test_lkup)
#     expect_s3_class(result, "data.table")
#   })
#
# NOTE: test_lkup is built ONCE per test session (not once per file), so
#       all integration tests in a session share the same in-memory lkup.
#       This avoids the 10–30 s startup cost per file.

# ── Pinned data vintage ───────────────────────────────────────────────────────
# Update this single line when a new PROD vintage is released.
TEST_VINTAGE <- "20250930_2021_01_02_PROD"

# ── Data directory ────────────────────────────────────────────────────────────
test_data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL", unset = "")

# ── Build the shared lkup (once per session) ──────────────────────────────────
test_lkup  <- NULL
test_lkups <- NULL

if (nzchar(test_data_dir)) {
  tryCatch(
    {
      test_lkups <- create_versioned_lkups(
        data_dir        = test_data_dir,
        vintage_pattern = TEST_VINTAGE
      )
      test_lkup <- test_lkups$versions_paths[[test_lkups$latest_release]]
    },
    error = function(e) {
      # Leave test_lkup as NULL — integration tests will skip via skip_if_no_lkup()
      message(
        "helper-lkup.R: could not build test_lkup for vintage '",
        TEST_VINTAGE, "': ", conditionMessage(e)
      )
    }
  )
}

# ── Skip helper ───────────────────────────────────────────────────────────────

#' Skip a test when the pinned lkup is not available
#'
#' Use this at the top of every integration test.  The test will be silently
#' skipped on machines without PIPAPI_DATA_ROOT_FOLDER_LOCAL set, and will
#' run normally on dev machines that have the data.
skip_if_no_lkup <- function() {
  testthat::skip_if(
    is.null(test_lkup),
    paste0(
      "Integration test requires PIPAPI_DATA_ROOT_FOLDER_LOCAL set to a ",
      "directory containing vintage '", TEST_VINTAGE, "'"
    )
  )
}
