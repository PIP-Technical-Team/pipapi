# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "" ||
          Sys.getenv("PIPAPI_APPLY_CACHING") != TRUE)
#

lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkup <- lkups$versions_paths[[lkups$latest_release]]

# lkup_path <- test_path("testdata", "lkup.rds")
# lkup      <- readRDS(lkup_path)

# cd        <- readRDS(test_path("testdata", "cd-ex.RDS"))
#
# skip_if_not(dir.exists(cd$info()$dir),
#             message = sprintf("The cache directory '%s' doesn't exist", cd$info()$dir))

test_that("clear_cache() is working as expected", {

  # Send request to make sure that at least one item is in the cache
  pip("AGO", 2008, lkup = lkup)

  # items found
  res <- clear_cache(cd)
  expect_equal(res$status, 'success')
  expect_equal(res$msg, 'Cache cleared.')

  # no items found
  res <- clear_cache(cd)
  expect_equal(res$status, 'success')
  expect_equal(res$msg, 'Cache directory is empty. Nothing to clear.')

  # function error
  res <- clear_cache()
  expect_equal(res$status, 'error')
  expect_equal(res$msg,  'Cache directory not found.')

})


