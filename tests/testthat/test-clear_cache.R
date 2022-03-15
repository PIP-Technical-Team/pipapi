# Tests depend on PIPAPI_DATA_ROOT_FOLDER. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")

lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
lkups <- lkups$versions_paths$latest_release

# Tests depend on a specific temp cache directory. Skip if not found.
cd <- readRDS('tests/testdata/cd-ex.RDS')
skip_if_not(dir.exists(cd$info()$dir),
            message = sprintf("The cache directory '%s' doesn't exist", cd$info()$dir))

test_that("clear_cache() is working as expected", {

  # Send request to make sure that at least one item is in the cache
  pip("AGO", 2008, lkup = lkups)

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
