load_pipapi_bootstrap <- function(cache_size, cache_root, bad_file = FALSE, fail_cache = FALSE) {
  callr::r(
    function(cache_size, cache_root, bad_file, fail_cache) {
      Sys.setenv(PIPAPI_APPLY_CACHING = "TRUE")
      Sys.setenv(PIPAPI_CACHE_MAX_SIZE = cache_size)
      Sys.setenv(R_USER_CACHE_DIR = cache_root)

      if (fail_cache) {
        ns <- asNamespace("cachem")
        old_cache_disk <- get("cache_disk", envir = ns)

        unlockBinding("cache_disk", ns)
        assign("cache_disk", function(...) stop("forced cache init failure"), envir = ns)

        on.exit({
          assign("cache_disk", old_cache_disk, envir = ns)
          lockBinding("cache_disk", ns)
        }, add = TRUE)
      }

      cache_dir <- rappdirs::user_cache_dir("pipapi")

      if (bad_file) {
        if (dir.exists(cache_dir)) {
          unlink(cache_dir, recursive = TRUE, force = TRUE)
        }

        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        file.create(file.path(cache_dir, "bad-cache-file.qs"))
      }

      library(pipapi)

      cd_exists <- exists("cd", envir = globalenv(), inherits = FALSE)

      if (cd_exists) {
        cd_info <- get("cd", envir = globalenv(), inherits = FALSE)[["info"]]()
      } else {
        cd_info <- NULL
      }

      list(
        cd_exists = cd_exists,
        cache_max_size = if (cd_exists) cd_info[["max_size"]] else NA_real_,
        bad_file_exists = if (bad_file) file.exists(file.path(cache_dir, "bad-cache-file.qs")) else NA
      )
    },
    args = list(
      cache_size = cache_size,
      cache_root = cache_root,
      bad_file = bad_file,
      fail_cache = fail_cache
    )
  )
}

test_that("Invalid cache max-size value uses default limit", {
  cache_root <- tempfile("pipapi-cache", tmpdir = tempdir())
  result <- load_pipapi_bootstrap(
    cache_size = "1024 * 1024^2",
    cache_root = cache_root,
    bad_file = FALSE,
    fail_cache = FALSE
  )

  expect_true(result$cd_exists)
  expect_equal(result$cache_max_size, 1024^3)
})

test_that("Empty cache max-size value uses default limit", {
  cache_root <- tempfile("pipapi-cache", tmpdir = tempdir())
  result <- load_pipapi_bootstrap(
    cache_size = "",
    cache_root = cache_root,
    bad_file = FALSE,
    fail_cache = FALSE
  )

  expect_true(result$cd_exists)
  expect_equal(result$cache_max_size, 1024^3)
})

test_that("Numeric cache max-size value is honored", {
  cache_root <- tempfile("pipapi-cache", tmpdir = tempdir())
  result <- load_pipapi_bootstrap(
    cache_size = "1073741824",
    cache_root = cache_root,
    bad_file = FALSE,
    fail_cache = FALSE
  )

  expect_true(result$cd_exists)
  expect_equal(result$cache_max_size, 1024^3)
})

test_that("Zero-size .qs files are removed on startup", {
  cache_root <- tempfile("pipapi-cache", tmpdir = tempdir())
  result <- load_pipapi_bootstrap(
    cache_size = "1073741824",
    cache_root = cache_root,
    bad_file = TRUE,
    fail_cache = FALSE
  )

  expect_true(result$cd_exists)
  expect_false(result$bad_file_exists)
})

test_that("Cache is disabled when disk cache init fails", {
  cache_root <- tempfile("pipapi-cache", tmpdir = tempdir())
  result <- load_pipapi_bootstrap(
    cache_size = "1073741824",
    cache_root = cache_root,
    bad_file = FALSE,
    fail_cache = TRUE
  )

  expect_false(result$cd_exists)
})
