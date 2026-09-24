test_that("API loader returns the Plumber router created by the script", {
  path <- tempfile(fileext = ".R")
  writeLines("pr <- structure(list(marker = 'fixture'), class = 'Plumber')", path)
  env <- new.env(parent = globalenv())
  router <- pipapi:::.load_api_router(path, env)
  expect_s3_class(router, "Plumber")
  expect_identical(router$marker, "fixture")
  expect_identical(env$pr, router)
})

test_that("API loader rejects scripts without a Plumber router", {
  path <- tempfile(fileext = ".R")
  writeLines("value <- 'not a router'", path)
  expect_error(
    pipapi:::.load_api_router(path, new.env(parent = globalenv())),
    "did not create a Plumber router"
  )
})

test_that("API lookup preparation preserves historical versions", {
  state <- pipapi:::.cache_v2_state
  old_config <- state$config
  on.exit(state$config <- old_config, add = TRUE)
  managed <- "20260922_2021_01_02_PROD"
  historical <- "20240627_2017_01_02_PROD"
  state$config <- list(
    manifest = list(versions = setNames(list(list(fingerprint = "source")), managed)),
    build = list(fingerprint = "build")
  )
  lkups <- list(
    versions = c(managed, historical),
    latest_release = managed,
    versions_paths = setNames(list(
      list(data_root = tempfile("managed-")),
      list(data_root = tempfile("historical-"))
    ), c(managed, historical))
  )

  prepared <- pipapi:::.cache_v2_prepare_lkups(lkups)

  expect_identical(prepared$versions, c(managed, historical))
  expect_identical(prepared$versions_paths[[managed]]$cache_v2$version, managed)
  expect_null(prepared$versions_paths[[historical]][["cache_v2", exact = TRUE]])
  checked <- character()
  local_mocked_bindings(cache_v2_validate_intermediate = function(lkup, require_rows) {
    expect_true(require_rows)
    checked <<- c(checked, lkup$cache_v2$version)
    list(valid = TRUE)
  }, .package = "pipapi")
  expect_identical(pipapi:::.cache_v2_verify_api_lkups(prepared), prepared)
  expect_identical(checked, managed)
  withr::local_envvar(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE")
  withr::local_options(pipapi.lkups = NULL)
  local_mocked_bindings(cache_v2_validate_intermediate = function(...) {
    stop("stale canonical DuckDB")
  }, .package = "pipapi")
  expect_error(pipapi::start_api(lkups = lkups, port = 8080),
               "stale canonical DuckDB")
  expect_error(
    pipapi:::.cache_v2_prepare_lkups(within(lkups, versions_paths[[managed]] <- NULL)),
    "must match exactly"
  )
  inconsistent <- lkups
  inconsistent$versions <- managed
  expect_error(pipapi:::.cache_v2_prepare_lkups(inconsistent), "must match exactly")
  lkups$latest_release <- historical
  expect_error(pipapi:::.cache_v2_prepare_lkups(lkups), "latest release")
})

test_that("API lookup resolution supports explicit, option, and legacy global values", {
  explicit <- list(source = "explicit")
  option <- list(source = "option")
  global <- list(source = "global")
  old_option <- getOption("pipapi.lkups")
  old_global <- get0("lkups", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    options(pipapi.lkups = old_option)
    if (is.null(old_global)) {
      if (exists("lkups", envir = .GlobalEnv, inherits = FALSE)) {
        rm("lkups", envir = .GlobalEnv)
      }
    } else {
      assign("lkups", old_global, envir = .GlobalEnv)
    }
  }, add = TRUE)

  assign("lkups", global, envir = .GlobalEnv)
  options(pipapi.lkups = option)
  expect_identical(pipapi:::.resolve_api_lkups(explicit), explicit)
  expect_identical(pipapi:::.resolve_api_lkups(), option)
  options(pipapi.lkups = NULL)
  expect_identical(pipapi:::.resolve_api_lkups(), global)
})
