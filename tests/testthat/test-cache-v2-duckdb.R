# These fixtures replace the provenance provider and its source guard. Database access uses
# the package helpers, private temporary databases, and the real file lock.
duckdb_v2_fixture <- function(root) {
  env <- new.env(parent = environment(intermediate_cache_path))
  helpers <- c(
    "intermediate_cache_path", "intermediate_cache_context", "intermediate_cache_identity",
    "intermediate_cache_lock", "with_intermediate_db", "intermediate_cache_schema",
    "create_duckdb_file", "load_inter_cache", "update_master_file",
    "safe_update_master_file", "return_if_exists", "connect_with_retry", "reset_cache", "delete_cache"
  )
  for (name in helpers) {
    fun <- get(name, envir = parent.env(env))
    environment(fun) <- env
    env[[name]] <- fun
  }
  env$context <- list(
    root = root, version = "20260922_2021_01_02_PROD",
    dependency_fingerprint = paste(rep("a", 64), collapse = ""),
    build_fingerprint = paste(rep("b", 64), collapse = ""),
    revision = "published-revision", intermediate_mode = "write",
    parameters = list(povline = 3, ppp = NULL, popshare = NULL)
  )
  env$cache_v2_context <- function(lkup = NULL) env$context
  env$.cache_v2_guard <- function(identity, inputs = FALSE) invisible(TRUE)
  env$lkup <- list(data_root = file.path(root, "legacy"), cache_v2 = list(lookup_variant = "full"))
  env$dat <- data.table::data.table(
    cache_id = "survey-1", reporting_level = "national", poverty_line = 3,
    headcount = 0.5, poverty_gap = 0.2, poverty_severity = 0.1, watts = 0.05
  )
  env
}

test_that("v2 rows are revision-aware, idempotent, and private", {
  withr::local_envvar(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE")
  withr::local_options(pipapi.query_live_data = FALSE, pipapi.verbose = FALSE)
  f <- duckdb_v2_fixture(withr::local_tempdir())
  path <- f$intermediate_cache_path(f$lkup)
  expect_match(path, "/v2/20260922_2021_01_02_PROD/intermediate/cache.duckdb$", fixed = FALSE)
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 0L)
  expect_false(file.exists(path))
  expect_equal(f$update_master_file(f$dat, path, FALSE), 1)
  expect_equal(f$update_master_file(f$dat, path, FALSE), 0)
  expect_equal(f$load_inter_cache(cache_file_path = path), f$dat)
  expect_false(dir.exists(f$lkup$data_root))

  original <- f$context
  for (field in c("revision", "dependency_fingerprint", "build_fingerprint", "version")) {
    f$context[[field]] <- paste0(original[[field]], "-changed")
    changed_path <- f$intermediate_cache_path(f$lkup)
    expect_equal(nrow(f$load_inter_cache(cache_file_path = changed_path)), 0L, info = field)
    expect_error(f$load_inter_cache(cache_file_path = path), "provenance")
    f$context <- original
  }
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 1L)
  selected <- data.table::copy(f$dat)[, c("poverty_line", "headcount", "poverty_gap", "poverty_severity", "watts") := NULL]
  selected[, is_interpolated := FALSE]
  expect_equal(nrow(f$return_if_exists(selected, 3, path, FALSE)$data_present_in_master), 1L)
  f$context$revision <- "next-revision"
  expect_null(f$return_if_exists(selected, 3, f$intermediate_cache_path(f$lkup), FALSE)$data_present_in_master)
})

test_that("custom arguments and lookup variants cannot share lower rows", {
  withr::local_envvar(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE")
  withr::local_options(pipapi.query_live_data = FALSE, pipapi.verbose = FALSE)
  f <- duckdb_v2_fixture(withr::local_tempdir())
  path <- f$intermediate_cache_path(f$lkup)
  f$update_master_file(f$dat, path, FALSE)
  for (custom in list(list(ppp = 2), list(popshare = 0.5), list(ppp = 2, popshare = 0.5))) {
    custom_path <- do.call(f$intermediate_cache_path, c(list(lkup = f$lkup), custom))
    expect_equal(nrow(f$load_inter_cache(cache_file_path = custom_path)), 0L)
    expect_equal(f$update_master_file(f$dat, custom_path, FALSE), 1)
    expect_equal(nrow(f$load_inter_cache(cache_file_path = custom_path)), 1L)
  }
  f$lkup$cache_v2$lookup_variant <- "cp"
  cp_path <- f$intermediate_cache_path(f$lkup)
  expect_equal(nrow(f$load_inter_cache(cache_file_path = cp_path)), 0L)
  f$context$parameters$censor <- FALSE
  expect_equal(nrow(f$load_inter_cache(cache_file_path = f$intermediate_cache_path(f$lkup))), 0L)
})

test_that("v2 never reuses legacy rows and repairs only missing tables", {
  withr::local_envvar(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE")
  withr::local_options(pipapi.query_live_data = FALSE, pipapi.verbose = FALSE)
  f <- duckdb_v2_fixture(withr::local_tempdir())
  path <- f$intermediate_cache_path(f$lkup)
  f$with_intermediate_db(path, TRUE, function(con) {
    f$intermediate_cache_schema(con, NULL)
    DBI::dbExecute(con, "INSERT INTO rg_master_file VALUES ('legacy', 'national', 3, 1, 1, 1, 1)")
  })
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 0L)
  expect_error(f$load_inter_cache(cache_file_path = file.path(f$lkup$data_root, "cache.duckdb")), "legacy paths")
  f$update_master_file(f$dat, path, FALSE)
  fg <- data.table::copy(f$dat)[, c("cache_id", "reporting_level") := NULL]
  fg[, interpolation_id := "lineup-1"]
  f$update_master_file(fg, path, TRUE)
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path, fill_gaps = TRUE)), 1L)
  f$context$revision <- "next-revision"
  expect_equal(nrow(f$load_inter_cache(cache_file_path = f$intermediate_cache_path(f$lkup), fill_gaps = TRUE)), 0L)
  f$context$revision <- "published-revision"
  f$with_intermediate_db(path, TRUE, function(con) DBI::dbExecute(con, "DROP TABLE fg_master_file_v2"))
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path, fill_gaps = TRUE)), 0L)
  f$create_duckdb_file(path)
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 1L)
  second <- data.table::copy(f$dat)[, cache_id := "survey-2"]
  f$update_master_file(second, path, FALSE)
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 2L)
  expect_equal(f$with_intermediate_db(path, FALSE, function(con) {
    DBI::dbGetQuery(con, "SELECT count(*) AS n FROM rg_master_file")$n
  }), 1)
})

test_that("read-only misses and live requests do not initialize or write", {
  withr::local_envvar(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE")
  withr::local_options(pipapi.query_live_data = FALSE, pipapi.verbose = FALSE)
  f <- duckdb_v2_fixture(withr::local_tempdir())
  f$context$intermediate_mode <- "read_only"
  path <- f$intermediate_cache_path(f$lkup)
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 0L)
  expect_false(f$update_master_file(f$dat, path, FALSE))
  expect_false(f$create_duckdb_file(path))
  expect_false(dir.exists(dirname(path)))
  f$context$intermediate_mode <- "write"
  path <- f$intermediate_cache_path(f$lkup)
  f$update_master_file(f$dat, path, FALSE)
  checksum <- digest::digest(file = path, algo = "sha256")
  f$context$intermediate_mode <- "read_only"
  path <- f$intermediate_cache_path(f$lkup)
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 1L)
  expect_false(f$safe_update_master_file(f$dat, path, FALSE))
  expect_identical(digest::digest(file = path, algo = "sha256"), checksum)
  options(pipapi.query_live_data = TRUE)
  f$context <- NULL
  expect_null(f$intermediate_cache_path(f$lkup))
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 0L)
  expect_false(f$safe_update_master_file(f$dat, path, FALSE))
  expect_false(f$create_duckdb_file(path))
  expect_identical(digest::digest(file = path, algo = "sha256"), checksum)
})

test_that("failed required writes roll back and release database and file locks", {
  withr::local_envvar(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE")
  withr::local_options(pipapi.query_live_data = FALSE, pipapi.verbose = FALSE)
  f <- duckdb_v2_fixture(withr::local_tempdir())
  path <- f$intermediate_cache_path(f$lkup)
  invalid <- data.table::copy(f$dat)[, watts := NULL]
  expect_error(f$safe_update_master_file(invalid, path, FALSE), "watts")
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 0L)
  expect_equal(f$update_master_file(f$dat, path, FALSE), 1)
  guard_calls <- 0L
  f$.cache_v2_guard <- function(identity, inputs = FALSE) {
    guard_calls <<- guard_calls + 1L
    if (guard_calls == 2L) stop("source changed before commit")
  }
  second <- data.table::copy(f$dat)[, cache_id := "survey-2"]
  expect_error(f$update_master_file(second, path, FALSE), "source changed before commit")
  f$.cache_v2_guard <- function(identity, inputs = FALSE) invisible(TRUE)
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 1L)
  expect_error(f$with_intermediate_db(path, TRUE, function(con) {
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, "DELETE FROM rg_master_file_v2")
      stop("forced rollback")
    })
  }), "forced rollback")
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 1L)
  expect_error(f$connect_with_retry(path, read_only = FALSE), "Raw DuckDB connections")
  testthat::with_mocked_bindings(
    expect_error(f$load_inter_cache(cache_file_path = path), "forced connect failure"),
    dbConnect = function(...) stop("forced connect failure"), .package = "DBI"
  )
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 1L)
  f$context$dependency_fingerprint <- NULL
  expect_error(f$intermediate_cache_path(f$lkup), "provenance")
})

test_that("writable readers and writers use bounded interprocess locks", {
  skip_if_not_installed("callr")
  withr::local_envvar(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE")
  withr::local_envvar(PIP_CACHE_LOCAL_KEY = "fixture", PIP_CACHE_SERVER_KEY = "fixture")
  withr::local_options(pipapi.query_live_data = FALSE, pipapi.verbose = FALSE,
                      pipapi.cache_v2_lock_timeout = 100)
  root <- withr::local_tempdir()
  f <- duckdb_v2_fixture(root)
  path <- f$intermediate_cache_path(f$lkup)
  f$update_master_file(f$dat, path, FALSE)
  ready <- file.path(root, "lock-ready")
  helpers <- c("with_intermediate_db", "intermediate_cache_context", "intermediate_cache_lock",
               "cache_v2_enabled")
  functions <- setNames(lapply(helpers, function(name) {
    fun <- get(name, envir = f)
    environment(fun) <- baseenv()
    fun
  }), helpers)
  worker <- callr::r_bg(function(path, ready, functions, context) {
    Sys.setenv(PIPAPI_APPLY_CACHING = "FALSE")
    env <- new.env(parent = asNamespace("pipapi"))
    Sys.setenv(PIPAPI_APPLY_CACHING = "TRUE")
    for (name in names(functions)) {
      environment(functions[[name]]) <- env
      env[[name]] <- functions[[name]]
    }
    env$cache_v2_context <- function(lkup = NULL) context
    env$.cache_v2_guard <- function(...) invisible(TRUE)
    options(pipapi.query_live_data = FALSE)
    env$with_intermediate_db(path, TRUE, function(con) {
      DBI::dbWithTransaction(con, {
        DBI::dbExecute(con, "DELETE FROM rg_master_file_v2")
        file.create(ready)
        Sys.sleep(3)
        stop("worker rollback")
      })
    })
  }, args = list(path = path, ready = ready, functions = functions, context = f$context),
  libpath = .libPaths())
  on.exit(if (worker$is_alive()) worker$kill(), add = TRUE)
  deadline <- Sys.time() + 15
  while (!file.exists(ready) && worker$is_alive() && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(file.exists(ready))
  expect_error(f$load_inter_cache(cache_file_path = path), "Timed out")
  expect_error(f$safe_update_master_file(f$dat, path, FALSE), "Timed out")
  expect_error(f$reset_cache(pass = "fixture", lkup = f$lkup), "Timed out")
  expect_error(f$delete_cache(pass = "fixture", lkup = f$lkup), "Timed out")
  worker$wait(timeout = 10000)
  expect_false(worker$is_alive())
  expect_error(worker$get_result(), "worker rollback")
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 1L)
  expect_equal(f$update_master_file(f$dat, path, FALSE), 0)
  f$context$intermediate_mode <- "read_only"
  expect_error(f$reset_cache(pass = "fixture", lkup = f$lkup), "read-only")
  expect_error(f$delete_cache(pass = "fixture", lkup = f$lkup), "read-only")
  f$context$intermediate_mode <- "write"
  f$reset_cache(pass = "fixture", lkup = f$lkup)
  expect_equal(nrow(f$load_inter_cache(cache_file_path = path)), 0L)
  f$delete_cache(pass = "fixture", lkup = f$lkup)
  expect_false(file.exists(path))
  expect_true(file.exists(paste0(path, ".lock")))
})

test_that("actual core provenance and taint guard protect lower caches", {
  skip_if(is.null(attr(get("pip", asNamespace("pipapi")), "cache_v2_original")),
          "Requires package startup with v2 wrappers enabled")
  withr::local_envvar(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE")
  withr::local_options(pipapi.query_live_data = FALSE, pipapi.verbose = FALSE,
                      pipapi.cache_v2_context = NULL)
  old_config <- .cache_v2_state$config
  on.exit(.cache_v2_state$config <- old_config, add = TRUE)
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  dir.create(source)
  for (dir in c("_aux", "estimations", "survey_data", "lineup_data")) {
    dir.create(file.path(source, dir))
    writeLines("fixture", file.path(source, dir, "input.txt"))
  }
  version <- "20260922_2021_01_02_PROD"
  manifest <- cache_v2_manifest(source, version)
  cache_v2_configure(file.path(root, "cache"), manifest, cache_v2_build())
  lkup <- cache_v2_attach(list(data_root = source), version)
  path <- intermediate_cache_path(lkup)
  dat <- duckdb_v2_fixture(root)$dat
  expect_equal(update_master_file(dat, path, FALSE), 1)
  expect_equal(nrow(load_inter_cache(lkup = lkup)), 1L)
  expect_false(file.exists(file.path(source, "cache.duckdb")))
  context <- cache_v2_context(lkup)
  context$parameters <- list(ppp = 2, popshare = NULL)
  options(pipapi.cache_v2_context = context)
  expect_equal(nrow(load_inter_cache(lkup = lkup)), 0L)
  options(pipapi.cache_v2_context = NULL)
  cache_v2_taint("fixture source change")
  expect_error(load_inter_cache(lkup = lkup), "tainted")
  expect_error(safe_update_master_file(dat, path, FALSE), "tainted")
})

test_that("v2 opt-in cannot fall back to legacy without configuration", {
  withr::local_envvar(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "FALSE")
  withr::local_options(pipapi.query_live_data = FALSE)
  root <- withr::local_tempdir()
  expect_error(intermediate_cache_path(list(data_root = root)), "requires caching")
  expect_error(load_inter_cache(cache_file_path = file.path(root, "cache.duckdb")), "requires caching")
  expect_false(file.exists(file.path(root, "cache.duckdb")))
})

test_that("legacy table repair is non-destructive with v2 disabled", {
  withr::local_envvar(PIPAPI_CACHE_V2 = "FALSE")
  withr::local_options(pipapi.query_live_data = FALSE, pipapi.verbose = FALSE)
  path <- file.path(withr::local_tempdir(), "cache.duckdb")
  create_duckdb_file(path)
  with_intermediate_db(path, TRUE, function(con) {
    DBI::dbExecute(con, "INSERT INTO rg_master_file VALUES ('legacy', 'national', 3, 1, 1, 1, 1)")
    DBI::dbExecute(con, "DROP TABLE fg_master_file")
  })
  create_duckdb_file(path)
  expect_equal(nrow(load_inter_cache(cache_file_path = path)), 1L)
  expect_equal(nrow(load_inter_cache(cache_file_path = path, fill_gaps = TRUE)), 0L)
})
