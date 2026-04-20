test_that("DuckDB file can be reopened for writing after load_inter_cache reads it", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  create_duckdb_file(cache_path)

  # Simulate the sequence: read (load_inter_cache) then write (update_master_file)
  load_inter_cache(cache_file_path = cache_path, fill_gaps = FALSE)

  # Must be able to open a write connection immediately after
  expect_no_error({
    write_con <- connect_with_retry(cache_path, read_only = FALSE)
    DBI::dbDisconnect(write_con, shutdown = TRUE)
  })
})

test_that("connect_with_retry shuts down DuckDB driver after connect failure", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  testthat::with_mocked_bindings(
    {
      expect_error(
        connect_with_retry(
          cache_path,
          read_only = FALSE,
          max_attempts = 1,
          delay_sec = 0,
          verbose = FALSE
        ),
        "forced connect failure"
      )
    },
    dbConnect = function(...) stop("forced connect failure"),
    .package = "duckdb"
  )

  expect_no_error({
    con <- connect_with_retry(cache_path, read_only = FALSE)
    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

test_that("load_inter_cache returns empty data.table when tables do not exist", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  # Empty DuckDB — no tables
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  duckdb::dbDisconnect(con)

  result <- expect_no_warning(
    load_inter_cache(cache_file_path = cache_path, fill_gaps = FALSE)
  )
  expect_true(is.data.table(result))
  expect_equal(nrow(result), 0L)
})

test_that("update_master_file creates schema on a fresh cache file", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  # Empty DuckDB — no tables
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  duckdb::dbDisconnect(con)

  # Seed pl_to_store so the filter inside update_master_file matches our data
  set_in_pipapienv("pl_to_store", c(2.15))

  dat <- data.table::data.table(
    cache_id = "AGO_2018_IBEP-II_V01_M_V02_A_GMD",
    reporting_level = "national",
    poverty_line = 2.15,
    headcount = 0.5,
    poverty_gap = 0.2,
    poverty_severity = 0.1,
    watts = 0.05
  )

  expect_no_error(
    update_master_file(dat, cache_file_path = cache_path, fill_gaps = FALSE)
  )

  # Tables must now exist and contain the inserted row
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  n <- DBI::dbGetQuery(con, "SELECT count(*) as n FROM rg_master_file")$n
  duckdb::dbDisconnect(con)

  expect_equal(n, 1L)
})

test_that("update_master_file releases DuckDB lock even when it errors", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  create_duckdb_file(cache_path)

  set_in_pipapienv("pl_to_store", c(2.15))

  dat <- data.table::data.table(
    cache_id = "AGO_2018_IBEP-II_V01_M_V02_A_GMD",
    reporting_level = "national",
    poverty_line = 2.15,
    headcount = 0.5,
    poverty_gap = 0.2,
    poverty_severity = 0.1
  )

  expect_error(
    update_master_file(dat, cache_file_path = cache_path, fill_gaps = FALSE),
    "watts"
  )

  expect_no_error({
    con <- connect_with_retry(cache_path, read_only = FALSE)
    DBI::dbDisconnect(con, shutdown = TRUE)
  })
})

test_that("treat_cache_and_main returns results when cache update fails", {
  out_in <- list(
    data_in_cache = NULL,
    main_data = data.table::data.table(
      country_code = "AGO",
      reporting_year = 2017L,
      reporting_level = "national",
      welfare_type = "income",
      poverty_line = 2.15,
      headcount = 0.2
    )
  )

  result <- testthat::with_mocked_bindings(
    expect_warning(
      pipapi:::treat_cache_and_main(
        out = out_in,
        cache_file_path = fs::path(withr::local_tempdir(), "cache.duckdb"),
        lkup = list(use_new_lineup_version = TRUE),
        fill_gaps = FALSE
      ),
      "Failed to update intermediate cache"
    ),
    update_master_file = function(...) stop("simulated cache lock"),
    .package = "pipapi"
  )

  expect_true(data.table::is.data.table(result))
  expect_equal(nrow(result), 1L)
  expect_equal(result$country_code, "AGO")
})

test_that("reset_cache does not error when cache tables do not exist", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  # Create an empty DuckDB — no tables inside
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  duckdb::dbDisconnect(con)

  lkup_mock <- list(data_root = tmp)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY = "test-key",
    PIP_CACHE_SERVER_KEY = "test-key"
  )

  expect_no_error(
    reset_cache(pass = "test-key", lkup = lkup_mock)
  )
})

test_that("reset_cache deletes rows when tables exist", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  # Create the DuckDB file with proper schema
  create_duckdb_file(cache_path)

  # Insert a dummy row
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  DBI::dbExecute(
    con,
    "INSERT INTO rg_master_file VALUES ('id1', 'national', 2.15, 0.1, 0.05, 0.02, 0.01)"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO fg_master_file VALUES ('id1', 2.15, 0.1, 0.05, 0.02, 0.01)"
  )
  duckdb::dbDisconnect(con)

  lkup_mock <- list(data_root = tmp)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY = "test-key",
    PIP_CACHE_SERVER_KEY = "test-key"
  )

  expect_no_error(
    reset_cache(pass = "test-key", lkup = lkup_mock)
  )

  # Verify tables are empty
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  rg_count <- DBI::dbGetQuery(con, "SELECT count(*) as n FROM rg_master_file")$n
  fg_count <- DBI::dbGetQuery(con, "SELECT count(*) as n FROM fg_master_file")$n
  duckdb::dbDisconnect(con)

  expect_equal(rg_count, 0)
  expect_equal(fg_count, 0)
})

test_that("delete_cache does not error when cache file does not exist", {
  tmp <- withr::local_tempdir()
  lkup_mock <- list(data_root = tmp)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY = "test-key",
    PIP_CACHE_SERVER_KEY = "test-key"
  )

  expect_no_error(
    delete_cache(pass = "test-key", lkup = lkup_mock)
  )

  result <- delete_cache(pass = "test-key", lkup = lkup_mock)
  expect_identical(result, character())
})

test_that("delete_cache removes duckdb file and wal sidecar", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")
  wal_path <- paste0(cache_path, ".wal")
  lkup_mock <- list(data_root = tmp)

  create_duckdb_file(cache_path)
  fs::file_create(wal_path)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY = "test-key",
    PIP_CACHE_SERVER_KEY = "test-key"
  )

  expect_true(file.exists(cache_path))
  expect_true(file.exists(wal_path))

  result <- delete_cache(pass = "test-key", lkup = lkup_mock)

  expect_length(result, 2L)
  expect_false(file.exists(cache_path))
  expect_false(file.exists(wal_path))

  deleted_paths <- delete_cache(pass = "test-key", lkup = lkup_mock)
  expect_length(deleted_paths, 0L) # second call: files already gone
})

test_that("delete_cache removes only the duckdb file when no WAL sidecar exists", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")
  lkup_mock <- list(data_root = tmp)

  create_duckdb_file(cache_path)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY = "test-key",
    PIP_CACHE_SERVER_KEY = "test-key"
  )

  expect_true(file.exists(cache_path))

  result <- delete_cache(pass = "test-key", lkup = lkup_mock)

  expect_length(result, 1L)
  expect_false(file.exists(cache_path))
})

# Auth failure tests ----

test_that("reset_cache aborts when PIP_CACHE_SERVER_KEY is not set", {
  tmp <- withr::local_tempdir()
  lkup_mock <- list(data_root = tmp)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY = "test-key",
    PIP_CACHE_SERVER_KEY = ""
  )

  expect_error(
    reset_cache(pass = "test-key", lkup = lkup_mock),
    class = "rlang_error"
  )
})

test_that("reset_cache aborts when supplied key does not match server key", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")
  lkup_mock <- list(data_root = tmp)
  create_duckdb_file(cache_path)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY = "correct-key",
    PIP_CACHE_SERVER_KEY = "correct-key"
  )

  expect_error(
    reset_cache(pass = "wrong-key", lkup = lkup_mock),
    class = "rlang_error"
  )
})

test_that("delete_cache aborts when PIP_CACHE_SERVER_KEY is not set", {
  tmp <- withr::local_tempdir()
  lkup_mock <- list(data_root = tmp)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY = "test-key",
    PIP_CACHE_SERVER_KEY = ""
  )

  expect_error(
    delete_cache(pass = "test-key", lkup = lkup_mock),
    class = "rlang_error"
  )
})

test_that("delete_cache aborts when supplied key does not match server key", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")
  lkup_mock <- list(data_root = tmp)
  create_duckdb_file(cache_path)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY = "correct-key",
    PIP_CACHE_SERVER_KEY = "correct-key"
  )

  expect_error(
    delete_cache(pass = "wrong-key", lkup = lkup_mock),
    class = "rlang_error"
  )
})

# TMP: live test against real data vintage ----
# Delete this test once the DuckDB connection lifecycle is confirmed working.
test_that("TMP: full read-then-write cycle works on real 2017 INT cache", {
  real_data_root <- Sys.getenv("PIPAPI_TMP_TEST_DATA_ROOT", unset = "")
  skip_if(
    !nzchar(real_data_root) || !dir.exists(real_data_root),
    "Real data directory not available (set PIPAPI_TMP_TEST_DATA_ROOT)"
  )

  cache_path <- fs::path(real_data_root, "cache", ext = "duckdb")

  probe <- tryCatch(
    {
      con <- connect_with_retry(
        cache_path,
        read_only = TRUE,
        max_attempts = 1,
        delay_sec = 0
      )
      DBI::dbDisconnect(con, shutdown = TRUE)
      TRUE
    },
    error = function(e) conditionMessage(e)
  )

  if (is.character(probe) && grepl("being used by another process", probe)) {
    skip("Real cache file is already locked in the current Positron R process")
  }

  # Step 1: read (replicates load_inter_cache inside return_if_exists)
  result <- expect_no_warning(
    load_inter_cache(cache_file_path = cache_path, fill_gaps = FALSE)
  )
  expect_true(data.table::is.data.table(result))

  # Step 2: write connection must open immediately after — this was the failure
  expect_no_error({
    write_con <- connect_with_retry(cache_path, read_only = FALSE)
    DBI::dbDisconnect(write_con, shutdown = TRUE)
  })
})
