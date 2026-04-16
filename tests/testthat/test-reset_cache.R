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
