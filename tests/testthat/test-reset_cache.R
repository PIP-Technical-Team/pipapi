test_that("reset_cache does not error when cache tables do not exist", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

 # Create an empty DuckDB — no tables inside
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  duckdb::dbDisconnect(con)

  lkup_mock <- list(data_root = tmp)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY  = "test-key",
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
  DBI::dbExecute(con, "INSERT INTO rg_master_file VALUES ('id1', 'national', 2.15, 0.1, 0.05, 0.02, 0.01)")
  DBI::dbExecute(con, "INSERT INTO fg_master_file VALUES ('id1', 2.15, 0.1, 0.05, 0.02, 0.01)")
  duckdb::dbDisconnect(con)

  lkup_mock <- list(data_root = tmp)

  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY  = "test-key",
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
