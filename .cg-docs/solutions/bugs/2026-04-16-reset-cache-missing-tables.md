---
date: 2026-04-16
title: "reset_cache fails when DuckDB cache tables do not exist yet"
category: "bugs"
type: "bug"
language: "R"
tags: [duckdb, cache, reset_cache, missing-table, catalog-error]
root-cause: "reset_cache assumes rg_master_file / fg_master_file tables already exist, but on first run or after a data version change the .duckdb file may be empty"
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

# reset_cache fails when DuckDB cache tables do not exist yet

## Symptom

Running `reset_cache(lkup = lkup)` throws:

```
Error in `dbSendQuery()`:
! Catalog Error: Table with name rg_master_file does not exist!
```

This happens when the DuckDB cache file exists but the tables have not been
created yet (e.g., first run against a new data vintage).

## Root Cause

`reset_cache()` unconditionally executes `DELETE from rg_master_file` and
`DELETE from fg_master_file` without checking whether these tables exist.
When the cache `.duckdb` file is new or was recreated, the tables are absent
and DuckDB raises a catalog error.

## Reproduction Test

File: `tests/testthat/test-reset_cache.R`

```r
test_that("reset_cache does not error when cache tables do not exist", {
  tmp <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  duckdb::dbDisconnect(con)

  lkup_mock <- list(data_root = tmp)
  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY  = "test-key",
    PIP_CACHE_SERVER_KEY = "test-key"
  )
  expect_no_error(reset_cache(pass = "test-key", lkup = lkup_mock))
})
```

## Fix

In `R/duckdb_func.R`, guard each `DELETE` with `DBI::dbExistsTable()`:

```r
if("rg" %in% type && DBI::dbExistsTable(write_con, "rg_master_file")) {
  DBI::dbExecute(write_con, "DELETE from rg_master_file")
}
if("fg" %in% type && DBI::dbExistsTable(write_con, "fg_master_file")) {
  DBI::dbExecute(write_con, "DELETE from fg_master_file")
}
```

If the table doesn't exist, the cache is already empty — no deletion needed.

## Lessons Learned

When interacting with DuckDB (or any database), always check for table
existence before issuing DML statements. This is especially important for
cache files that may be created lazily or rebuilt across data vintages.
Anti-pattern: assuming database state based on file existence alone.

## Related

- [testing-patterns/2026-04-16-testing-duckdb-functions.md](../testing-patterns/2026-04-16-testing-duckdb-functions.md) — pattern for unit-testing DuckDB cache functions with ephemeral databases
