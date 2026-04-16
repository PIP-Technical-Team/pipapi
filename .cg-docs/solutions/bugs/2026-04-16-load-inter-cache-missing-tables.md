---
date: 2026-04-16
title: "load_inter_cache emits catalog warning when DuckDB tables do not exist"
category: "bugs"
type: "bug"
language: "R"
tags: [duckdb, cache, load_inter_cache, missing-table, catalog-error, warning]
root-cause: "load_inter_cache queries the cache table unconditionally; when the .duckdb file exists but is uninitialized, it throws a Catalog Error that bubbles up as a warning via the upstream tryCatch in return_if_exists"
severity: "P3"
test-written: "yes"
fix-confirmed: "yes"
---

# load_inter_cache emits catalog warning when DuckDB tables do not exist

## Symptom

When running `pip()` against a data vintage whose DuckDB cache file exists but
has never been initialized, the following warning is emitted (but execution
continues):

```
Warning message:
Failed to load intermediate cache: Catalog Error: Table with name rg_master_file
does not exist! ...
```

The warning did not appear for vintages that had already been written to at
least once (because those vintages had an initialized schema).

## Root Cause

`load_inter_cache()` calls `DBI::dbGetQuery(con, "select * from {target_file}")`
unconditionally. When the `.duckdb` file exists but `rg_master_file` /
`fg_master_file` have not been created yet (first run of a new vintage),
DuckDB raises a Catalog Error. The caller (`return_if_exists`) catches the
error and re-emits it as a `cli::cli_warn`, so execution continues — but the
warning is noisy and indicates a real code smell.

## Reproduction Test

File: `tests/testthat/test-reset_cache.R`

```r
test_that("load_inter_cache returns empty data.table when tables do not exist", {
  tmp        <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  duckdb::dbDisconnect(con)

  result <- expect_no_warning(
    load_inter_cache(cache_file_path = cache_path, fill_gaps = FALSE)
  )
  expect_true(is.data.table(result))
  expect_equal(nrow(result), 0L)
})
```

## Fix

In `R/duckdb_func.R`, added a `DBI::dbExistsTable()` guard in
`load_inter_cache()` before the query. If the table is absent, disconnect and
return an empty `data.table()`:

```r
if (!DBI::dbExistsTable(con, target_file)) {
  duckdb::dbDisconnect(con)
  return(data.table::data.table())
}
```

## Lessons Learned

This is the third instance of the same root anti-pattern: DuckDB operations
assuming tables exist. The pattern to follow in every DuckDB function that
reads or writes a named table:

> **Always call `DBI::dbExistsTable()` before any DML or query on a named table.**

Previous instances fixed the same day:
1. `reset_cache` — `DELETE` without existence check (`d67c005`)
2. `update_master_file` — `PRAGMA table_info()` without existence check (`95795d3`)
3. `load_inter_cache` — `SELECT *` without existence check (`4df2714`)

## Related

- [bugs/2026-04-16-reset-cache-missing-tables.md](2026-04-16-reset-cache-missing-tables.md)
- [testing-patterns/2026-04-16-testing-duckdb-functions.md](../testing-patterns/2026-04-16-testing-duckdb-functions.md)
- [data-quality/2026-04-16-duckdb-dbexiststable-guard.md](../data-quality/2026-04-16-duckdb-dbexiststable-guard.md) — team-wide convention: always guard DuckDB table operations with `dbExistsTable()`
