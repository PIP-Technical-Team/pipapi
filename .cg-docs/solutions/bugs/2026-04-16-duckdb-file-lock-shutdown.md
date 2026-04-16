---
date: 2026-04-16
title: "DuckDB file locked after dbDisconnect — write connection fails with 'file in use'"
category: "bugs"
type: "bug"
language: "R"
tags: [duckdb, DBI, dbDisconnect, shutdown, file-lock, connection, cache]
root-cause: "duckdb::dbDisconnect(con) closes the connection but keeps the DuckDB instance alive in-process, leaving the file locked; subsequent write connections fail with IO error"
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
---

# DuckDB file locked after dbDisconnect — write connection fails with "file in use"

## Symptom

`update_master_file` (called via `pip()`) fails with:

```
Failed to connect after 5 attempts.
Last error: Cannot open file "cache.duckdb": The process cannot access
the file because it is being used by another process.
File is already open in ark.exe (PID XXXXX)
```

The file is reported as locked by the same R process (`ark.exe`). The error
occurs when a read connection (from `load_inter_cache` / `return_if_exists`)
is followed by a write connection (from `update_master_file`) in the same
session.

## Root Cause

`duckdb::dbDisconnect(con)` closes the *connection* object but does not shut
down the underlying DuckDB *instance*. DuckDB holds an exclusive lock on the
`.duckdb` file at the instance level, not the connection level. Until the
instance is explicitly shut down, the file cannot be reopened — especially not
with a different `read_only` flag.

The correct call is `DBI::dbDisconnect(con, shutdown = TRUE)`, which both
closes the connection and shuts down the instance, fully releasing the file.

The codebase already contained a comment acknowledging this problem, but the
fix (shutdown) had not been applied uniformly:

> *"It is important to close the read connection before you open a write
> connection because duckdb kind of inherits read_only flag from previous
> connection object if it is not closed"*

## Reproduction Test

File: `tests/testthat/test-reset_cache.R`

```r
test_that("DuckDB file can be reopened for writing after load_inter_cache reads it", {
  tmp        <- withr::local_tempdir()
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
```

## Fix

In `R/duckdb_func.R`, replaced every `duckdb::dbDisconnect(con)` (and the one
`DBI::dbDisconnect(con)`) with `DBI::dbDisconnect(con, shutdown = TRUE)` at
all six call sites:

| Function | Location |
|---|---|
| `load_inter_cache` | early-return path (table absent) |
| `load_inter_cache` | end of function |
| `update_master_file` | early-exit before schema init |
| `update_master_file` | end of function |
| `reset_cache` | end of function |
| `create_duckdb_file` | end of function |

Also switched `reset_cache` from a bare `duckdb::dbConnect(duckdb::duckdb(), ...)` 
to use `connect_with_retry()` for consistency.

## Lessons Learned

**Never use `duckdb::dbDisconnect(con)` or `DBI::dbDisconnect(con)` without
`shutdown = TRUE` in this codebase.**

DuckDB file locking is instance-level, not connection-level. The pattern to
follow for every DuckDB operation:

```r
con <- connect_with_retry(cache_file_path, read_only = FALSE)
# ... work ...
DBI::dbDisconnect(con, shutdown = TRUE)   # ← always shutdown = TRUE
```

This is the fourth instance of a DuckDB connection lifecycle issue in a single
session — all traceable to assumptions about how DuckDB manages file handles.

## Related

- [bugs/2026-04-16-reset-cache-missing-tables.md](2026-04-16-reset-cache-missing-tables.md)
- [bugs/2026-04-16-load-inter-cache-missing-tables.md](2026-04-16-load-inter-cache-missing-tables.md)
- [data-quality/2026-04-16-duckdb-dbexiststable-guard.md](../data-quality/2026-04-16-duckdb-dbexiststable-guard.md)
- [testing-patterns/2026-04-16-testing-duckdb-functions.md](../testing-patterns/2026-04-16-testing-duckdb-functions.md)
