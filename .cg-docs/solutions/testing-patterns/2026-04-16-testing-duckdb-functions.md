---
date: 2026-04-16
title: "Testing DuckDB functions with ephemeral in-process databases"
category: "testing-patterns"
language: "R"
tags: [duckdb, DBI, testthat, withr, cache, ephemeral, unit-test]
root-cause: "DuckDB functions are hard to test without a real file; withr + local_tempdir makes it trivial"
severity: "P2"
---

# Testing DuckDB functions with ephemeral in-process databases

## Problem

Functions that read from or write to a DuckDB file (e.g. cache helpers like
`reset_cache`, `create_duckdb_file`) are hard to unit-test because they need
a real file-backed database. Tests that rely on production data directories
are slow, fragile, and environment-dependent.

## Root Cause

No established pattern existed for spinning up a throw-away DuckDB file inside
a testthat test, so tests either skipped the function entirely or required
external environment variables.

## Solution

Use `withr::local_tempdir()` to create an isolated, automatically-cleaned-up
temp directory, then connect DuckDB to a file inside it. Combine with
`withr::local_envvar()` for any env-var dependencies.

```r
test_that("reset_cache does not error when cache tables do not exist", {
  tmp        <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  # Empty DuckDB file — no tables
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  duckdb::dbDisconnect(con)

  lkup_mock <- list(data_root = tmp)
  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY  = "test-key",
    PIP_CACHE_SERVER_KEY = "test-key"
  )

  expect_no_error(reset_cache(pass = "test-key", lkup = lkup_mock))
})

test_that("reset_cache deletes rows when tables exist", {
  tmp        <- withr::local_tempdir()
  cache_path <- fs::path(tmp, "cache", ext = "duckdb")

  create_duckdb_file(cache_path)   # uses the real schema creator

  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  DBI::dbExecute(con, "INSERT INTO rg_master_file VALUES ('id1','national',2.15,0.1,0.05,0.02,0.01)")
  duckdb::dbDisconnect(con)

  lkup_mock <- list(data_root = tmp)
  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY  = "test-key",
    PIP_CACHE_SERVER_KEY = "test-key"
  )

  reset_cache(pass = "test-key", lkup = lkup_mock)

  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_path)
  n   <- DBI::dbGetQuery(con, "SELECT count(*) as n FROM rg_master_file")$n
  duckdb::dbDisconnect(con)

  expect_equal(n, 0L)
})
```

### Key techniques

| Technique | Purpose |
|---|---|
| `withr::local_tempdir()` | Auto-cleaned temp dir scoped to the test |
| `withr::local_envvar()` | Temporarily sets env vars without polluting other tests |
| Connect → Disconnect → reconnect | DuckDB requires connections to be closed before another process opens the file |
| Pass `lkup_mock <- list(data_root = tmp)` | Minimal mock: only supply the fields the function actually uses |

## Prevention

- Always use `withr::local_*` helpers instead of `tempdir()` + manual cleanup.
- Never open two simultaneous DuckDB connections to the same file in a test.
- Mock only the fields of `lkup` that the function under test actually accesses —
  avoids coupling tests to the full `lkup` structure.
- Use `create_duckdb_file()` in tests that need the real schema, so tests stay
  in sync with schema changes automatically.

## Related

- [bugs/2026-04-16-reset-cache-missing-tables.md](../bugs/2026-04-16-reset-cache-missing-tables.md) — the bug that surfaced this pattern
- [data-quality/2026-04-16-duckdb-dbexiststable-guard.md](../data-quality/2026-04-16-duckdb-dbexiststable-guard.md) — team-wide convention: always guard DuckDB table operations with `dbExistsTable()`
