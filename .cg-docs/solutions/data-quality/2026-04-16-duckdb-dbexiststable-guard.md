---
date: 2026-04-16
title: "Always guard DuckDB table operations with dbExistsTable()"
category: "data-quality"
language: "R"
tags: [duckdb, DBI, dbExistsTable, catalog-error, missing-table, defensive-coding, cache]
root-cause: "DuckDB raises a Catalog Error on any DML or query against a table that does not exist; file existence does not imply schema existence"
severity: "P2"
---

# Always guard DuckDB table operations with dbExistsTable()

## Problem

Three separate functions in `R/duckdb_func.R` independently threw the same
Catalog Error on a fresh `.duckdb` file (one that exists on disk but has never
had `CREATE TABLE` run against it):

```
Catalog Error: Table with name rg_master_file does not exist!
```

The functions affected were:
1. `reset_cache()` — `DELETE FROM rg_master_file` (error, commit `d67c005`)
2. `update_master_file()` — `PRAGMA table_info(rg_master_file)` (error, commit `95795d3`)
3. `load_inter_cache()` — `SELECT * FROM rg_master_file` (warning via upstream `tryCatch`, commit `4df2714`)

All three hit the same condition: the `.duckdb` file existed (created by
`duckdb::dbConnect`) but neither `rg_master_file` nor `fg_master_file` had
been created yet because `create_duckdb_file()` had never been called for
that data vintage.

## Root Cause

DuckDB — like most real databases — distinguishes between the database file
existing and a table inside it existing. Opening a connection to a new file
creates an empty database. Referencing a table name that hasn't been created
raises a `CATALOG` error immediately, before any rows are read or written.

The codebase assumed that if the `.duckdb` file existed, the schema was
already initialized. This assumption breaks on first run of any new data
vintage.

## Solution

Add a `DBI::dbExistsTable()` guard before every DML or query on a named
DuckDB table.

### reset_cache — skip DELETE when table absent
```r
if ("rg" %in% type && DBI::dbExistsTable(write_con, "rg_master_file")) {
  DBI::dbExecute(write_con, "DELETE FROM rg_master_file")
}
```

### update_master_file — create schema lazily on first write
```r
if (
  !DBI::dbExistsTable(write_con, "rg_master_file") ||
    !DBI::dbExistsTable(write_con, "fg_master_file")
) {
  duckdb::dbDisconnect(write_con)
  create_duckdb_file(cache_file_path)
  write_con <- connect_with_retry(cache_file_path, read_only = FALSE)
}
```

### load_inter_cache — return empty data.table when table absent
```r
if (!DBI::dbExistsTable(con, target_file)) {
  duckdb::dbDisconnect(con)
  return(data.table::data.table())
}
```

## Prevention

### Rule
> **Before every `SELECT`, `INSERT`, `DELETE`, `PRAGMA table_info`, or any
> other statement that references a named DuckDB table by name, call
> `DBI::dbExistsTable(con, table_name)` and handle the absent-table case
> explicitly.**

### Pattern options by operation type

| Operation | When table absent, do… |
|---|---|
| Read (`SELECT *`) | Return empty `data.table()` |
| Write (`INSERT INTO`) | Call `create_duckdb_file()` first, then proceed |
| Delete (`DELETE FROM`) | Skip silently (cache is already empty) |
| Schema introspection (`PRAGMA table_info`) | Call `create_duckdb_file()` first |

### Anti-patterns to avoid
- Inferring table existence from file existence (`fs::file_exists(cache_path)`)
- Relying on `tryCatch` to swallow Catalog Errors instead of preventing them
- Assuming `create_duckdb_file()` has been called by a previous step

## Related

- [bugs/2026-04-16-reset-cache-missing-tables.md](../bugs/2026-04-16-reset-cache-missing-tables.md) — first instance (`reset_cache`)
- [bugs/2026-04-16-load-inter-cache-missing-tables.md](../bugs/2026-04-16-load-inter-cache-missing-tables.md) — third instance (`load_inter_cache`)
- [testing-patterns/2026-04-16-testing-duckdb-functions.md](../testing-patterns/2026-04-16-testing-duckdb-functions.md) — how to unit-test DuckDB functions safely
