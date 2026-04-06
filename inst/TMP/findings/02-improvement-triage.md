# pipapi — Improvement Triage

> **Date**: 2026-04-06
> **Package version**: 1.5.3

Findings are organized into four triage levels:

- **CRITICAL (P1)** — Must fix. Bugs, data correctness risks, security issues.
- **MAJOR (P2)** — Should fix. Performance, reliability, maintainability risks.
- **MINOR (P3)** — Good to fix. Code quality, consistency, readability.
- **NICE-TO-HAVE (P4)** — Enhancements and modernizations.

---

## CRITICAL (P1) — Must Fix

### P1-01: `sprintf` bug in `clear_cache()`

**File**: `R/utils-misc.R`, `clear_cache()` function

**Issue**: Uses `%n` in `sprintf()`, which is not a valid R format specifier:

```r
sprintf('Something went wrong. %n items remain in cache.', n)
```

`%n` in C writes to a pointer — in R, `sprintf("%n", 1)` will crash or produce garbage. Should be `%d`.

**Fix**: Change `%n` to `%d`.

**Impact**: Runtime error when cache reset partially fails.

---

### P1-02: Global environment pollution in `.onLoad()`

**File**: `R/zzz.R`

**Issue**: The line `assign("cd", cd, envir = .GlobalEnv)` injects the cache object into the user's global environment.

```r
assign("cd", cd, envir = .GlobalEnv)
```

**Problems**:
1. Violates R package best practices (packages should never modify `.GlobalEnv`).
2. `R CMD check` will flag this.
3. Can overwrite user variables named `cd`.
4. The `cache-reset`, `cache-keys`, `cache-info` endpoints in `endpoints.R` reference `cd` as a free variable — they depend on this leak.

**Fix**: Store `cd` in the package's private environment (`.pipapienv`) instead. Update all references in `endpoints.R` to use `get_from_pipapienv("cd")`.

---

### P1-03: Duplicate `rg_pip_old()` definitions

**Files**: `R/rg_pip_old.R` and `R/pip_old.R`

**Issue**: Two different implementations of `rg_pip_old()` exist. The one that "wins" at load time depends on file alphabetical order (`pip_old.R` < `rg_pip_old.R`), so `rg_pip_old.R` takes precedence. If the implementations differ, one silently overrides the other.

**Fix**: Remove the duplicate from `R/pip_old.R` or consolidate into one file.

---

### P1-04: Acknowledged incorrect algorithm in `filter_for_aggregate_by()`

**File**: `R/pip_grp.R`

**Issue**: The code contains this comment:

```r
# This algorithm is incorrect, but should mostly work as a first iteration
```

The function selects one row per country/year by preferring national reporting level, but the logic using `check == 1 | (check > 1 & reporting_level == "national")` can silently produce incorrect aggregates for countries with only sub-national data.

**Fix**: Design and implement the correct selection algorithm. Document the expected behavior with test cases.

---

### P1-05: Security — Cache management endpoints exposed without authentication

**File**: `inst/plumber/v1/endpoints.R`

**Issue**: Several destructive endpoints have no authentication:
- `/api/v1/cache-reset` — resets the response cache
- `/api/v1/cache-delete` — **deletes the entire cache directory** via `unlink(cd$info()$dir, recursive = TRUE)`
- `/api/v1/cache-get` — can retrieve any cached value
- `/api/v1/cache-keys` — lists all cache keys

Only `/api/v1/duckdb-reset` checks a password.

**Fix**: Add authentication to all cache management endpoints, or remove them from the public API and expose only via an admin interface.

---

### P1-06: `dir-info` endpoint exposes server filesystem

**File**: `inst/plumber/v1/endpoints.R`

**Issue**: The `/api/v1/dir-info` endpoint returns full filesystem paths:

```r
x <- fs::dir_info(dir, recurse = TRUE, type = "file")
```

This leaks absolute server paths to any API consumer.

**Fix**: Return only relative paths, or restrict this endpoint to authenticated/internal use.

---

## MAJOR (P2) — Should Fix

### P2-01: Remove `R/pip_old.R` — near-complete code duplication

**File**: `R/pip_old.R`

**Issue**: `pip_old()` is a ~200-line function nearly identical to `pip_old_lineups()` in `R/pip_old_lineups.R`. It also contains a duplicate `rg_pip_old()` (see P1-03).

**Fix**: Delete `R/pip_old.R` entirely. `pip_old_lineups()` is the canonical old pathway.

---

### P2-02: Remove deprecated `group_by` code from `pip_old_lineups()`

**File**: `R/pip_old_lineups.R`

**Issue**: Contains two code blocks marked `**** TO BE REMOVED ****` that handle the deprecated `group_by` parameter. This dead code:
1. Forces `fill_gaps = TRUE` as a side effect
2. Bypasses the standard post-processing pipeline
3. Creates a maintenance burden

**Fix**: Remove the `group_by` parameter and the marked blocks. All aggregation should go through `pip_agg()` / `pip_grp()`.

---

### P2-03: `create_lkups()` is too large and does too much

**File**: `R/create_lkups.R`, `create_lkups()` function

**Issue**: ~500 lines in a single function that:
- Loads 10+ data files
- Performs inline data wrangling (joins, pivots, renames)
- Builds interpolation lists
- Creates query controls
- Computes cache hashes
- Has 6+ TODOs about moving logic upstream

**Fix**: Refactor into smaller functions:
- `load_aux_files(data_dir)`
- `build_svy_lkup(data_dir, paths_ids, countries)`
- `build_ref_lkup(data_dir, paths_ids, countries)`
- `build_refy_lkup(data_dir, ...)` (new pathway only)
- `build_interpolation_list(ref_lkup)`
- `compute_cache_hashes(...)`

---

### P2-04: `get_caller_names()` for action-at-a-distance logic

**Files**: `R/utils-misc.R`, `R/utils-lkup.R`

**Issue**: `select_years()` and `subset_ctry_years()` call `get_caller_names()` to inspect the call stack and change behavior based on which parent function invoked them. This is an "action at a distance" anti-pattern that:
1. Makes code difficult to reason about
2. Breaks if call structure changes (refactoring fragility)
3. Is hard to test in isolation

**Fix**: Add an explicit parameter (e.g., `context = c("pip", "agg")`) instead of inspecting the call stack.

---

### P2-05: DuckDB connection management lacks pooling

**File**: `R/duckdb_func.R`

**Issue**: Every cache read/write opens a new DuckDB connection and closes it immediately:
```r
write_con <- connect_with_retry(cache_file_path, read_only = FALSE)
# ... execute query ...
duckdb::dbDisconnect(write_con)
```

Under high API concurrency, this causes:
1. Connection contention (DuckDB is single-writer)
2. Retry storms (`connect_with_retry` up to 5 attempts with 1s delays)
3. Performance overhead from repeated connect/disconnect

**Fix**: Use a connection pool or a single long-lived read connection with a write mutex.

---

### P2-06: `fillin_list()` uses fragile metaprogramming

**File**: `R/utils-misc.R`

**Issue**: Uses `parent.frame()`, `deparse(substitute())`, and `get()` to look up variables from the calling frame. This breaks under:
- `tryCatch()` wrappers
- `do.call()` invocations
- Future refactoring

**Fix**: Replace with explicit parameter passing (e.g., accept a named list or environment).

---

### P2-07: `return_if_exists()` is overly complex

**File**: `R/duckdb_func.R`

**Issue**: ~130 lines with complex branching, multiple `ZP comment`/`ZP Question` annotations indicating uncertainty, and inline ad-hoc transformations (`slkup[, survey_comparability := NA_real_]`). Multiple code paths return early with different structures.

**Fix**: Simplify into distinct helper functions:
- `check_cache_availability()` — returns TRUE/FALSE
- `partition_cached_vs_new()` — returns two clean data.tables
- `update_povline_from_cache()` — adjusts which poverty lines need computation

---

### P2-08: Hardcoded country exclusion in `get_aux_table_ui()`

**File**: `R/get_aux_table.R`

**Issue**: `to_remove = c("UKR")` is hardcoded. If additional countries need exclusion, this requires a code change and redeployment.

**Fix**: Move to a configuration file or an aux table that can be updated with data.

---

### P2-09: Missing test coverage for critical paths

**Gap analysis** (files with no dedicated tests):

| File | Risk |
|---|---|
| `R/infer_poverty_line.R` | No tests. Handles popshare → poverty_line conversion. |
| `R/copy_functions.R` | No tests. Contains local copies of `wbpip`/`pipster` functions. |
| `R/duckdb_func.R` | Only `clear_cache` tested. `return_if_exists()`, `update_master_file()`, `connect_with_retry()` untested. |
| `R/pip_grp_new.R` | No dedicated test file. |
| `R/pipapi-env.R` | No tests for the environment getters/setters. |
| `R/fgt_cumsum.R` | Has tests but edge cases (empty surveys, zero population, single observation) may be underexplored. |

---

### P2-10: Inconsistent join styles

**Issue**: The codebase mixes three join approaches:
1. `data.table` `X[Y, on=]` syntax
2. `collapse::join()` 
3. `joyn::joyn()`

This hurts readability and makes it harder to reason about join behavior (especially left vs inner vs anti).

**Fix**: Standardize on `collapse::join()` for simple joins and `data.table` for complex cases, per the R instruction file hierarchy. Phase out `joyn::joyn()` where possible.

---

### P2-11: `copy_functions.R` contains vendored upstream code

**File**: `R/copy_functions.R`

**Issue**: Contains copies of functions from `wbpip` and `pipster` packages, with a header comment saying it "needs to be cleaned up". If the upstream packages change, these copies become stale.

**Fix**: Either:
1. Use the upstream functions directly (preferred)
2. If custom behavior is needed, document exactly what differs and why

---

## MINOR (P3) — Good to Fix

### P3-01: Inconsistent error handling style

**Issue**: Mix of error reporting approaches:
- `cli::cli_abort()` (new code)
- `rlang::abort()` (some utilities)
- `stop()` (old code, `create_lkups.R`)
- `stopifnot()` (scattered)

Per the R instruction file: use `cli::cli_abort()` for user-facing, `rlang::abort()` for internal.

**Fix**: Standardize on `cli::cli_abort()` for all user-facing errors. Replace bare `stop()` with `cli::cli_abort()`.

---

### P3-02: `is_empty()` can be simplified

**File**: `R/utils-misc.R`

**Issue**: `length(x) == 0 & !is.null(x)` uses `&` (vectorized AND) instead of `&&` (short-circuit AND). While it works for scalar inputs, it's technically incorrect.

**Fix**: Use `&&` instead of `&`.

---

### P3-03: Content-Security-Policy header is overly permissive

**File**: `inst/plumber/v1/endpoints.R`, `response_headers` filter

**Issue**: The CSP header includes `'unsafe-inline' 'unsafe-eval' 'unsafe-dynamic'` and `*` sources for nearly everything. This effectively disables CSP protection.

**Fix**: Tighten CSP to reflect actual needs (the API serves JSON/CSV, not HTML with scripts).

---

### P3-04: `%||%` operator defined twice

**Files**: `inst/plumber/v1/plumber.R` and `inst/plumber/v1/endpoints.R`

**Issue**: The null-coalescing operator `%||%` is defined identically in both files. R 4.4+ includes `%||%` in base.

**Fix**: Use `rlang::%||%` or base R's `%||%` (R ≥ 4.4) instead of defining locally.

---

### P3-05: Numerous `TODO` comments remain unresolved

Across the codebase, at least 12 TODO/FIXME comments indicate known technical debt:

| Location | TODO |
|---|---|
| `create_lkups.R` (×6) | Move data prep upstream |
| `fg_pip.R` | Fix inefficient popshare path |
| `utils-lkup.R` (×2) | Remove `filter_lkup()` when popshare supported; handle `wb_region_code` |
| `pip_old_lineups.R` (×2) | Remove deprecated `group_by` blocks |
| `get_aux_table.R` | Remove hardcoded `UKR` exclusion |

---

### P3-06: `unnest_dt_longer()` uses `rlang::expr()` unnecessarily

**File**: `R/utils-misc.R`

**Issue**: Uses `rlang::syms()` and `rlang::expr()` for NSE that could be done with plain `data.table` syntax.

**Fix**: Rewrite using standard `data.table` syntax to reduce dependency on `rlang` for this function.

---

### P3-07: Missing `@family` tags in roxygen2

**Issue**: Related functions (e.g., `pip()`, `pip_new_lineups()`, `pip_old_lineups()`) lack `@family` tags, making it harder to navigate related functions in documentation.

---

### P3-08: `pip_grp()` and `pip_agg()` overlap

**File**: `R/pip_grp.R` and `R/pip_agg.R`

**Issue**: Both are exported and perform aggregation, but `pip_grp()` is the old entry point while `pip_agg()` is the new dispatcher. The API endpoint `/api/v1/pip-grp` calls `pip_agg()`, not `pip_grp()`. Having both exported is confusing.

**Fix**: Deprecate `pip_grp()` with a `.Deprecated()` call pointing to `pip_agg()`, or make `pip_grp()` internal.

---

### P3-09: OpenAPI spec is incomplete

**File**: `inst/plumber/v1/openapi.yaml`

**Issue**: 
1. Only documents a subset of endpoints (missing UI endpoints, cache endpoints, etc.)
2. No response schemas defined (just "Default response")
3. Country/year enums are illustrative, not exhaustive
4. Missing error response definitions (400, 404, 500)

---

### P3-10: `@import collapse` and `@import data.table` in NAMESPACE

**File**: `R/pipapi-package.R`

**Issue**: `@import collapse` and `@import data.table` import entire namespaces. The R instruction file recommends `@importFrom` for selective imports.

**Impact**: Higher risk of namespace collisions (both packages export `fdroplevels`, hence the `except = fdroplevels` clause). More functions polluting the package namespace than needed.

**Fix**: Gradually replace with `@importFrom` for specific functions used. This is a large effort but improves namespace hygiene.

---

## NICE-TO-HAVE (P4) — Enhancements

### P4-01: Rate limiting on the API

**Issue**: No rate limiting is configured. A single client can send unlimited requests, potentially overwhelming the server.

**Enhancement**: Add `plumber`-compatible rate limiting, either via a filter or an upstream reverse proxy.

---

### P4-02: Request logging to structured log file

**Issue**: Logs go to stderr as JSON strings. No structured logging framework, no log rotation, no log levels.

**Enhancement**: Use a structured logging package (e.g., `logger`) with configurable log levels, file rotation, and optional JSON output for log aggregation systems.

---

### P4-03: API versioning strategy

**Issue**: Currently `v1` only. No versioning strategy documented for breaking changes.

**Enhancement**: Document API versioning policy. Consider header-based versioning or URL-based (v2) for future breaking changes.

---

### P4-04: Health check should verify data availability

**Issue**: `/api/v1/health-check` returns a static string. It does not verify that:
- Data directories are accessible
- `lkups` are loaded
- DuckDB cache is reachable

**Enhancement**: Add a "deep" health check that validates data layer connectivity.

---

### P4-05: Replace `fst` with Parquet/Arrow for data storage

**Issue**: `.fst` format is fast but niche. The `fst` package has had limited maintenance.

**Enhancement**: Evaluate migrating to Parquet (via `arrow`) for:
- Broader ecosystem compatibility
- Better compression
- Column pruning on read
- Cross-language compatibility

---

### P4-06: Add OpenAPI response schemas

**Enhancement**: Define proper JSON schemas for all endpoint responses in `openapi.yaml`. This enables:
- Auto-generated client SDKs
- Request/response validation
- Better Swagger UI documentation

---

### P4-07: Dockerize the API

**Enhancement**: Provide a `Dockerfile` for reproducible deployment:
- Pin R version and system dependencies
- Include `renv.lock` for package versions
- Set environment variables for caching configuration
- Health check in Docker compose

---

### P4-08: Migrate `memoise` caching to faster backend

**Issue**: `cachem::cache_disk` with `qs2` serialization is good, but for high-concurrency production use, Redis or a similar shared cache would be more appropriate.

**Enhancement**: Abstract the caching layer behind an interface, allowing swappable backends (disk, Redis, memcached).

---

### P4-09: Add request tracing / correlation IDs

**Issue**: Current request IDs are generated in the API layer but not propagated to downstream function calls or logs.

**Enhancement**: Propagate request IDs through the computation pipeline for end-to-end debugging.

---

### P4-10: Consider async/parallel endpoint handling

**Issue**: Plumber handles requests sequentially by default. Long-running computations block all other requests.

**Enhancement**: Evaluate `plumber`'s `future` integration or a multi-process deployment strategy (multiple workers behind a load balancer).

---

### P4-11: Add `pkgdown` articles for API usage

**Enhancement**: The `_pkgdown.yml` exists but the docs site could benefit from vignettes showing:
- How to set up and run the API locally
- Common API query patterns
- How to extend with new endpoints
- Data preparation pipeline

---

### P4-12: Standardize on one pipe operator

**Issue**: Mix of `|>` (modern base R) and occasional `%>%` (magrittr) in older code.

**Fix**: Standardize on `|>` throughout, per R instruction file.
