---
date: 2026-10-31
depth: architecture
type: standard
plan: .cg-docs/plans/2026-10-31-pairing-filtered-startup-isolation.md
findings:
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P2.5: fixed
  P2.6: fixed
  P2.7: open
  P2.8: open
  P3.1: fixed
  P3.2: fixed
  P3.3: fixed
  P3.4: skipped
---

## Review Report

**Review mode**: architecture
**Files reviewed**: 4 (`R/create_lkups.R`, `tests/testthat/test-create_lkups.R`, `NAMESPACE`, `DESCRIPTION`)
**Findings**: 16 (P0: 0, P1: 4, P2: 8, P3: 4)

### P0 — BLOCKING (immediate remediation required)
*(None)*

### P1 — CRITICAL (must fix before merge)

- **[P1.1]** [cg-architecture] [manual] `R/create_lkups.R:46` — `read_pairing_vintages()` returns the union of all bindings when all filter arguments (`schema`, `pipapi_version`, `pipapi_sha`) are `NULL`
  **Why**: When `schema`, `pipapi_version`, and `pipapi_sha` are all omitted (`NULL`), `matched` retains every entry in `pairing$bindings`, returning folders across incompatible schema versions and `pipapi` releases. During container startup, an unconfigured or `NULL` filter silently loads incompatible vintages instead of failing fast.
  **Fix**: Require at least one selector (`schema`, `pipapi_version`, or `pipapi_sha`) to be non-`NULL` in `read_pairing_vintages()`, aborting via `cli::cli_abort()` if all three are `NULL`.

- **[P1.2]** [cg-architecture] [manual] `R/create_lkups.R:153` — Partial vintage load failure silently reassigns `latest_release` to an older vintage without an explicit warning
  **Why**: `create_versioned_lkups()` catches per-vintage errors, drops failed entries from `versions`, and then sets `latest_release = versions[1]`. If the newest vintage (`versions[1]` before filtering) fails to load, the container promotes an older vintage to `latest_release` without emitting a dedicated warning that `latest_release` changed.
  **Fix**: Emit an explicit warning via `cli::cli_warn()` when the originally first (latest) candidate vintage fails and `latest_release` falls back to an older surviving vintage.

- **[P1.3]** [cg-data-quality] [manual] `R/create_lkups.R:57` — `pairing_file_version` validation uses `as.integer()`, which silently truncates non-integer versions (e.g., `1.2` or `"1.9"` becomes `1L`) and emits unguarded coercion warnings on non-numeric strings
  **Why**: In YAML, `pairing_file_version: 1.2` is parsed as numeric `1.2`. `identical(as.integer(1.2), 1L)` evaluates to `TRUE`, silently accepting incompatible minor schema changes instead of failing fast, while strings like `"v1"` emit `NAs introduced by coercion` before `cli::cli_abort()`.
  **Fix**: Validate that `pairing$pairing_file_version` is a scalar equal to `1L`, `1`, or `"1"` without truncating fractional numbers or emitting base R coercion warnings.

- **[P1.4]** [cg-data-quality] [manual] `R/create_lkups.R:83` — `read_pairing_vintages()` does not validate that matched bindings contain non-empty, non-`NA` `folders` entries
  **Why**: If a matched binding in the YAML file has an empty `folders: []`, `folders: ~`, or blank strings, `unlist(lapply(...))` and `sort(unique(...))` return `character(0)` instead of signaling a corrupt pairing entry.
  **Fix**: Filter out `NA` and blank folder strings and call `cli::cli_abort()` if `length(folders) == 0L` after extraction.

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-code-quality] [safe_auto] `R/create_lkups.R:118` — Growing `failed_vintages` in a `for` loop lengthens `create_versioned_lkups()`
  **Why**: `failed_vintages <- c(failed_vintages, v)` grows a vector inside the loop even though failed entries are already represented as `NULL` in `versions_paths`.
  **Fix**: Derive `keep <- !vapply(versions_paths, is.null, logical(1))` directly from `versions_paths` and subset `versions[keep]` and `versions_paths[keep]`.

- **[P2.2]** [cg-testing] [safe_auto] `tests/testthat/test-create_lkups.R:339` — Incomplete test for `create_lkups()` explicit `schema` override
  **Why**: The test `"create_lkups: explicit schema overrides date heuristic"` only tests `schema = "invalid-schema"` and does not assert that `schema = "new-lineup"` and `schema = "pre-lineup"` override `use_new_lineup_version()` for pre-2025 and post-2025 folder dates.
  **Fix**: Extract or test the schema-resolution logic (`resolve_use_new_lineup(versions, schema)`) and verify both `"new-lineup"` (`TRUE` on a 2022 folder) and `"pre-lineup"` (`FALSE` on a 2026 folder) alongside the invalid-schema error check.

- **[P2.3]** [cg-testing] [safe_auto] `tests/testthat/test-create_lkups.R:262` — Untested validation branches and case-insensitive SHA matching in `read_pairing_vintages()`
  **Why**: `read_pairing_vintages()` validates malformed YAML states (missing `pairing_file_version`, unsupported `pairing_file_version != 1`, empty `bindings`) and normalizes `pipapi_sha` with `tolower()`, which are not all covered in `test-create_lkups.R`.
  **Fix**: Add test assertions covering missing `pairing_file_version`, unsupported `pairing_file_version` (`2L` and `1.2`), empty `bindings = list()`, and uppercase `pipapi_sha` matching lowercase SHA in the YAML file.

- **[P2.4]** [cg-documentation] [safe_auto] `R/create_lkups.R:18` — Exported functions `build_vintage_pattern()`, `read_pairing_vintages()`, and `create_versioned_lkups()` are missing `@examples` in roxygen2 documentation
  **Why**: Exported R package functions require `@examples` to demonstrate typical usage and satisfy GPID package standards.
  **Fix**: Add `@examples` blocks to `build_vintage_pattern()`, `read_pairing_vintages()`, and `create_versioned_lkups()`, and expand `@return` on `create_versioned_lkups()`, then re-run `roxygen2::roxygenise()`.

- **[P2.5]** [cg-data-quality] [safe_auto] `R/create_lkups.R:46` — `pairing_path` and filter arguments (`schema`, `pipapi_version`, `pipapi_sha`) are not validated as non-`NA` scalar strings before use
  **Why**: Passing `NULL`, `character(0)`, `NA_character_`, or a vector of length > 1 to `pairing_path` causes `file.exists(pairing_path)` to fail with a base R error, and `NA` filter arguments could match `NA` fields.
  **Fix**: Validate `pairing_path` and non-`NULL` filter parameters (`schema`, `pipapi_version`, `pipapi_sha`) as scalar non-`NA`, non-empty character strings.

- **[P2.6]** [cg-data-quality] [safe_auto] `R/create_lkups.R:18` — `build_vintage_pattern()` coerces non-character inputs via `as.character()` and checks `nzchar(trimws(folders))` without trimming `folders` or enforcing character type
  **Why**: Passing non-character inputs (such as `TRUE` or numeric vectors) silently coerces them into regex patterns instead of rejecting them, and untrimmed whitespace in `folders` is preserved in the regex pattern.
  **Fix**: Check `if (!is.character(folders) || length(folders) == 0L)` before coercion, and assign `folders <- trimws(folders)` before filtering with `nzchar(folders)`.

- **[P2.7]** [cg-architecture] [manual] `R/create_lkups.R:103` — Duplicate filtering contract between `vintage_pattern` and `vintage_folders`
  **Why**: `create_versioned_lkups()` converts `vintage_folders` into `vintage_pattern` via `build_vintage_pattern()` and also passes `vintage_folders` to `extract_data_dirs()`.
  **Fix**: Keep both parameters supported cleanly so `vintage_folders` acts as an exact-set filter in `extract_data_dirs()` even when a custom `vintage_pattern` list (with `prod_regex`/`int_regex`/`test_regex`) is supplied.

- **[P2.8]** [cg-architecture] [manual] `R/create_lkups.R:1` — Pairing manifest parsing and regex utilities are placed in `R/create_lkups.R`
  **Why**: `R/create_lkups.R` holds both pairing YAML helpers and lookup table construction.
  **Fix**: Keep in `R/create_lkups.R` per the approved Workstream B plan boundary (`Allowed: R/create_lkups.R`), or extract to a separate file in a future refactor.

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] [safe_auto] `R/create_lkups.R:66` — Repeated `Filter()` boilerplate and lines exceeding 80 characters
  **Why**: Lines 67, 70, and 73 repeat `Filter(function(b) identical(as.character(...)), matched)` and exceed 80 characters.
  **Fix**: Extract a compact field-matching helper and wrap lines within 80 characters.

- **[P3.2]** [cg-code-quality] [safe_auto] `R/create_lkups.R:166` — `extract_data_dirs()` adds `vintage_folders = NULL` parameter without updating roxygen `@param`
  **Why**: Roxygen block above `extract_data_dirs()` uses `@inheritParams create_versioned_lkups`, which inherits `vintage_folders` once `create_versioned_lkups()` documents it; ensure roxygen docs stay aligned.
  **Fix**: Verify `@inheritParams create_versioned_lkups` covers `vintage_folders`.

- **[P3.3]** [cg-testing] [safe_auto] `tests/testthat/test-create_lkups.R:234` — Unnecessary `pipapi:::` prefix and prefer `local_mocked_bindings()`
  **Why**: Internal functions are already in scope during `testthat` runs, and `local_mocked_bindings()` is preferred over `with_mocked_bindings()`.
  **Fix**: Call `escape_vintage_regex()` directly and use `local_mocked_bindings()` in `test-create_lkups.R`.

- **[P3.4]** [cg-documentation] [advisory] `DESCRIPTION:80` — `Config/roxygen2/version: 8.0.0` vs `RoxygenNote: 7.3.2`
  **Why**: Local environment has `roxygen2` 7.3.2 installed, which writes `RoxygenNote: 7.3.2` when `roxygenise()` runs.
  **Fix**: Remove the extra `RoxygenNote: 7.3.2` line in `DESCRIPTION` if `Config/roxygen2/version: 8.0.0` is the repo standard.

### ✅ Passed
- `@cg-version-control`: No issues found (`.Rbuildignore` already excludes `^\.cg-docs$`).
- `@cg-performance`: No issues found.
