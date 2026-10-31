---
date: 2026-10-31
title: "Pairing-filtered vintage startup isolation and strict YAML version validation in {pipapi}"
category: "data-quality"
language: "R"
tags: [pipapi, pipschema, pairing, startup, create_versioned_lkups, yaml, tryCatch, regex, cache-v2]
root-cause: "Implicit full-directory /Data scanning, hard-coded May 2025 date heuristic for lineup schema selection, unguarded mapply(create_lkups), and base R as.integer() truncation on YAML version fields"
severity: "P1"
---

# Pairing-Filtered Vintage Startup Isolation and Strict YAML Version Validation in `{pipapi}`

## Problem

In the multi-container code-data versioning architecture for `{pipapi}`, each container serves a frozen `(schema, pipapi_version)` pairing from `pairing.yml` while mounting a shared flat `/Data` directory. Four startup failure modes existed:
1. **Unscoped directory scanning**: `create_versioned_lkups(data_dir)` scanned all folders under `/Data` matching the general vintage regex, attempting to load incompatible schemas into the same container.
2. **Hard-coded date cutoff**: `create_lkups()` relied on `use_new_lineup_version()` (`> 2025-05-01`) to decide whether to load new-lineup tables (`prod_refy_estimation.fst`, `lineup_years.fst`, `lineup_dist_stats.fst`), rather than using the explicit `schema` declared by the pairing.
3. **All-or-nothing startup**: `create_versioned_lkups()` called `mapply(create_lkups, ...)` with no error boundary, so a single unreadable or corrupted vintage folder aborted the entire API container startup.
4. **Silent YAML version truncation and empty selector traps**: Validating `pairing_file_version` with `identical(as.integer(ver), 1L)` silently truncated fractional versions like `1.2` to `1L` (and emitted base R coercion warnings on strings like `"v1"`), while calling `read_pairing_vintages(path)` with all `NULL` selectors returned the union of all bindings across incompatible schemas.

## Root Cause

- `create_versioned_lkups()` was originally designed when a single `{pipapi}` container loaded every vintage folder in `/Data` and inferred schema structure from folder-name dates.
- When parsing `pairing.yml` via `yaml::read_yaml()`, numeric values like `1.2` are parsed as `double`; coercing via `as.integer()` truncates the fractional part before comparison.
- When skipping failed vintages in `create_versioned_lkups()`, `versions` and `names(versions_paths)` must remain strictly identical (`identical(as.character(lkups$versions), names(lkups$versions_paths))`) to satisfy downstream `.cache_v2_prepare_lkups()` invariants in `R/start_api.R`, and falling back from `versions[1]` shifts `latest_release` for unversioned API requests.

## Solution

1. **Pairing YAML reader with strict selector and version guards (`read_pairing_vintages`)**:
   - Require at least one non-`NULL` selector (`schema`, `pipapi_version`, or `pipapi_sha`).
   - Validate `pairing_file_version` against exact scalar values `list(1L, 1, "1")` without `as.integer()` truncation:
     ```r
     ver <- pairing$pairing_file_version
     if (length(ver) != 1L || is.na(ver) ||
         !any(vapply(list(1L, 1, "1"), identical, logical(1), ver))) {
       cli::cli_abort("Unsupported pairing_file_version: {.val {ver}} (expected 1).")
     }
     ```
   - Escape regex metacharacters with `perl = TRUE` (`gsub("([.\\\\|()[{}\\\\^$+*?\\]])", "\\\\\\1", x, perl = TRUE)`) in `build_vintage_pattern()` (note: TRE without `perl = TRUE` fails on `]` inside character classes or silently misses metacharacters).

2. **Explicit `schema` override (`resolve_use_new_lineup`)**:
   - Pass `schema = "new-lineup"` or `"pre-lineup"` to `create_versioned_lkups()` and `create_lkups()` to set `use_new_lineup_version` directly, falling back to the date heuristic only when `schema = NULL`.

3. **Per-vintage `tryCatch()` isolation with synchronized pruning and `latest_release` fallback warning**:
   ```r
   versions_paths <- Map(
     function(d, v) {
       tryCatch(
         create_lkups(data_dir = d, versions = v, schema = schema),
         error = function(err) {
           cli::cli_warn(c(
             "Failed to load vintage folder {.val {v}} at {.path {d}}.",
             x = conditionMessage(err),
             i = "Skipping this vintage; it will not be served by this container."
           ))
           NULL
         }
       )
     },
     data_dirs,
     versions
   )

   expected_latest <- versions[1]
   keep <- !vapply(versions_paths, is.null, logical(1))
   versions <- versions[keep]
   versions_paths <- versions_paths[keep]
   names(versions_paths) <- versions

   if (length(versions) == 0L) {
     cli::cli_abort("All candidate vintage folders failed to load in {.path {data_dir}}.")
   }
   if (!identical(versions[1], expected_latest)) {
     cli::cli_warn(c(
       "Primary vintage {.val {expected_latest}} failed to load.",
       i = "Falling back to {.val {versions[1]}} as {.field latest_release}."
     ))
   }
   ```

4. **Direct commit tagging for frozen `.900x` SHAs**:
   - Because `pipschema::write_pairing()` freezes `pipapi_sha` permanently and only permits a one-time `.900x` version fill on `pipapi_version`, historical commits with development versions in `DESCRIPTION` (such as `089c3fc877e003259694fb9b6aef8626a317bb53` with `1.3.7.9006`) must be tagged directly on the exact commit SHA (`git tag -a v1.3.22 089c3fc8...`) rather than creating a new commit to edit `DESCRIPTION`.

## Prevention

- Never use `as.integer()` to validate integer schema version fields parsed from YAML/JSON; check exact equality against `list(1L, 1, "1")` or verify `ver == floor(ver)`.
- When using `gsub()` in R to escape regex metacharacters including `[` and `]`, always pass `perl = TRUE` and test against a string containing all regex metacharacters (`"a+b*c?d(e)f[g]{h}^i$j|k"`).
- Whenever filtering `versions_paths` in `create_versioned_lkups()`, always filter `versions` with the exact same logical mask and warn explicitly if `versions[1]` (`latest_release`) changes.

## Related

- `.cg-docs/architecture/code-data-versioning.md`
- `.cg-docs/plans/2026-10-31-pairing-filtered-startup-isolation.md`
- `.cg-docs/reviews/2026-10-31-pairing-filtered-startup-isolation-review.md`
