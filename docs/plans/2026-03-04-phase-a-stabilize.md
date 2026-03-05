---
date: 2026-03-04
title: "Phase A — Stabilize: snapshot baseline + dead code removal"
status: active
brainstorm: "docs/brainstorms/2026-03-04-pipapi-stability-refactor.md"
language: "R"
estimated-effort: "medium"
tags: [refactoring, dead-code, snapshot-tests, stability, phase-a]
---

# Plan: Phase A — Stabilize (Make It Safe to Change)

## Objective

Create a regression safety net (snapshot tests) and remove all dead code from the
core pipeline, so that subsequent refactoring phases (B and C) can proceed with
confidence that nothing breaks silently.

## Context

The brainstorm decided on a three-stage approach: Stabilize → Restructure → Harden.
This plan covers **Stage A** with two sub-phases:

- **A1**: Generate snapshot `.rds` baselines for the new pathway (user runs locally)
- **A2**: Remove dead code (debug statements, commented-out blocks, marked-for-removal
  code, unused functions)

The old pathway is explicitly **frozen** — we do not touch or optimize it, but we also
do not delete it (it's still needed for older data versions).

## Implementation Steps

### A1. Snapshot Baseline Tests

**Objective**: Capture current output of key new-pathway functions as `.rds` files,
then write testthat tests that compare future runs against these snapshots.

#### A1.1 Create snapshot generation script

- **File to create**: `tests/testdata/generate_snapshots.R`
- **Details**: Script that the user runs locally with `PIPAPI_DATA_ROOT_FOLDER_LOCAL` set.
- **Output location**: `tests/testdata/snapshots/`
- **Acceptance criteria**: Script runs without error and generates 8 `.rds` files.

Run this script once to generate the snapshot files:

```r
# tests/testdata/generate_snapshots.R
#
# PURPOSE: Generate snapshot .rds files for regression testing.
# Run this script manually whenever the data version changes and you want to
# update the baseline. Requires PIPAPI_DATA_ROOT_FOLDER_LOCAL to be set.
#
# Usage (from project root):
#   source("tests/testdata/generate_snapshots.R")

library(pipapi)
library(fs)

# --- Setup -------------------------------------------------------------------

data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")
if (data_dir == "") {
  stop("PIPAPI_DATA_ROOT_FOLDER_LOCAL is not set. Cannot generate snapshots.")
}

lkups <- create_versioned_lkups(data_dir = fs::path(data_dir))
lkup  <- lkups$versions_paths[[lkups$latest_release]]

snap_dir <- fs::path("tests", "testdata", "snapshots")
fs::dir_create(snap_dir)

# Record the data version used to generate these snapshots
writeLines(
  c(
    paste("Generated:", Sys.time()),
    paste("Data version:", lkups$latest_release),
    paste("pipapi version:", as.character(packageVersion("pipapi")))
  ),
  fs::path(snap_dir, "snapshot_manifest.txt")
)

# --- Helper ------------------------------------------------------------------

save_snap <- function(expr, name) {
  message("Generating: ", name)
  result <- tryCatch(
    force(expr),
    error = function(e) {
      warning("FAILED generating ", name, ": ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(result)) {
    saveRDS(result, fs::path(snap_dir, paste0(name, ".rds")))
    message("  -> saved (", nrow(result), " rows)")
  }
}

# --- Snapshots ---------------------------------------------------------------

# 1. Single country, single survey year
save_snap(
  pip("AGO", year = 2000, povline = 1.9, lkup = lkup),
  "snap_pip_ago_2000"
)

# 2. Single country, all survey years
save_snap(
  pip("AGO", year = "ALL", povline = 1.9, lkup = lkup),
  "snap_pip_ago_all"
)

# 3. Single country, fill gaps (lineup years)
save_snap(
  pip("AGO", year = "ALL", povline = 1.9, fill_gaps = TRUE, lkup = lkup),
  "snap_pip_ago_fg"
)

# 4. All countries, single year
save_snap(
  pip("ALL", year = 2015, povline = 1.9, lkup = lkup),
  "snap_pip_all_2015"
)

# 5. Multi-reporting-level country (national/rural/urban)
save_snap(
  pip("CHN", year = 2018, povline = 1.9, reporting_level = "all", lkup = lkup),
  "snap_pip_chn_2018"
)

# 6. Aggregation via pip_agg (new pathway)
save_snap(
  pip_agg("ALL", year = 2015, povline = 1.9, group_by = "wb", lkup = lkup),
  "snap_agg_all_2015"
)

# 7. Multiple poverty lines
save_snap(
  pip("AGO", year = 2000, povline = c(1.9, 3.65, 6.85), lkup = lkup),
  "snap_pip_ago_multi_pl"
)

# 8. Popshare
save_snap(
  pip("AGO", year = 2000, popshare = 0.2, lkup = lkup),
  "snap_pip_ago_popshare"
)

message("\nDone. Snapshots saved to: ", snap_dir)
message("Review snapshot_manifest.txt to confirm the data version.")
```

#### A1.2 Write snapshot comparison tests

- **File to create**: `tests/testthat/test-snapshot-baseline.R`
- **Details**: For each snapshot, test that re-running the same call produces
  identical output (using `expect_equal()` with tolerance for floating point).
  Tests should `skip_if` snapshots don't exist or data folder is unavailable.
- **Acceptance criteria**: `devtools::test(filter = "snapshot")` passes when
  snapshots and data are available, skips cleanly otherwise.

---

### A2. Dead Code Removal

**Objective**: Remove all identified dead code from the new pathway and shared files.
Done in small, independently committable sub-steps.

> **Rule**: Do NOT delete any `_old` function or file — those are frozen, not dead.
> Only remove code that is dead within the current codebase (unused, commented-out,
> or explicitly marked for removal).

#### A2.1 Remove debug statements

- **Files to modify**:
  - `R/fg_pip.R` — remove `print("here")` in `fg_remove_duplicates()`
  - `R/rg_pip_old.R` — remove commented `#browser()`
  - `R/compute_fgt_new.R` — remove commented `#print("ZP: no metadata...")`
- **Acceptance criteria**: Zero `print()`, `cat()`, `browser()` calls in R/ that
  are clearly debugging (not error reporting). Verify with:
  `grep -rn "print\|browser\|cat(" R/ | grep -v "#'" | grep -v "print_"` — only
  functional uses remain.

#### A2.2 Remove commented-out code blocks

Remove commented-out code that is clearly dead (not documentation). Each block
should be a separate commit for easy revert.

- **Files to modify** (new pathway + shared files only):
  - `R/compute_fgt_new.R` — remove commented-out `pov_from_DT2()` function body
  - `R/duckdb_func.R` — remove commented-out connection object creation block
  - `R/pip_new_lineups.R` — remove commented-out `fg_standardize_cache_id()` call
  - `R/pip_grp_new.R` — remove commented-out `pip_grp()` call block
  - `R/zzz.R` — remove all commented-out blocks (`assign("pip_raw"...)`,
    `memo_norm(...)`, `memoise` lines, parallel `detectCores()`)
  - `R/create_lkups.R` — remove commented-out `coerce_chr_to_fct()` calls,
    commented-out `md_ctrs` assignments, commented-out `pkg` list block

- **Acceptance criteria**: No commented-out R code blocks remain in modified files
  (roxygen comments and explanatory `#` comments are fine).

#### A2.3 Remove unused functions (new pathway only)

Remove functions confirmed to have zero call sites across the entire codebase.

- **Functions to remove from `R/compute_fgt_new.R`**:
  - `pov_from_DT()` — zero calls
  - `map_fgt()` — zero calls
  - `map_lt_to_dt()` — zero calls
  - `lt_to_dt()` — zero calls
  - `DT_fgt_by_rl()` — zero calls

- **Functions to remove from `R/utils-pipdata.R`**:
  - `transform_input()` — zero calls
  - `get_rl_rows_single()` — zero calls
  - `get_rl_rows()` — zero calls
  - `get_dt_dist_stats()` — zero calls
  - `get_lt_attr()` — zero calls

- **Functions to remove from `R/utils.R`**:
  - `coerce_chr_to_fct()` — all call sites are commented out
  - `convert_empty()` — zero calls
  - `collapse_rows()` — zero calls

- **Functions to remove from `R/zzz.R`**:
  - `memo_norm()` — all call sites commented out

- **NAMESPACE cleanup**: After removing functions, run `devtools::document()` to
  regenerate NAMESPACE. Verify removed functions are no longer exported.

- **Acceptance criteria**: Package builds without warnings. `R CMD check` passes
  (or has same warnings as before, not new ones).

#### A2.4 Remove `**** TO BE REMOVED ****` blocks (new pathway only)

These are the deprecated `group_by` handling blocks that force `fill_gaps=TRUE`
and do inline grouped aggregation. They exist in `pip_new_lineups()`.

- **File**: `R/pip_new_lineups.R`
  - Lines ~91–100: Remove the block that forces `fill_gaps <- TRUE` when
    `group_by != "none"` and shows deprecation message
  - Lines ~168–203: Remove the inline grouped aggregation block that runs when
    `group_by != "none"`

- **Risk**: The `group_by` parameter in `pip()` still has `"wb"` as an option.
  After removing these blocks, calling `pip(group_by="wb")` will no longer
  redirect to aggregation. Users should use `pip_agg()` instead. Verify that
  no plumber endpoint calls `pip(group_by="wb")`.

- **Pre-check**: Search `inst/plumber/` for any calls to `pip()` with `group_by`.

- **Acceptance criteria**: `pip_new_lineups()` no longer contains any
  `TO BE REMOVED` markers. `pip_agg()` still works for aggregation.

#### A2.5 Evaluate and annotate `TEMPORARY FIX` blocks

These blocks guard against `popshare` on aggregate distributions. They may still
be needed. Do NOT remove — instead, convert the `TEMPORARY FIX` comment to a
proper `# TODO(username): ...` with context.

- **Files**:
  - `R/fg_pip.R` — popshare TEMPORARY FIX
  - `R/rg_pip.R` — popshare TEMPORARY FIX
  - `R/rg_pip_old.R` — popshare TEMPORARY FIX (frozen, but annotate anyway)
  - `R/utils.R` — popshare TEMPORARY FIX

- **Acceptance criteria**: No `TEMPORARY FIX` comments remain — all converted
  to `# TODO:` with explanation of why the guard is still needed.

#### A2.6 Evaluate and annotate `TEMP` blocks in `create_lkups.R`

These data-cleaning blocks in `create_lkups()` may still be necessary if upstream
data hasn't been fixed. Do NOT remove — convert to `# TODO:` with context.

- **File**: `R/create_lkups.R`
  - `TEMP cleaning` for `svy_lkup`
  - `TEMP cleaning` for `ref_lkup`
  - `TEMP START: add distribution type`
  - `TEMP START: fix ARG population`

- **Acceptance criteria**: No `TEMP START` / `TEMP cleaning` markers remain —
  all converted to descriptive `# TODO:` comments.

---

## Testing Strategy

- **A1 snapshots**: Regression tests comparing current output to saved baselines.
  Tolerance of `1e-10` for floating-point comparisons.
- **A2 dead code removal**: No new tests needed — the removal is validated by:
  1. `R CMD check` passes
  2. Snapshot tests (A1) still pass
  3. Package loads without errors
- **Each A2 sub-step** should be a separate commit so any breakage can be bisected.

## Commit Strategy

Suggested commits (one per sub-step):

```
chore(tests): add snapshot generation script and baseline tests (A1)
refactor(core): remove debug statements from pipeline (A2.1)
refactor(core): remove commented-out code blocks (A2.2)
refactor(core): remove unused functions from new pathway (A2.3)
refactor(core): remove deprecated group_by blocks from pip_new_lineups (A2.4)
docs(core): convert TEMPORARY FIX markers to TODO annotations (A2.5)
docs(core): convert TEMP markers in create_lkups to TODO annotations (A2.6)
```

## Documentation Checklist

- [ ] Remove roxygen documentation for deleted functions
- [ ] No README updates needed (no public API changes)
- [ ] Add inline comments explaining why TODO blocks are kept

## Risks & Mitigations

| Risk | Mitigation |
|---|---|
| Removing "unused" function that's actually called dynamically | Search for string-based calls (`do.call`, `get`, `match.fun`) before removing |
| `TO BE REMOVED` blocks are still hit by plumber endpoints | Search `inst/plumber/` for `group_by` usage before removing |
| Snapshot data becomes stale when data updates | Snapshots are tied to a specific data version — document which version |
| User doesn't have data available to generate snapshots | A2 can proceed independently; A1 waits for data |

## Out of Scope

- Old pathway cleanup (frozen, not dead)
- `lkup` object redesign
- DuckDB caching layer
- UI endpoint functions
- Plumber endpoint hardening
- Proper unit tests (that's Phase C)
- `TEMP` blocks in UI files (`get_aux_table.R`, `ui_country_profile.R`, `ui_poverty_indicators.R`)

## Future Phases (Reference)

- **Phase B**: Restructure — split `utils.R`, deduplicate `pip_new_lineups`/`pip_old_lineups`, simplify `compute_fgt_new.R`
- **Phase C**: Harden — input validation, proper unit tests, roxygen2 documentation
