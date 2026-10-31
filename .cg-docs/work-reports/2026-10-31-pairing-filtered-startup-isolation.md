# Execution Report: Enable {pipapi} to Consume Pairing-Filtered Vintage Lists and Isolate Startup (Workstream B)

- **Plan reference**: `.cg-docs/plans/2026-10-31-pairing-filtered-startup-isolation.md`
- **Active deviation policy**: `ask`
- **Execution start date**: 2026-10-31

## Completed Steps/Phases

- **Phase 1 (Core Implementation)**: Completed 2026-10-31
  - Step 1: Implemented pairing filter helpers (`escape_vintage_regex`, `build_vintage_pattern`, `read_pairing_vintages`) in `R/create_lkups.R`.
  - Step 2: Replaced `use_new_lineup_version()` date cutoff with explicit `schema` parameter configuration in `create_versioned_lkups()` and `create_lkups()`.
  - Step 3: Implemented per-vintage `tryCatch()` isolation with `cli::cli_warn()`, filtering failed vintages out of both `versions` and `versions_paths` and aborting only on total failure.
- **Phase 2 (Tests, Documentation, and Git Tag Strategy)**: Completed 2026-10-31
  - Step 4: Added 30 new unit tests to `tests/testthat/test-create_lkups.R` covering regex escaping, pattern building, YAML pairing reading, directory filtering, schema overrides, and `tryCatch` isolation/recovery (62/62 tests passing).
  - Step 5: Documented `main.R` pairing pattern, updated roxygen docs, regenerated `NAMESPACE` and man pages via `roxygen2::roxygenise()`, verified `test-roxygen-docs.R`, and tagged local git releases `v1.5.15` on `280af151` and `v1.3.22` on `089c3fc8`.

## Deviations

*(None)*

## Accepted Exceptions

*(None)*

## Evidence Table

| ID | Phase | Evidence Required | Command/Artifact | Status |
|----|-------|-------------------|------------------|--------|
| V1 | 1 | Pairing filter helpers, explicit schema loading override, and per-vintage tryCatch isolation implemented in `R/create_lkups.R` | `pkgload::load_all()` | passed |
| V2 | 2 | All unit tests for pairing filtering, schema override, and per-vintage tryCatch fault tolerance pass | `testthat::test_file('tests/testthat/test-create_lkups.R')` (62/62 passed) | passed |
| V3 | final | Lookup validation and roxygen documentation tests pass with no regressions | `testthat::test_file('tests/testthat/test-validate_lkup.R')` (9/9 passed); `testthat::test_file('tests/testthat/test-roxygen-docs.R')` (3/3 passed) | passed |

## Constraints Check

| ID | Phase | Constraint | Check | Status |
|----|-------|------------|-------|--------|
| C1 | 1 | Internal dual-lineup calculation pathways remain functional and receive `lkup$use_new_lineup_version` | `testthat::test_file('tests/testthat/test-validate_lkup.R')` | passed |
| C2 | final | No files in `pipschema` are modified | `git -C ../pipschema status --porcelain` (clean) | passed |
| C3 | final | `versions` and `names(versions_paths)` returned by `create_versioned_lkups()` remain strictly identical even when corrupt vintages are skipped | Asserted in `test-create_lkups.R` | passed |

## Remaining Uncertainty

- Git tags `v1.5.15` and `v1.3.22` are created locally on their target commits; push to remote repository `PIP-Technical-Team/pipapi` awaits user confirmation per plan iteration policy.

## Final Status

`completed`
