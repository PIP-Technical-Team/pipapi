---
date: 2026-10-31
title: "Enable {pipapi} to Consume Pairing-Filtered Vintage Lists and Isolate Startup (Workstream B)"
status: completed
completed-date: 2026-10-31
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-10-05-pipapi-versioning-post-its-revision.md"
language: "R"
estimated-effort: "medium"
deviation-policy: "ask"
artifact-schema-version: 1
phases: 2
completed-phases: [1, 2]
execution-report: ".cg-docs/work-reports/2026-10-31-pairing-filtered-startup-isolation.md"
tags: [versioning, pairing, startup, create_versioned_lkups, tryCatch, git-tags, pipschema]
---

# Plan: Enable {pipapi} to Consume Pairing-Filtered Vintage Lists and Isolate Startup (Workstream B)

## Objective

Configure `{pipapi}` startup so an API container loads strictly the vintage folders assigned to its `(schema, pipapi_version)` pairing from `pairing.yml`, replaces the May 2025 date heuristic (`use_new_lineup_version()`) for folder/schema loading decisions with explicit pairing-driven configuration, isolates per-vintage lookup construction (`tryCatch`) so a single corrupt or incompatible folder warns without aborting container startup, and defines the exact git release-tagging strategy for frozen `{pipapi}` SHAs.

## Context

In the decided code-data versioning architecture (`.cg-docs/architecture/code-data-versioning.md` and `.cg-docs/brainstorms/2026-10-05-pipapi-versioning-post-its-revision.md`), each deployed container corresponds to one frozen `(schema, pipapi_version)` pairing and mounts the shared flat `/Data` directory. Today:
1. `create_versioned_lkups(data_dir)` scans every subdirectory under `/Data` matching the general vintage regex, loading all vintages across all schemas into memory.
2. `create_lkups()` calls `use_new_lineup_version(versions)` (hard-coded date cutoff `> 2025-05-01` in `R/create_lkups.R:989-1001`) to decide whether to load new-lineup tables (`prod_refy_estimation.fst`, `lineup_years.fst`, `lineup_dist_stats.fst`) vs. old-lineup tables.
3. `create_versioned_lkups()` runs bare `mapply(create_lkups, ...)` (`R/create_lkups.R:19-25`), so a single unreadable or structurally incompatible vintage folder aborts the entire API process.
4. `pipschema/inst/pairing/pairing.yml` binds historical releases to three `{pipapi}` commits:
   - `f081d28fc9ed9fbc90660bcecf324e3164d77d19` (`1.0.0`, already tagged `v1.0.0`)
   - `089c3fc877e003259694fb9b6aef8626a317bb53` (`1.3.7.9006` dev version string, untagged)
   - `280af151d05902a550d5dea18cd4bf1a3d013239` (`1.5.15` on `origin/DEV`, untagged)

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | **Startup filtering**: Update `create_versioned_lkups()` and provide pairing-filter helpers so a container or `main.R` can pass an explicit vintage filter (an exact-match `vintage_pattern` regex or an explicit character vector of folder names derived from `pairing.yml`), loading strictly those folders without scanning or loading unrelated folders in `/Data`. | Workstream B Req 1 |
| R2 | **Remove date cutoff for loading**: Replace the `use_new_lineup_version()` date-heuristic check (`> 2025-05-01`) for folder/schema loading in `create_versioned_lkups()` / `create_lkups()` with explicit pairing/schema-driven configuration (e.g. `schema = c("new-lineup", "pre-lineup")` or explicit `use_new_lineup` control derived from pairing), while keeping internal dual-lineup compute pathways intact until single-schema cleanup. | Workstream B Req 2 |
| R3 | **Startup fault-tolerance**: Wrap per-vintage `create_lkups()` execution inside `create_versioned_lkups()` with `tryCatch()` so a single corrupt or incompatible vintage folder logs an explicit warning (`cli::cli_warn`) and is excluded from `versions` and `versions_paths` rather than aborting container startup; abort only if zero valid vintages load. | Workstream B Req 3 |
| R4 | **Git tag strategy**: Define and verify the release-tagging plan for `{pipapi}`: pin exact release tag `v1.5.15` on `280af151d05902a550d5dea18cd4bf1a3d013239`, verify `v1.0.0` on `f081d28fc9ed9fbc90660bcecf324e3164d77d19`, and specify the clean release tag strategy for `089c3fc877e003259694fb9b6aef8626a317bb53` (replacing development version string `1.3.7.9006` while respecting `pipschema::write_pairing()`'s frozen `pipapi_sha` + one-time `.900x` version-fill rules). | Workstream B Req 4 |

## Implementation Steps

## Phase 1: Pairing-Filtered Discovery, Schema-Driven Loading, and Per-Vintage Isolation

### 1. Add Pairing-to-Vintage Filter Helpers (`R/create_lkups.R`)
- **Requirements**: R1
- **Files**: `R/create_lkups.R`, `NAMESPACE`, `man/`
- **Details**:
  - Add helper functions in `R/create_lkups.R` so `main.R` (and direct callers) can derive folder filters from `pairing.yml` without adding a runtime dependency on `{pipschema}` (`yaml` is already in `Imports`):
    - `escape_vintage_regex(x)`: escapes regex metacharacters (`([.|()\\^{}+$*?]|\\[|\\])`) in folder names.
    - `build_vintage_pattern(folders)`: validates non-empty character vector of folder names and builds an anchored alternation regex `paste0("^(", paste(escape_vintage_regex(unique(folders)), collapse = "|"), ")$")`.
    - `read_pairing_vintages(pairing_path, schema = NULL, pipapi_version = NULL, pipapi_sha = NULL)`: reads `pairing.yml` via `yaml::read_yaml()`, validates `pairing_file_version == 1`, filters `bindings` matching the container's `schema` and `pipapi_version` (or `pipapi_sha`), and returns the sorted unique character vector of bound `folders` (plus resolved `schema`).
  - Extend `create_versioned_lkups()` and `extract_data_dirs()` so callers can either pass `vintage_pattern = build_vintage_pattern(folders)` or pass `vintage_folders = <character vector>` (or multi-element folder vector), restricting directory discovery strictly to the specified folders and avoiding full-directory loading.
- **Test Scenarios**:
  - Happy path: `read_pairing_vintages()` on `pairing.yml` for `(schema = "new-lineup", pipapi_version = "1.5.15")` returns the 6 expected `PROD` folders; for `(schema = "pre-lineup", pipapi_sha = "089c3fc877e003259694fb9b6aef8626a317bb53")` returns the 10 bound folders across the 5 releases.
  - Edge case: folder names containing regex special characters are escaped accurately by `build_vintage_pattern()` and match only exact folder names in `id_valid_dirs()`.
  - Error path: missing `pairing_path`, unsupported `pairing_file_version`, or a `(schema, pipapi_version)` combination with zero matching bindings raises a structured `cli::cli_abort()` error.
- **Tests**: `tests/testthat/test-create_lkups.R`
- **Acceptance criteria**: `extract_data_dirs()` and `create_versioned_lkups()` select strictly the folders bound to a pairing when given the pairing filter, ignoring other valid-looking vintage folders present in `data_dir`.

### 2. Replace `use_new_lineup_version()` Date Cutoff in Lookup Loading (`R/create_lkups.R`)
- **Requirements**: R2
- **Files**: `R/create_lkups.R`, `man/create_versioned_lkups.Rd`, `man/create_lkups.Rd`, `man/use_new_lineup_version.Rd`
- **Details**:
  - Add explicit schema/lineup configuration to `create_versioned_lkups(data_dir, vintage_pattern = NULL, schema = NULL, ...)` and `create_lkups(data_dir, versions, schema = NULL)` where `schema` accepts `"new-lineup"` or `"pre-lineup"` (or logical/NULL fallback for backward compatibility in legacy standalone calls).
  - When `schema` is provided (the standard path for pairing-configured containers), set `lkup$use_new_lineup_version` directly from `schema == "new-lineup"` (or `"pre-lineup"` -> `FALSE`) without calling the `> 2025-05-01` date heuristic `use_new_lineup_version(versions)`.
  - Verify that a folder whose name prefix is before `2025-05-01` (or after `2025-05-01`) follows the explicit `schema` parameter rather than the folder-name date when `schema` is supplied.
  - Mark `use_new_lineup_version()` as superseded/deprecated for startup loading in roxygen docs while preserving `lkup$use_new_lineup_version` in the returned lookup list so downstream dual-lineup calculation code (`pip.R`, `pip_agg.R`, `fg_pip.R`, `pip_new_lineups.R`, `utils-stats.R`, `validate_lkup.R`) continues to work without modification (per Out of Scope).
- **Test Scenarios**:
  - Happy path: `create_lkups()` / `create_versioned_lkups()` with `schema = "pre-lineup"` sets `use_new_lineup_version = FALSE` regardless of folder date prefix; `schema = "new-lineup"` sets `use_new_lineup_version = TRUE` regardless of folder date prefix.
  - Edge case: calling `create_versioned_lkups()` with `schema = NULL` preserves backward compatibility for existing unit/integration tests (`helper-lkup.R`).
  - Error path: passing an unknown `schema` identifier (e.g., `"unknown-schema"`) aborts early via `cli::cli_abort()`.
- **Tests**: `tests/testthat/test-create_lkups.R`
- **Acceptance criteria**: Folder loading in `create_versioned_lkups()` and `create_lkups()` is driven by explicit `schema` configuration rather than the `2025-05-01` date cutoff, and all downstream `validate_lkup()` checks pass.

### 3. Implement Per-Vintage `tryCatch` Fault Tolerance in `create_versioned_lkups()` (`R/create_lkups.R`)
- **Requirements**: R3
- **Files**: `R/create_lkups.R`
- **Details**:
  - Replace the bare `mapply(create_lkups, ...)` call in `create_versioned_lkups()` (`R/create_lkups.R:19-25`) with a per-vintage iteration wrapped in `tryCatch()`.
  - On error for an individual vintage `v`:
    - Emit a structured warning via `cli::cli_warn()` naming the failed vintage folder `v`, its path, and `conditionMessage(err)`.
    - Return `NULL` for that vintage and drop failed entries from both `versions` and `versions_paths` so `names(versions_paths)` and `versions` remain strictly identical (required by `.cache_v2_prepare_lkups()` in `R/start_api.R:21-26`).
    - Recompute `latest_release <- versions[1]` from the surviving sorted versions.
  - If all candidate vintages fail (or zero valid directories match), abort clearly via `cli::cli_abort()` so the container does not start with an empty `lkups` object.
- **Test Scenarios**:
  - Happy path: all matched vintage folders are valid -> all load into `versions` and `versions_paths`, `latest_release` is `versions[1]`.
  - Edge case (partial failure): 2 vintage folders match, 1 is corrupt (e.g. missing `_aux/missing_data.fst` or corrupt fst) and 1 is valid -> `create_versioned_lkups()` emits a warning naming the corrupt folder, succeeds, and returns only the valid vintage in `versions`, `versions_paths`, and `latest_release`.
  - Error path (total failure): all matched vintage folders are corrupt or zero folders match -> `create_versioned_lkups()` aborts with a clear error message after warning on each failed folder.
- **Tests**: `tests/testthat/test-create_lkups.R`
- **Acceptance criteria**: A single broken vintage folder logs an explicit warning and is skipped without crashing `create_versioned_lkups()`, while `identical(lkups$versions, names(lkups$versions_paths))` holds.

## Phase 2: Tests, Documentation, and Git Release-Tagging Strategy

### 4. Add Unit Tests for Pairing Filtering, Schema Override, and Startup Isolation (`tests/testthat/test-create_lkups.R`)
- **Requirements**: R1, R2, R3
- **Files**: `tests/testthat/test-create_lkups.R`
- **Details**:
  - Add synthetic temporary-directory (`withr::local_tempdir()`) unit tests in `tests/testthat/test-create_lkups.R` covering:
    1. `escape_vintage_regex()`, `build_vintage_pattern()`, and `read_pairing_vintages()` using a synthetic `pairing.yml` fixture matching the `pairing_file_version: 1` contract.
    2. `extract_data_dirs()` filtering a multi-vintage temp `/Data` tree down to only the folders belonging to a single pairing.
    3. Explicit `schema = "pre-lineup"` vs `schema = "new-lineup"` overriding the `2025-05-01` date heuristic via `testthat::local_mocked_bindings()`.
    4. Per-vintage `tryCatch()` isolation in `create_versioned_lkups()` when one vintage throws an error and another succeeds, verifying warning emission, exclusion of the failed vintage, and preservation of `latest_release`.
    5. Total-failure abort when every candidate vintage fails to load.
- **Test Scenarios**:
  - Happy path, partial-failure warning path, and total-failure abort path all execute deterministically without requiring `PIPAPI_DATA_ROOT_FOLDER_LOCAL`.
- **Tests**: `Rscript -e "testthat::test_file('tests/testthat/test-create_lkups.R')"`
- **Acceptance criteria**: All unit tests in `test-create_lkups.R` and `test-validate_lkup.R` pass with 0 failures.

### 5. Document `main.R` Pairing Pattern and Git Release-Tagging Strategy (`R/create_lkups.R`, `.cg-docs/architecture/code-data-versioning.md`)
- **Requirements**: R1, R2, R3, R4
- **Files**: `R/create_lkups.R`, `man/`, `NAMESPACE`
- **Details**:
  - Document the canonical `main.R` startup pattern in `@examples` / roxygen docs of `create_versioned_lkups()` and `read_pairing_vintages()`:
    ```r
    vintages <- read_pairing_vintages(
      pairing_path   = "/Data/pairing.yml",
      schema         = "new-lineup",
      pipapi_version = "1.5.15"
    )
    lkups <- create_versioned_lkups(
      data_dir        = "/Data",
      vintage_pattern = build_vintage_pattern(vintages),
      schema          = "new-lineup"
    )
    start_api(api_version = "v1", port = 8080, lkups = lkups)
    ```
  - Specify the exact **Git Release-Tagging Plan** for `{pipapi}` (Requirement R4) so all three SHAs in `pipschema/inst/pairing/pairing.yml` have clean release tags on GitHub `PIP-Technical-Team/pipapi`:
    1. **Pin `v1.0.0` (`pre-lineup`, release `20220609`)**:
       - Target SHA: `f081d28fc9ed9fbc90660bcecf324e3164d77d19` (`DESCRIPTION` `Version: 1.0.0`).
       - Status: Tag `v1.0.0` already exists and points to `f081d28f`. Verify with `git rev-parse v1.0.0^{commit}`.
    2. **Pin `v1.5.15` (`new-lineup`, releases `20250930`, `20260324`, `20260922`)**:
       - Target SHA: `280af151d05902a550d5dea18cd4bf1a3d013239` (current tip of `origin/DEV`, `DESCRIPTION` `Version: 1.5.15`).
       - Action: Cut annotated release tag directly on that exact SHA:
         `git tag -a v1.5.15 280af151d05902a550d5dea18cd4bf1a3d013239 -m "Release v1.5.15 (new-lineup pairing pin)"` and `git push origin v1.5.15`.
       - Impact on `pairing.yml`: `pipapi_version: 1.5.15` and `pipapi_sha: 280af151d05902a550d5dea18cd4bf1a3d013239` already match; no `pairing.yml` change needed.
    3. **Replace `1.3.7.9006` dev version on SHA `089c3fc877e003259694fb9b6aef8626a317bb53` (`pre-lineup`, releases `20220909`, `20230328`, `20230919`, `20240326`, `20240627`)**:
       - Constraint check: In `{pipschema}`, `write_pairing()` freezes `pipapi_sha` (`089c3fc877e003259694fb9b6aef8626a317bb53`) permanently (`R/pairing-write.R:54`) and permits a one-time fill of `pipapi_version` when the old version matches `\\.900[0-9]*$` (`1.3.7.9006`). Because `pipapi_sha` cannot be mutated without violating the append-only contract (and because creating a new commit to edit `DESCRIPTION` would change the SHA), the release tag **must** point directly to commit `089c3fc877e003259694fb9b6aef8626a317bb53`.
       - Tag version selection: Tags `v1.3.0` through `v1.3.7`, `1.3.8`, `v1.3.9`, `v1.3.15`, and `1.3.21` already exist in `{pipapi}` (`1.3.8` is at `e480cbb5`, 26 commits after `089c3fc8`). To avoid colliding with existing `1.3.x` tags while providing a clean release version string, tag `089c3fc877e003259694fb9b6aef8626a317bb53` as **`v1.3.7-p1`** (or the next unused patch number in the `1.3.x` series, e.g., **`v1.3.22`**, if strict 3-component `X.Y.Z` is preferred by ACR/Docker image tags):
         `git tag -a v1.3.22 089c3fc877e003259694fb9b6aef8626a317bb53 -m "Release v1.3.22 (frozen pre-lineup pairing pin for 089c3fc8, replacing 1.3.7.9006)"`
       - Follow-up in coverage/pairing workflow (outside this plan's file mutations): updating `source_version` in coverage for `089c3fc877e003259694fb9b6aef8626a317bb53` from `1.3.7.9006` to `1.3.22` will trigger `pipschema::write_pairing()`'s one-time `.900x` version fill while keeping `pipapi_sha: 089c3fc877e003259694fb9b6aef8626a317bb53` intact.
  - Regenerate roxygen documentation (`roxygen2::roxygenise()`).
- **Test Scenarios**:
  - Happy path: `roxygen2::roxygenise()` succeeds and `test-roxygen-docs.R` / `test-lint.R` pass.
- **Tests**: `Rscript -e "testthat::test_file('tests/testthat/test-roxygen-docs.R')"`
- **Acceptance criteria**: Roxygen docs and `NAMESPACE` are up to date, and the git release-tagging commands and SHA mappings are documented and ready for execution.

## Testing Strategy

- **Unit tests (`tests/testthat/test-create_lkups.R`)**: All new functionality (pairing YAML parsing, regex escaping and pattern building, directory filtering, explicit `schema` override of `use_new_lineup_version()`, and `tryCatch` per-vintage fault isolation) is tested using synthetic temporary directory trees (`withr::local_tempdir()`) and `testthat::local_mocked_bindings()`. No external `/Data` mount is required.
- **Lookup validation tests (`tests/testthat/test-validate_lkup.R`)**: Verify `lkup` structure invariants remain satisfied.
- **Documentation tests (`tests/testthat/test-roxygen-docs.R`)**: Verify exported functions have complete roxygen documentation and `NAMESPACE` entries.

## Documentation Checklist

- [ ] `@param`, `@return`, and `@examples` roxygen tags added for new/updated functions in `R/create_lkups.R`.
- [ ] `use_new_lineup_version()` roxygen note updated to state that startup loading now uses explicit pairing `schema` configuration.
- [ ] `NAMESPACE` and `man/*.Rd` regenerated via `roxygen2::roxygenise()`.

## Risks & Mitigations

| Risk | Severity | Likelihood | Mitigation |
|------|----------|------------|------------|
| Regex metacharacters in folder names cause partial or unintended matches in `id_valid_dirs()` | Medium | Low | `build_vintage_pattern()` escapes all regex metacharacters and anchors with `^(` and `)$`. |
| Skipping a failed vintage in `create_versioned_lkups()` leaves `versions` and `names(versions_paths)` out of sync, breaking `.cache_v2_prepare_lkups()` | High | Medium | Filter both `versions` and `versions_paths` from the non-NULL surviving results and assert `identical(versions, names(versions_paths))` before returning. |
| All vintages in a container fail to load, leaving an empty `lkups` list that only fails on the first HTTP request | High | Low | Abort immediately in `create_versioned_lkups()` via `cli::cli_abort()` if zero vintages succeed so the container fails fast at startup. |
| Modifying `DESCRIPTION` on SHA `089c3fc877e003259694fb9b6aef8626a317bb53` to remove `.9006` would alter the commit SHA and violate `pipschema::write_pairing()`'s frozen `pipapi_sha` check | High | High | Tag commit `089c3fc877e003259694fb9b6aef8626a317bb53` directly without creating a new commit, preserving the exact 40-char SHA recorded in `pairing.yml` and utilizing `pipschema`'s built-in one-time `.900x` version fill. |

## Out of Scope

- Re-running schema classification or modifying any files in `{pipschema}`.
- Azure DevOps pipelines, Podman scripts, Dockerfiles, or nginx configuration (Workstream C).
- Removing the internal dual-lineup calculation code (`pip_old_lineups.R`, `pip_new_lineups.R`, `fg_pip_old.R`, `rg_pip_old.R`, `compute_fgt_old.R`) — deferred until single-schema releases are fully deployed.

## Completion Contract

### Outcome

`{pipapi}` can filter startup vintage folders strictly to a pairing's folder list from `pairing.yml`, determine lineup schema behavior from explicit pairing configuration rather than the `2025-05-01` date cutoff, isolate per-vintage lookup creation with `tryCatch` warnings so one bad folder does not crash startup, and follow a verified git release-tagging plan for `v1.0.0`, `v1.5.15`, and SHA `089c3fc877e003259694fb9b6aef8626a317bb53`.

### Verification Surface

| ID | Phase | Evidence Required | Command/Artifact | Required |
|----|-------|-------------------|------------------|----------|
| V1 | 1 | Pairing filter helpers (`read_pairing_vintages`, `build_vintage_pattern`), explicit `schema` loading override, and per-vintage `tryCatch` isolation implemented in `R/create_lkups.R` | `Rscript -e "pkgload::load_all()"` in `pipapi` | yes |
| V2 | 2 | All unit tests for pairing filtering, schema override, and per-vintage `tryCatch` fault tolerance pass | `Rscript -e "testthat::test_file('tests/testthat/test-create_lkups.R')"` | yes |
| V3 | final | Lookup validation and roxygen documentation tests pass with no regressions | `Rscript -e "testthat::test_file('tests/testthat/test-validate_lkup.R'); testthat::test_file('tests/testthat/test-roxygen-docs.R')"` | yes |

### Constraints

| ID | Phase | Constraint | Check |
|----|-------|------------|-------|
| C1 | 1 | Internal dual-lineup calculation pathways (`pip.R`, `pip_agg.R`, `fg_pip.R`, `pip_new_lineups.R`, `validate_lkup.R`) must remain functional and keep receiving `lkup$use_new_lineup_version` | `Rscript -e "testthat::test_file('tests/testthat/test-validate_lkup.R')"` |
| C2 | final | No files in `pipschema` are modified | `git -C ../pipschema status --porcelain` |
| C3 | final | `versions` and `names(versions_paths)` returned by `create_versioned_lkups()` remain strictly identical even when corrupt vintages are skipped | Asserted in `test-create_lkups.R` |

### Boundaries

- Allowed: `R/create_lkups.R`, `tests/testthat/test-create_lkups.R`, `man/*.Rd`, `NAMESPACE`, `.cg-docs/plans/`, `.cg-docs/work-reports/`.
- Out of scope: Modifying `{pipschema}` files, Azure DevOps / Podman / Dockerfile / nginx configs (Workstream C), or deleting internal dual-lineup calculation code (`pip_old_lineups.R`, `pip_new_lineups.R`, etc.).

### Iteration Policy

1. Follow `deviation-policy: ask` — pause and request user approval before any deviation from the planned scope or files.
2. Run `test-create_lkups.R` after each change to `R/create_lkups.R` to verify zero regressions in existing pattern and sorting tests.
3. Do not push git tags to remote without explicit user confirmation during implementation.

### Blocked-Stop Conditions

- Required verification cannot be run through the safe runner.
- Any required evidence item fails after allowed recovery attempts.
- A required deviation is discovered under policy `ask` and user approval is unavailable.
- A protected boundary (such as modifying `pipschema` or deleting dual-lineup compute pathways) must be crossed to continue.
- Completion would require marking evidence as passed from static inspection alone rather than an executed check.
