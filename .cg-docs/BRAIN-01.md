# 🧠 Project Brain — Part 1

_Generated 2026-10-31_

## Duckdb / Reset_Cache / Load_Inter_Cache

_Keywords: `duckdb` · `reset_cache` · `load_inter_cache`_ · 7 entities

- **[DuckDB file locked after dbDisconnect — write connection fails with 'file in use'](.cg-docs/solutions/bugs/2026-04-16-duckdb-file-lock-shutdown.md)** · `solution` · _—_ · `2026-04-16`
  > `update_master_file` (called via `pip()`) fails with:
- **[load_inter_cache emits catalog warning when DuckDB tables do not exist](.cg-docs/solutions/bugs/2026-04-16-load-inter-cache-missing-tables.md)** · `solution` · _—_ · `2026-04-16`
  > When running `pip()` against a data vintage whose DuckDB cache file exists but has never been initialized, the follow…
- **[reset_cache fails when DuckDB cache tables do not exist yet](.cg-docs/solutions/bugs/2026-04-16-reset-cache-missing-tables.md)** · `solution` · _—_ · `2026-04-16`
  > Running `reset_cache(lkup = lkup)` throws:
- **[Sys.getenv\(\) empty-string auth bypass: "" != "" is FALSE](.cg-docs/solutions/bugs/2026-04-20-getenv-empty-string-auth-bypass.md)** · `solution` · _—_ · `2026-04-20`
  > `reset_cache()` and `delete_cache()` had a key-validation guard: When **both** `PIP_CACHE_LOCAL_KEY` and `PIP_CACHE_S…
- **[Always guard DuckDB table operations with dbExistsTable\(\)](.cg-docs/solutions/data-quality/2026-04-16-duckdb-dbexiststable-guard.md)** · `solution` · _—_ · `2026-04-16`
  > Three separate functions in `R/duckdb_func.R` independently threw the same Catalog Error on a fresh `.duckdb` file (o…
- **[Testing DuckDB functions with ephemeral in-process databases](.cg-docs/solutions/testing-patterns/2026-04-16-testing-duckdb-functions.md)** · `solution` · _—_ · `2026-04-16`
  > Functions that read from or write to a DuckDB file (e.g. cache helpers like `reset_cache`, `create_duckdb_file`) are …
- **[withr::local_envvar must set ALL env vars the implementation reads independently](.cg-docs/solutions/testing-patterns/2026-04-20-withr-local-envvar-both-keys-required.md)** · `solution` · _—_ · `2026-04-20`
  > After extracting `.check_cache_auth()` as an internal helper, the `delete_cache` "no-file" test was simplified to onl…

## Main.R / {Pipapi} / /Data

_Keywords: `main.r` · `{pipapi}` · `/data`_ · 4 entities

- **[PIP API Code-Data Versioning Model](.cg-docs/brainstorms/2026-08-28-pipapi-code-data-versioning-model.md)** · `brainstorm` · _decided_ · `2026-08-28`
  > The pipapi codebase and data folder structure are tightly coupled, but no formal contract tracks or enforces this cou…
- **[PIP API Multi-Version Routing Architecture](.cg-docs/brainstorms/2026-09-03-pipapi-multi-version-api-routing.md)** · `brainstorm` · _decided_ · `2026-09-03`
  > This brainstorm extends the [2026-08-28 code-data versioning model](2026-08-28-pipapi-code-data-versioning-model.md) …
- **[2026-09-23-its-meeting-code-data-versioning-overview](.cg-docs/brainstorms/2026-09-23-its-meeting-code-data-versioning-overview.md)** · `brainstorm` · _—_ · `—`
  > **Purpose.** Prepare a shared reference for tomorrow's meeting with ITS on the design of a code–data versioning featu…
- **[PIP API Code-Data Versioning: Post-ITS Revision \(pairing-keyed containers, central schema file, pipschema\)](.cg-docs/brainstorms/2026-10-05-pipapi-versioning-post-its-revision.md)** · `brainstorm` · _decided_ · `2026-10-05`
  > <!-- Valid status values: decided, in-progress, abandoned -->

## Create_Versioned_Lkups\(\) / Create_Lkups.R / Pairing.Yml

_Keywords: `create_versioned_lkups()` · `create_lkups.r` · `pairing.yml`_ · 3 entities

- **[Enable {pipapi} to Consume Pairing-Filtered Vintage Lists and Isolate Startup \(Workstream B\)](.cg-docs/plans/2026-10-31-pairing-filtered-startup-isolation.md)** · `plan` · _completed_ · `2026-10-31`
  > Configure `{pipapi}` startup so an API container loads strictly the vintage folders assigned to its `(schema, pipapi_…
- **[2026-10-31-pairing-filtered-startup-isolation-review](.cg-docs/reviews/2026-10-31-pairing-filtered-startup-isolation-review.md)** · `review` · _—_ · `2026-10-31`
  > **Review mode**: architecture **Files reviewed**: 4 (`R/create_lkups.R`, `tests/testthat/test-create_lkups.R`, `NAMES…
- **[Pairing-filtered vintage startup isolation and strict YAML version validation in {pipapi}](.cg-docs/solutions/data-quality/2026-10-31-pairing-filtered-startup-isolation.md)** · `solution` · _—_ · `2026-10-31`
  > In the multi-container code-data versioning architecture for `{pipapi}`, each container serves a frozen `(schema, pip…

## Data Versioning Objective / Phase / Schema

_Keywords: `data versioning
objective` · `phase` · `schema`_ · 3 entities

- **[Phase 1: Schema Contract](roadmap.json#phase-1-schema-contract)** · `feature` · _idea_ · `—`
  > Phase 1: Schema Contract
- **[Phase 2: Code Pinning + Single Schema](roadmap.json#phase-2-code-pinning-single-schema)** · `feature` · _idea_ · `—`
  > Phase 2: Code Pinning + Single Schema
- **[Phase 3: Historical Addressability](roadmap.json#phase-3-historical-addressability)** · `feature` · _idea_ · `—`
  > Phase 3: Historical Addressability
