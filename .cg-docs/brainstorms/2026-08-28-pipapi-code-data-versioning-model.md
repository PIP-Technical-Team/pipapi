---
date: 2026-08-28
title: "PIP API Code-Data Versioning Model"
status: decided
scope: "Deep"
artifact-schema-version: 1
chosen-approach: "Phased implementation: Schema contracts (Phase 1) → Code pinning (Phase 2) → Historical addressability (Phase 3)"
tags: [versioning, deployment, reproducibility, data, schema, api]
---

# PIP API Code-Data Versioning Model

## Context

The pipapi codebase and data folder structure are tightly coupled, but no formal contract tracks or enforces this coupling. This creates three distinct problems:

1. **Startup fragility**: One structurally incompatible vintage folder can crash `create_versioned_lkups()` and kill the entire API process
2. **Silent corruption**: Subtler schema mismatches produce NAs or incorrect results with no error signals
3. **No historical reproducibility**: "Version X" currently means old *data* under *current code*, not a stable code–data pairing

The current system uses one implicit schema indicator: `use_new_lineup_version()` checks if a vintage's release date is after May 1, 2025 to decide between two fundamentally different computational approaches (old vs new lineup). This creates ~230 lines of conditional logic in `create_lkups.R` and parallel code paths throughout the system.

## Requirements

### Functional

- **Schema contract**: Declare what folder structure a pipapi release expects; declare what schema a vintage folder provides
- **Compatibility checking**: Filter vintages at startup to load only schema-compatible folders
- **Per-vintage isolation**: One bad vintage folder must not crash the entire startup
- **Code pinning**: Deployment must specify a pinned pipapi release (tag/SHA), not a floating `@DEV` branch
- **Reproducibility**: "Query API version X" must mean querying a stable code–data pairing from release X
- **Clean code paths**: Each pipapi release should support ONE schema family, eliminating conditional schema logic (`use_new_lineup_version()` and parallel old/new lineup code paths should be removable)

### Non-Functional

- **Backward compatibility**: Multi-vintage API contract (`?version=` param) must be preserved
- **Operational simplicity**: Default deployment remains one live container serving multiple vintages (all from the same schema family)
- **Data pipeline unchanged**: Existing data pipeline (ITSES-POVERTYSCORE-DATA) continues dropping folders into shared `/Data`; no re-sharding required
- **Incremental deployment**: Changes can be phased; don't require big-bang rewrite

## Problem Analysis

### What is a "vintage folder"?

A vintage folder (e.g., `20250601_2025_01_02_PROD`) encodes:
- **Release date**: `20250601` (June 1, 2025)
- **PPP year**: `2025`
- **Version identifiers**: `01_02`
- **Environment tag**: `PROD` (vs `INT` or `TEST`)

Each vintage is a **data snapshot + implicit schema** (currently inferred from release date via the May 1, 2025 cutoff).

### Current schema detection mechanism

`use_new_lineup_version()` (R/create_lkups.R:989-1001):
```r
use_new_lineup_version <- function(x) {
  date_str <- substr(x, 1, 8)
  date_val <- as.Date(date_str, format = "%Y%m%d")
  threshold <- as.Date("2025-05-01")
  date_val > threshold
}
```

This boolean controls:
- Which lookup files to load (`ref_lkup` vs `refy_lkup`)
- Which data folders to access (`survey_data/` vs `lineup_data/`)
- Which computation functions to call (`pip_old_lineups.R` vs `pip_new_lineups.R`)
- Which FGT algorithm to use (loop-based vs cumsum-based)

### Schema differences between old (lineup-v1) and new (lineup-v2)

| Dimension | Old (lineup-v1) | New (lineup-v2) |
|-----------|-----------------|-----------------|
| **Algorithm** | Loop-based FGT (O(N*M)) | Cumsum-based with `findInterval` (O(N + M log N)) |
| **Input files** | Raw survey `.fst` files (welfare, weight) | Pre-processed `.fst` with cumulative columns (`cw`, `cwy`, `cwy2`, `cwylog`) |
| **Metadata** | `ref_lkup` (survey-level interpolation) | `refy_lkup` (lineup-level yearly estimates) |
| **Missing data** | Handled via duplicates | Synthetic CMD rows in `refy_lkup` |
| **Distribution stats** | `dist_stats` on `cache_id` | `lineup_dist_stats` on `country_code + year` |
| **Hardcoded paths** | ~14 files per vintage | ~18 files per vintage (adds 4: `refy`, `lineup_years`, `lineup_dist_stats`, CMD data) |

These are **fundamentally different computational approaches**, not just different file names. Eliminating conditional schema logic would delete ~800 lines of old-pathway code (73% reduction), but the new pathway's inherent complexity (~330 lines) remains.

### Historical layout change pattern

- **May 2025 (v1.3.24)**: One major breaking change — new lineup introduction
- **Aug–Sep 2025**: Incremental changes (file format switches, auxiliary table updates) within lineup-v2
- **2025–2026**: Bug fixes and test updates, no new schema breaks

**Pattern**: ~1 major schema break per year; when it happens, it's high-impact (4+ new files, 230+ lines of conditional code).

### Current deployment architecture

**Code side (this repo)**:
- R package with plumber API
- `create_versioned_lkups(data_dir)` discovers all vintage folders, loads all into one global `lkups` object
- Package version in DESCRIPTION (~1.5.6) is semantic versioning for releases, not tied to deployments

**Deployment side (Azure DevOps `ITSES-POVERTYSCOREAPI`)**:
- Repo branches: `DEV` / `QA` / `PROD` (environment promotion, not version pinning)
- `PIP_CustomImage/Dockerfile`: installs `remotes::install_github('PIP-Technical-Team/pipapi@DEV')` — **floating branch reference**
- `R/main.R`: calls `create_versioned_lkups("/Data")`, then `start_api("v1", 8080)`
- One container live; `/Data` mount = shared storage with accumulated vintages from ITSES-POVERTYSCORE-DATA pipeline

**Key vulnerability**: Code is unpinned, schema is implicit, no validation happens before loading all vintages.

### Consumers and use cases

- **Internal team debugging**: Need to reproduce results from a previous release to diagnose discrepancies
- **External API consumers**: Need reproducibility guarantees for published numbers
- **Not required**: Bit-for-bit reproducibility of intermediate computations (only final API responses)

Multi-vintage API contract (`?version=` param) is institutional — cannot break without high coordination cost.

## Approaches Considered

### Approach 1: Schema Contract Only (Reliability-First, Phase 1)

**Summary**: Add formal contracts between code and data without changing deployment.

**Components**:
- **Folder manifest**: Each vintage contains `_manifest.yaml` declaring its schema:
  ```yaml
  release_date: 2025-06-01
  schema_id: lineup-v2
  ppp_year: 2025
  required_files:
    - estimations/prod_refy_estimation.fst
    - estimations/lineup_years.fst
    - lineup_data/
    # ... (full list)
  ```
- **Code schema declaration**: pipapi DESCRIPTION or new `R/schemas.R` declares supported schemas:
  ```yaml
  supported_schemas: [lineup-v2]
  ```
- **Compatibility checking**: `create_versioned_lkups()` filters vintages by schema before loading:
  ```r
  valid_dirs <- data_dirs[vapply(data_dirs, check_schema_compat, logical(1))]
  ```
- **Per-vintage isolation**: Wrap each `create_lkups()` call in `tryCatch()` so one bad vintage doesn't crash the process
- **Logging**: Incompatible or failed vintages logged with reason (schema mismatch, missing files, load error)

**What it does NOT include**:
- No code pinning (still installs `@DEV`)
- No deployment changes to `main.R` or Dockerfile
- No historical pairings addressable

**Pros**:
- Solves startup fragility immediately
- Solves silent corruption (incompatible vintages rejected upfront)
- Small incremental change — mostly additive to existing code
- Data pipeline unchanged (manifests added to new vintages going forward)
- Enables clean schema-per-release model (future releases can drop old lineup code)
- Quick win: ~1-2 weeks to ship

**Cons**:
- Does NOT solve reproducibility (old code–data pairings not addressable)
- Manifest backfill: must add `_manifest.yaml` to existing vintages retroactively (or continue using date heuristic for pre-manifest vintages)
- Still running one process with floating code (pin happens in Phase 2)

**Effort**: **Small** — 3-5 days implementation + 2-3 days testing

**Recommended?** **Yes, as Phase 1** — minimum viable safety net, deployable without disrupting production.

---

### Approach 2: Code Pinning + Single Schema Per Deployment (Reliability + Reproducibility Foundation, Phase 2)

**Summary**: Approach 1 + pin the code and enforce ONE supported schema per deployment.

**What it adds to Approach 1**:
- **Pin pipapi installation** in Dockerfile:
  ```dockerfile
  RUN R -e "remotes::install_github('PIP-Technical-Team/pipapi@v1.6.0')"
  ```
  (Tag, not `@DEV`)
- **Single-schema enforcement**: Each pipapi release declares exactly ONE `supported_schema`. Example:
  - `pipapi v1.6.0` → `lineup-v2` only (all old lineup code deleted)
  - `pipapi v1.4.2` → supports both (transition release, keeps conditionals)
- **Pairing manifest**: A `pairings.yaml` (in Azure repo or this repo) records which pipapi version pairs with which schema:
  ```yaml
  pairings:
    - pipapi_version: v1.6.0
      schema_id: lineup-v2
      release_date: 2026-09-01
    - pipapi_version: v1.4.2
      schema_id: lineup-v1
      release_date: 2025-04-15
  ```
- **Deployment config update**: `main.R` reads `PIPAPI_VERSION` env var; Dockerfile uses that to pin the install
- **Schema-specific releases**: Starting with Phase 2, new pipapi releases ship with clean single-schema code (delete all `use_new_lineup_version()` conditionals and old lineup functions)

**What it does NOT include**:
- No multi-pairing addressability yet (still one live container serving one schema family)
- Historical pairings (e.g., "run v1.4.2 code on lineup-v1 vintages") require manual spin-up (Phase 3)

**Pros**:
- Solves all 3 problems for the **current live deployment** (reliability + current-state reproducibility)
- Code and data now explicitly coupled via manifest + pairings.yaml
- Audit trail: git tags + pairings.yaml = reproducibility record
- Foundation for historical pairings (Phase 3)
- Operational simplicity: still one container
- Enables clean codebase: future releases can be single-schema (73% code reduction)

**Cons**:
- Historical pairings not automatically addressable (must trigger Approach 3b/3c to query old pairings)
- Deployment must be updated (new container built/deployed) whenever a new pipapi release ships
- Requires coordination: code release tagged → deployment updated → pairings.yaml updated

**Effort**: **Medium** — Approach 1 (3-5 days) + pinning logic (2 days) + pairings manifest (1 day) + deployment testing (2-3 days) = **8-13 days total**

**Recommended?** **Yes, as Phase 2** — production-ready foundation for reproducibility, complete solution for current-state queries.

---

### Approach 3: Addressable Historical Pairings (Full Reproducibility, Phase 3)

**Summary**: Approach 2 + make historical code–data pairings queryable on demand.

**Problem to solve**: Users want to query "API version 1.4.2" (lineup-v1 era) while the live deployment runs v1.6.0 (lineup-v2). This requires running multiple pipapi versions.

**Three sub-options**:

#### 3a. Path-based versioning (multiple versions in one container)
- Live container runs latest pinned pairing
- Add `?api_version=v1.4.2` param to all endpoints
- Lazy-load old versions' lkups on first request (subprocess? pre-load at startup?)
- **Problem**: Running multiple pipapi package versions in one R process is architecturally fragile (namespace collisions, memory bloat, mixed dependencies)

**Not recommended** — high complexity, low reliability.

#### 3b. On-demand container spin-up
- Live container = latest pairing (v1.6.0 + lineup-v2)
- Historical pairings: DevOps pipeline deploys separate container for old version on demand (e.g., `pipapi-v1-4-2.azurewebsites.net`)
- Container runs until manually torn down
- **Pros**: Real deployment, addressable URL, reuses existing Azure pipeline tooling
- **Cons**: Manual trigger (not automatic), resource overhead while live, requires naming conventions to avoid collisions

**Recommended for external API reproducibility** — institutional use case (published numbers must be reproducible) justifies the resource cost.

#### 3c. Ephemeral Docker images (local or cloud)
- No live historical containers by default
- To reproduce: `docker run -v /Data:/Data pipapi:v1.4.2` locally or in temporary cloud instance
- **Pros**: Zero standing cost, simple, good for internal debugging
- **Cons**: Not a real public API, manual process, no automatic discoverability

**Recommended for internal team debugging** — low-cost, low-overhead solution for ad hoc reproduction.

**Effort**:
- **3a**: Large (15+ days) — high architectural risk, not recommended
- **3b**: Medium (10-15 days after Approach 2) — pipeline automation, naming conventions, runbook
- **3c**: Small (5 days after Approach 2) — Dockerfile tagging per release, local run documentation

**Recommended?** **Defer to Phase 3** — only implement after Approach 2 is stable and proven demand exists. Choose **3b for external consumers, 3c for internal team**.

---

## Comparison Summary

| Approach | Startup Fragility | Silent Corruption | Reproducibility | Code Simplification | Effort | Phase |
|----------|-------------------|-------------------|-----------------|---------------------|--------|-------|
| **1. Schema Contract** | ✅ Solved | ✅ Solved | ❌ Not addressed | 🟡 Enables (future) | Small (3-5d) | **Phase 1** |
| **2. Code Pinning + Single Schema** | ✅ Solved | ✅ Solved | ✅ Current state | ✅ Yes (73% reduction) | Medium (8-13d) | **Phase 2** |
| **3b. On-Demand Containers** | ✅ Solved | ✅ Solved | ✅ Full historical | ✅ Yes | Medium (10-15d) | **Phase 3** |
| **3c. Ephemeral Docker** | ✅ Solved | ✅ Solved | ✅ Internal only | ✅ Yes | Small (5d) | **Phase 3 (lite)** |

## Decision

**Chosen approach**: Phased implementation in sequence:

1. **Phase 1 (immediate)**: Implement Approach 1 (Schema Contract)
   - Add `_manifest.yaml` schema declaration to vintage folders
   - Add schema compatibility checking to `create_versioned_lkups()`
   - Add per-vintage `tryCatch()` isolation
   - Backfill manifests for existing vintages (or keep date heuristic as fallback)
   - Deploy to production to validate the contract model

2. **Phase 2 (follow-up, same quarter)**: Add Approach 2 (Code Pinning)
   - Pin pipapi installation in Dockerfile to tagged releases
   - Create `pairings.yaml` mapping releases to schemas
   - Update deployment process to read `PIPAPI_VERSION` env var
   - **Critical**: Starting with Phase 2, new pipapi releases declare ONE schema and delete old schema code paths

3. **Phase 3 (as needed)**: Implement Approach 3b and/or 3c based on demand
   - **3c (internal debugging)**: Document ephemeral Docker run process
   - **3b (external reproducibility)**: Automate on-demand container deployment via Azure pipeline if external consumers require it

**Rationale**:
- Phase 1 is a quick win that solves 2 of 3 problems immediately without deployment changes
- Phase 2 completes the foundation and enables clean single-schema releases going forward
- Phase 3 deferred until proven demand (don't build infrastructure for hypothetical use cases)
- Phasing allows validation at each step before committing to the next layer

## Open Questions and Risks

### Open Questions

1. **Manifest backfill strategy**: For existing vintages without `_manifest.yaml`, should we:
   - Retroactively add manifests (requires data pipeline coordination)
   - Keep date heuristic as fallback for pre-manifest vintages (technical debt)
   - Require manifests only going forward and refuse to load vintages without them (breaks existing deployments)

2. **Schema version granularity**: Should schema IDs be:
   - Coarse-grained (`lineup-v1`, `lineup-v2`) — matches major computational changes
   - Fine-grained (`lineup-v2.1`, `lineup-v2.2`) — tracks every file format or column change
   - Recommended: Start coarse-grained; only introduce fine-grained versions if backward-incompatible changes happen within a schema family

3. **Schema change triggers**: What forces a new schema version?
   - Adding a new required file to vintages?
   - Renaming an existing file?
   - Adding/removing columns in a table?
   - Changing column types or data formats?
   - Recommended: Document a schema versioning policy as part of Phase 1 implementation

4. **Transition release strategy**: When introducing schema v3, should there be:
   - One release supporting both v2 and v3 (transition release, like current v1.4.2)
   - Clean cutover (last v2-only release, then first v3-only release)
   - Recommended: Plan transition releases to avoid "no pipapi version can read this vintage" gaps

5. **Cache interaction**: Does `cache_data_id` (computed via `rlang::hash()` over lookup tables) stay consistent across schema versions?
   - If schema changes, does cache invalidation happen automatically?
   - Could cached results from lineup-v1 leak into lineup-v2 queries if vintage dates overlap?
   - Recommended: Audit cache behavior in Phase 1 testing

6. **In-place data corrections**: If a historical vintage's data content is corrected (not schema, just numbers), does reproducibility require:
   - Content-addressed vintages (hash-based identifiers)?
   - Immutable vintages (corrections create new dated folders)?
   - Version-within-vintage (e.g., `20250601_2025_01_02_PROD_rev2`)?
   - Recommended: Define correction policy before Phase 2 (affects whether release date alone is sufficient identifier)

7. **Who triggers Phase 3 deployments**: For on-demand historical containers (3b), who is authorized to:
   - Request deployment of an old version?
   - Approve the resource allocation?
   - Decide when to tear down?
   - Recommended: Document approval workflow as part of Phase 3 planning

8. **Retirement policy**: When (if ever) are old vintages or old pipapi versions decommissioned?
   - Keep all history forever (unbounded storage/resource growth)?
   - Rolling window (e.g., current + previous 2 years)?
   - Milestone-based (mark certain releases as "long-term support")?
   - Recommended: Define retention policy in collaboration with institutional stakeholders

### Risks

1. **Manifest creation bottleneck**: Adding `_manifest.yaml` to hundreds of existing vintages could delay Phase 1 deployment
   - **Mitigation**: Implement date-based fallback; manifests required only for new vintages initially

2. **Schema drift**: Code and manifest definitions could fall out of sync if not validated automatically
   - **Mitigation**: Add automated tests in Phase 1 that validate manifest against actual files present

3. **Deployment coordination complexity**: Phase 2 requires tighter coupling between code releases and deployment updates
   - **Mitigation**: Document clear release → deployment → pairings.yaml update workflow; consider CI automation

4. **Unknown vintage incompatibilities**: Existing vintages might fail to load even with correct schema due to undocumented quirks
   - **Mitigation**: Phase 1 testing on full production `/Data` before enabling in live deployment

5. **Phase 3 demand uncertainty**: Building infrastructure for historical pairings without proven consumer demand wastes resources
   - **Mitigation**: Defer Phase 3 until actual reproduction requests arrive; document ephemeral Docker process (3c) as low-cost interim solution

6. **Code simplification regression**: Future developers might re-introduce conditional schema logic instead of maintaining clean single-schema releases
   - **Mitigation**: Document the "one release, one schema" principle explicitly in CONTRIBUTING.md; add linter rule to flag `use_new_lineup_version()` reintroduction

## Next Steps

### Immediate (Phase 1 implementation):

1. Design `_manifest.yaml` schema format and validation rules
2. Add schema compatibility checking to `create_versioned_lkups()`
3. Wrap `create_lkups()` calls in `tryCatch()` with logging
4. Generate manifests for existing vintages (or implement date fallback)
5. Add unit tests validating manifest vs actual file presence
6. Deploy to TEST environment, validate against real `/Data`
7. Deploy to PROD, monitor startup logs for schema mismatches
8. Document manifest format and schema versioning policy

### Follow-up (Phase 2 preparation):

9. Tag current pipapi release (e.g., v1.5.6) and create release notes documenting its supported schema
10. Create `pairings.yaml` manifest in Azure repo recording historical code–schema mappings
11. Draft Dockerfile pinning changes and `PIPAPI_VERSION` env var support
12. Plan v1.6.0 release that declares `lineup-v2` only and deletes all old lineup code
13. Coordinate with data pipeline team on manifest addition to new vintages

### Deferred (Phase 3 evaluation):

14. Monitor reproduction requests from internal team and external consumers (6-month window)
15. If demand exists, design Phase 3b on-demand deployment automation
16. If low demand, document ephemeral Docker process (Phase 3c) and close

## Related Work

- Prior brainstorm exploration (2026-08-12, no longer accurate per owner)
- Session handoff (2026-08-27) provided deployment architecture corrections and coupling analysis
- Codebase investigation (2026-08-28) confirmed schema differences are computational, not just file naming
