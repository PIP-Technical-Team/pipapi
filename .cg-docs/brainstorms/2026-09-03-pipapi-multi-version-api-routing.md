---
date: 2026-09-03
title: "PIP API Multi-Version Routing Architecture"
status: decided
scope: "Deep"
artifact-schema-version: 1
chosen-approach: "Path-based routing with shared storage (Phase 1), partitioned storage recommended for ITS discussion"
tags: [versioning, deployment, routing, infrastructure, azure, multi-release, concurrent-serving]
related-brainstorms:
  - 2026-08-28-pipapi-code-data-versioning-model.md
---

# PIP API Multi-Version Routing Architecture

## Context

This brainstorm extends the [2026-08-28 code-data versioning model](2026-08-28-pipapi-code-data-versioning-model.md) to support **concurrent multi-version API routing**. The original brainstorm established a phased approach (schema contracts → code pinning → historical addressability) but assumed only a single active API instance runs at any time. 

**New requirement**: Users need the ability to target specific **releases** concurrently, where each release = a deterministic pairing of a pipapi package version + schema family. This enables reproducibility across breaking schema changes (e.g., querying lineup-v1 results from 2024 while the current production API serves lineup-v2).

**Validated demand**: This requirement came from explicit user requests for concurrent access to different release versions, not hypothetical planning.

**Scope boundary**: This brainstorm addresses the **serving and infrastructure layer** only. The foundation (schema manifests, code pinning, `pairings.yaml`) is covered by the original brainstorm and assumed to be completed or in progress.

**Ownership model**:
- **Your team controls**: pipapi code, main.R, data organization specifications, Dockerfile specification, git tags/releases
- **ITS team controls**: Azure DevOps pipelines, container orchestration, load balancer/routing infrastructure, storage provisioning, deployment triggers

This brainstorm defines the **interface requirements** and **architectural patterns** for ITS to implement, not the implementation itself.

## Requirements

### Functional

- **Concurrent release serving**: Multiple pipapi releases (e.g., v1.4.2 + lineup-v1, v1.6.0 + lineup-v2) served simultaneously from distinct containers
- **Release targeting**: Users can explicitly target a release via URL structure (default: latest stable release)
- **Backward compatibility**: Existing `/api/v1/...` endpoints continue working (route to latest release)
- **Vintage selection within release**: Existing `?version=` / `release_version` / `ppp_version` / `identity` query parameters continue working for vintage selection within a release's schema family
- **Zero endpoints.R changes**: Routing handled by infrastructure layer; pipapi route annotations (`@get /api/v1/pip`) unchanged

### Non-Functional

- **Phased deployment**: Changes can be layered incrementally (application layer → infrastructure layer → optimization), not big-bang
- **Operational clarity**: Clear separation between your team's deliverables (code, specifications) and ITS deliverables (infrastructure, deployment)
- **Efficiency alignment**: Must address `compound-gpid.md` constraint "Code should be as efficient as possible" — storage/mounting strategy affects startup performance
- **ITS coordination**: Storage strategy, lifecycle policy, and build triggers require joint decisions with ITS

## Architectural Analysis

### Current Architecture Constraints

**Pipapi's per-process code pinning** (from existing codebase investigation):
- `start_api(api_version = "v1", ...)` sources `inst/plumber/v1/plumber.R` at startup and mounts routes with hardcoded `/api/v1/` prefix (endpoints.R:325-331)
- Route annotations are literal strings (`@get /api/v1/pip`), not parameterized — the string `v1` is baked into every endpoint decorator
- **Implication**: A single plumber process cannot serve two different pipapi package versions simultaneously (no per-request code switching)
- **Consequence**: Multi-release serving inherently requires **multiple deployed containers**, one per release, with upstream routing

**Vintage resolution is per-request** (existing behavior to preserve):
- `validate_version` filter calls `return_correct_version()` against `lkups$versions` (endpoints.R:91-130)
- If no vintage parameters specified, defaults to `lkups$latest_release` (max release date from loaded vintages) (endpoints.R:94-100)
- **Implication**: The `?version=` mechanism already works for selecting vintages within a release; it just needs to operate within the schema-filtered `lkups` for each release container

### URL Routing Design

**Requirement**: Release targeting must happen **before** the request reaches pipapi code (since code version is pinned per-process).

**Evaluated options**:
1. **Path-based**: `/releases/v1.4.2/api/v1/pip` (release prefix before existing API path)
2. **Host-based**: `v1-4-2.api.pipdata.org/api/v1/pip` (subdomain per release)
3. **Header-based**: `X-API-Version: v1.4.2` header
4. **Query parameter**: `?api_version=v1.4.2&version=20250601...` (overloads existing `?version=`)

**Chosen: Path-based (Option 1)** — Rationale:
- Extends existing `api_version` convention naturally (`/api/v1/` → `/releases/v1.4.2/api/v1/`)
- Zero endpoints.R changes (ITS router strips `/releases/v1.4.2` prefix, forwards `/api/v1/pip` unchanged)
- Preserves HTTP cache-by-URL semantics (pipapi already relies on `Cache-Control` headers, endpoints.R:271-286)
- Future-proof: composes cleanly if API v2 routes are introduced (e.g., `/releases/v1.6.0/api/v2/...`)
- ITS implementation: standard reverse proxy path-prefix routing (simple, well-understood)

**Rejected alternatives**:
- **Host-based**: Adds DNS/cert management overhead disproportionate to benefits
- **Header-based**: Breaks HTTP caching (cache keys on URL, not headers), less discoverable for API consumers
- **Query parameter**: Conflates two axes (release selection = routing concern, vintage selection = within-release data concern)

**Default/fallback behavior**:
- `/api/v1/pip` (no release prefix) → routes to latest stable release (backward compatibility)
- `/releases/latest/api/v1/pip` → explicit alias for current stable (self-documenting, avoids silent version drift for reproducibility-critical consumers)

### Container Build Strategy

**Requirement**: Each release needs a distinct container image with pinned pipapi code version.

**Design**:
- **Image naming**: `pipapi:{version}` (e.g., `pipapi:v1.4.2`, `pipapi:v1.6.0`, `pipapi:latest`)
- **Dockerfile**: Parameterized with build arg `ARG PIPAPI_VERSION`
  ```dockerfile
  ARG PIPAPI_VERSION=v1.6.0
  RUN R -e "remotes::install_github('PIP-Technical-Team/pipapi@${PIPAPI_VERSION}')"
  COPY main.R /app/main.R
  CMD ["Rscript", "/app/main.R"]
  ```
- **ITS pipeline**: Sets `--build-arg PIPAPI_VERSION=v1.6.0` when building
- **Build trigger**: TBD with ITS (options: automated on git tag push from this repo, manual pipeline trigger, webhook-based)

**Rejected alternatives**:
- **Multiple Dockerfiles per release**: Dockerfile proliferation, manual maintenance
- **Dockerfile in this repo**: Couples R package repo to deployment concerns; ITS prefers owning deployment artifacts

### Data Storage and Mounting

**Critical trade-off**: Storage architecture directly impacts startup performance and charter compliance ("Code should be as efficient as possible").

#### Option A: Shared Flat Storage with Schema Filtering

**Structure**:
```
Azure Blob/File: /Data/
├── 20240315_2024_02_01_PROD/ (_manifest.yaml: schema=lineup-v1)
├── 20250601_2025_01_02_PROD/ (_manifest.yaml: schema=lineup-v2)
├── 20250915_2025_02_01_PROD/ (_manifest.yaml: schema=lineup-v2)
└── ... (~200 vintage folders)
```

**Mounting**: All containers mount `/Data` (uniform mount configuration)

**Filtering**: `create_versioned_lkups()` scans all folders, reads `_manifest.yaml`, loads only schema-compatible vintages

**Pros**:
- ✅ **Zero data migration** — no reorganization of existing vintages
- ✅ **Data pipeline unchanged** — continues writing to flat `/Data/`
- ✅ **Operationally simple** — single mount configuration for all containers
- ✅ **Transition releases natural** — releases supporting multiple schemas (e.g., v1.5.0 → both lineup-v1 and lineup-v2) load from one scan

**Cons**:
- ❌ **Startup overhead** — each container scans all ~200 vintages, rejects 60-80% as incompatible (~3600+ file metadata checks per startup)
- ❌ **No storage isolation** — containers have filesystem access to wrong-schema data (filter is logical, not physical)
- ❌ **Efficiency charter conflict** — scanning and rejecting most vintages is wasteful, especially with Azure Blob/File latency
- ❌ **Scales poorly** — scan cost grows linearly with vintage accumulation (in 2027-2028, could be 300-400 vintages)

#### Option B: Schema-Partitioned Storage with Release-Specific Mounts

**Structure**:
```
Azure Blob/File: /Data/
├── lineup-v1/
│   ├── 20240315_2024_02_01_PROD/
│   ├── 20240820_2024_01_02_PROD/
│   └── ... (lineup-v1 vintages only)
├── lineup-v2/
│   ├── 20250601_2025_01_02_PROD/
│   ├── 20250915_2025_02_01_PROD/
│   └── ... (lineup-v2 vintages only)
```

**Mounting**: Each container mounts only its schema partition
- Container A (pipapi:v1.4.2): mounts `/Data/lineup-v1` → `/Data` in-container
- Container B (pipapi:v1.6.0): mounts `/Data/lineup-v2` → `/Data` in-container

**Filtering**: Not needed — `create_versioned_lkups()` only sees compatible vintages

**Pros**:
- ✅ **Faster startup** — 60-80% reduction in file scans (e.g., 50 lineup-v2 vintages vs. 200 total)
- ✅ **Storage-level isolation** — containers physically cannot access wrong-schema data (mount boundary enforces it)
- ✅ **Efficiency charter alignment** — no wasteful scanning/rejection
- ✅ **Independent lifecycle** — archive/delete old schema partitions separately (e.g., move lineup-v1 to cold storage in 2028)
- ✅ **Scales better long-term** — new schemas (lineup-v3, v4) don't add to scan cost of existing containers

**Cons**:
- ❌ **Data migration required** — one-time reorganization of ~200 existing vintages into schema folders
- ❌ **Data pipeline changes** — ITS pipeline must route new vintages to correct partition (requires schema awareness)
- ❌ **Mount configuration complexity** — per-release mount mapping (ITS must track release → schema → partition)
- ❌ **Transition release handling** — releases supporting multiple schemas need multi-mount configuration (e.g., v1.5.0 mounts both lineup-v1 and lineup-v2)

#### Storage Decision

**Recommendation**: 
- **Start with Option A** (shared storage) for immediate deployment — no ITS coordination blocker
- **Migrate to Option B** (partitioned storage) after initial stability proven — operational optimization and charter compliance
- **However**: Option B should be a **high-priority ITS discussion** because Option A violates the "efficient as possible" charter constraint

### Application Configuration Strategy

**Requirement**: main.R must adapt to deployment environment (data paths, schema filtering) without hardcoding per-release.

**Design**: Environment variable injection (ITS sets these when deploying containers)

```r
# main.R
data_root <- Sys.getenv("PIPAPI_DATA_ROOT", "/Data")
schema_filter <- Sys.getenv("PIPAPI_SCHEMA", "")  # Optional: explicit filter
api_version <- Sys.getenv("PIPAPI_API_VERSION", "v1")

lkups <- create_versioned_lkups(
  data_dir = data_root,
  schema_filter = if (schema_filter != "") schema_filter else NULL
)
start_api(api_version = api_version, port = 8080)
```

**Environment variables**:

| Variable | Purpose | Example | Required? | Storage Option A | Storage Option B |
|----------|---------|---------|-----------|------------------|------------------|
| `PIPAPI_DATA_ROOT` | Data mount path | `/Data` | Yes | `/Data` | `/Data/lineup-v2` |
| `PIPAPI_SCHEMA` | Schema filter | `lineup-v2` | No | Set (filters scan) | Omit (mount pre-filtered) |
| `PIPAPI_API_VERSION` | API route version | `v1` | No | Default `v1` | Default `v1` |

**Rationale for env vars over manifest file**:
- ✅ Zero files to maintain when cutting releases (no `release-config.yaml` to update)
- ✅ ITS-native (Azure container deployment already uses env vars)
- ✅ Runtime flexibility (ITS can override without rebuilding container)
- ✅ Testable locally (`Sys.setenv(PIPAPI_SCHEMA="lineup-v2")`)
- ✅ Respects ownership boundary (deployment config = ITS responsibility, not yours)

## Approaches Considered

### Approach 1: Path-Based Routing with Shared Storage (Minimum Viable Multi-Release)

**Summary**: Deploy multiple release-specific containers behind ITS-managed reverse proxy routing by URL path. Shared flat storage with schema filtering at startup.

**Components**:
- **Routing**: `/releases/{version}/api/v1/*` → container running `pipapi:{version}`
- **Storage**: Option A (shared `/Data/`, manifest filtering)
- **Container build**: Parameterized Dockerfile with `ARG PIPAPI_VERSION`
- **Configuration**: Environment variables (`PIPAPI_DATA_ROOT`, `PIPAPI_SCHEMA`)

**Pros**:
- ✅ Quick deployment (no data migration)
- ✅ Extends Phase 1/2 work from original brainstorm
- ✅ Zero endpoints.R changes
- ✅ Backward compatible
- ✅ Minimal ITS coordination

**Cons**:
- ❌ Startup overhead (scans all vintages)
- ❌ No storage isolation
- ❌ Violates efficiency charter constraint
- ❌ Lifecycle policy undefined (deferred to ITS)

**Effort**: **Medium** — 8-12 days
- 2 days: main.R env var support + testing
- 2 days: Dockerfile parameterization + pipeline integration (ITS-led)
- 3 days: ITS routing configuration
- 2 days: End-to-end testing
- 1-2 days: Documentation

**Recommended?** **Yes, as Phase 1** — Unblocks concurrent serving immediately, proves architecture, minimal risk.

---

### Approach 2: Path-Based Routing with Partitioned Storage (Efficiency-Optimized)

**Summary**: Approach 1 + schema-partitioned storage for performance and isolation.

**What changes**:
- **Storage**: Option B (partitioned `/Data/lineup-v1/`, `/Data/lineup-v2/`)
- **Mounting**: Per-release schema-specific mounts
- **Filtering**: Not needed (mount boundary pre-filters)

**Pros** (additions to Approach 1):
- ✅ 60-80% faster startup
- ✅ Storage-level isolation
- ✅ Efficiency charter alignment
- ✅ Better long-term scalability

**Cons** (additions to Approach 1):
- ❌ Data migration required
- ❌ Data pipeline changes (ITS coordination)
- ❌ Mount configuration complexity
- ❌ Transition release multi-mount handling

**Effort**: **Large** — 15-20 days (includes Approach 1 + migration)
- Approach 1 baseline: 8-12 days
- +2 days: Migration planning
- +3 days: ITS pipeline changes
- +2 days: Multi-mount support
- +2-3 days: Migration execution

**Recommended?** **Yes, but as Phase 2 after Approach 1 stability** — Or as Approach 1 replacement if ITS commits to upfront coordination. **High-priority ITS discussion** due to charter alignment.

---

### Approach 3: Host-Based Routing (Alternative if Path-Based Blocked)

**Summary**: Subdomain routing instead of path prefixes (e.g., `v1-4-2.api.pipdata.org`).

**Pros**:
- ✅ Cleaner URLs (no path nesting)
- ✅ Standard practice for versioned APIs

**Cons**:
- ❌ DNS management overhead per release
- ❌ TLS certificate complexity (wildcard or per-subdomain)
- ❌ Retirement complexity (DNS record removal slower than routing rule changes)
- ❌ No real advantage over path-based for this use case

**Effort**: **Medium** — 10-14 days

**Recommended?** **No, unless path-based is blocked** — Path-based is simpler and fits existing conventions better.

---

## Comparison Summary

| Dimension | Approach 1: Shared Storage | Approach 2: Partitioned Storage | Approach 3: Host-Based |
|-----------|----------------------------|--------------------------------|------------------------|
| **Deployment speed** | ✅ Fast (no migration) | ❌ Slower (migration needed) | 🟡 Medium (DNS setup) |
| **Startup performance** | ❌ Scans all vintages (~200) | ✅ Scans partition only (~50-80) | ❌ Scans all vintages |
| **Storage isolation** | ❌ Logical only (filter-based) | ✅ Physical (mount boundary) | ❌ Logical only |
| **Charter alignment** | ❌ Violates efficiency constraint | ✅ Aligns with efficiency constraint | ❌ Violates efficiency constraint |
| **Long-term scalability** | ❌ Degrades with vintage growth | ✅ Scales linearly per schema | ❌ Degrades with vintage growth |
| **ITS coordination** | ✅ Minimal | ❌ High (pipeline changes) | 🟡 Medium (DNS/certs) |
| **Backward compatibility** | ✅ Full | ✅ Full | ✅ Full |
| **Effort** | 8-12 days | 15-20 days | 10-14 days |

## Decision

**Chosen approach**: **Approach 1 (Path-Based Routing with Shared Storage)** as initial implementation, with **Approach 2 (Partitioned Storage)** as high-priority ITS discussion topic for charter compliance.

**Rationale**:
1. **Phased deployment preferred** — Layer changes incrementally (application → infrastructure → optimization), not big-bang
2. **Unblock immediately** — Approach 1 has no data migration dependency, proves architecture quickly
3. **Charter alignment critical** — Approach 2 better aligns with "efficient as possible" constraint; should be pursued with ITS after initial stability
4. **Validated demand** — Concurrent serving is a real user requirement, not hypothetical
5. **Operational clarity** — Clear separation of responsibilities (your team: code/specs, ITS: infrastructure)

**Implementation strategy**: Four-phase rollout building on original brainstorm's foundation

---

## Implementation Plan

### Phase 1: Foundation (Original Brainstorm — Assumed Complete/In Progress)

**From [2026-08-28 brainstorm](2026-08-28-pipapi-code-data-versioning-model.md)**:
- ✅ Schema contracts (`_manifest.yaml` in vintage folders declaring schema)
- ✅ Compatibility checking in `create_versioned_lkups()` (filters vintages by schema)
- ✅ Per-vintage isolation (`tryCatch()` wrapping so one bad vintage doesn't crash startup)
- ✅ Code pinning (Dockerfile installs tagged releases, not floating `@DEV` branch)
- ✅ `pairings.yaml` manifest (maps pipapi release → schema family → release date)

**Status**: Verify with your team whether Phase 1 is complete before starting Phase 2.

---

### Phase 2: Application Layer (Your Responsibility)

**Goal**: Make pipapi containers release-aware and configurable for multi-release deployment.

**Deliverables**:

1. **main.R environment variable support**
   ```r
   data_root <- Sys.getenv("PIPAPI_DATA_ROOT", "/Data")
   schema_filter <- Sys.getenv("PIPAPI_SCHEMA", "")
   api_version <- Sys.getenv("PIPAPI_API_VERSION", "v1")
   
   lkups <- create_versioned_lkups(
     data_dir = data_root,
     schema_filter = if (schema_filter != "") schema_filter else NULL
   )
   start_api(api_version = api_version, port = 8080)
   ```

2. **README.md deployment documentation section**
   - Document required/optional env vars (`PIPAPI_DATA_ROOT`, `PIPAPI_SCHEMA`, `PIPAPI_API_VERSION`)
   - Document container naming convention (`pipapi:{version}`)
   - Document URL routing specification for ITS (see Phase 3 specification below)
   - Document release → schema mapping from `pairings.yaml`

3. **Testing**
   - Test `create_versioned_lkups()` with `PIPAPI_SCHEMA` set (filters to one schema)
   - Test with `PIPAPI_SCHEMA` unset (loads all compatible schemas, for transition releases)
   - Test with schema-partitioned mock data (validates Option B compatibility)
   - Test default behavior (no env vars set)

**Effort**: 2-3 days

**Dependencies**: Phase 1 complete (manifest filtering logic exists)

**Acceptance criteria**:
- `main.R` reads env vars and passes them correctly to `create_versioned_lkups()` and `start_api()`
- Tests pass for both shared storage (Option A) and partitioned storage (Option B) scenarios
- README documents deployment configuration clearly

---

### Phase 3: Infrastructure Layer (ITS Responsibility, Your Specification)

**Goal**: Deploy routing infrastructure to serve multiple release containers concurrently.

**Your deliverable**: Write specification document for ITS containing the following sections.

---

#### 3.1 URL Routing Rules

**Specification for ITS**:

```
# Primary routing pattern
/releases/{release_tag}/api/v1/*  → Container running pipapi:{release_tag}
                                    Load balancer/reverse proxy strips /releases/{release_tag}
                                    Forwards /api/v1/* to target container

# Aliases for usability
/releases/latest/api/v1/*         → Container tagged as "stable" in deployment config
/api/v1/*                         → Fallback to latest stable (backward compatibility)

# Examples
Request: /releases/v1.4.2/api/v1/pip?version=20240315_2024_02_01_PROD
  → Routes to: pipapi:v1.4.2 container
  → Container receives: /api/v1/pip?version=20240315_2024_02_01_PROD
  → Response: lineup-v1 results for specified vintage

Request: /releases/v1.6.0/api/v1/pip?release_version=2025_01_02
  → Routes to: pipapi:v1.6.0 container
  → Container receives: /api/v1/pip?release_version=2025_01_02
  → Response: lineup-v2 results for latest matching vintage

Request: /api/v1/pip
  → Routes to: pipapi:latest container (current stable release)
  → Container receives: /api/v1/pip
  → Response: latest release, latest vintage (default behavior preserved)

Request: /releases/retired-version/api/v1/pip
  → Response: HTTP 410 Gone or 404 Not Found (per lifecycle policy, see section 3.5)
```

**Implementation notes for ITS**:
- Standard reverse proxy path-prefix matching (e.g., Azure Application Gateway path-based routing, or nginx `location` blocks)
- Routing happens **before** request reaches pipapi code (code cannot see the `/releases/{version}/` prefix)
- Cache-Control headers from pipapi responses should be preserved (existing behavior, endpoints.R:271-286)

---

#### 3.2 Container Image Build

**Parameterized Dockerfile** (ITS maintains in Azure DevOps repo `ITSES-POVERTYSCOREAPI`):

```dockerfile
ARG PIPAPI_VERSION=v1.6.0
ARG R_VERSION=4.3.0

FROM rocker/r-ver:${R_VERSION}

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install pipapi from pinned release
RUN R -e "install.packages('remotes')"
RUN R -e "remotes::install_github('PIP-Technical-Team/pipapi@${PIPAPI_VERSION}')"

# Copy startup script
COPY main.R /app/main.R
WORKDIR /app

# Expose API port
EXPOSE 8080

# Start API
CMD ["Rscript", "main.R"]
```

**Build command**:
```bash
docker build \
  --build-arg PIPAPI_VERSION=v1.6.0 \
  --tag pipapi:v1.6.0 \
  --tag pipapi:latest \
  .
```

**Image tagging convention**:
- `pipapi:v1.4.2` — specific release (tag matches git tag from pipapi repo)
- `pipapi:v1.6.0`
- `pipapi:latest` — alias for current stable release (multi-tag the image)

**Build trigger**: TBD with ITS (options):
- **Automated**: Webhook from GitHub when git tag pushed to pipapi repo → triggers Azure DevOps pipeline
- **Manual**: ITS runs pipeline manually after release announcement
- **Hybrid**: Automated for DEV/QA environments, manual gate for PROD

**ITS decision required**: Which trigger mechanism fits your deployment workflow?

---

#### 3.3 Container Environment Configuration

**Specification for ITS**: Each deployed container requires these environment variables.

| Variable | Purpose | Example | Required? | Notes |
|----------|---------|---------|-----------|-------|
| `PIPAPI_DATA_ROOT` | Data mount path | `/Data` | Yes | For Option A (shared): `/Data`. For Option B (partitioned): `/Data/lineup-v2` |
| `PIPAPI_SCHEMA` | Schema filter | `lineup-v2` | No | For Option A: set explicitly to filter. For Option B: omit (mount is pre-filtered) |
| `PIPAPI_API_VERSION` | API route version | `v1` | No | Default `v1`. Future-proofs if API v2 routes introduced |

**Release → Schema mapping** (from `pairings.yaml` in pipapi repo):

```yaml
# Example pairings.yaml (maintained in pipapi repo)
pairings:
  - pipapi_version: v1.6.0
    schema_id: lineup-v2
    release_date: 2026-09-01
  - pipapi_version: v1.4.2
    schema_id: lineup-v1
    release_date: 2025-04-15
```

**ITS implementation**: Deployment config (e.g., Kubernetes manifests, Azure Container Instances config) uses `pairings.yaml` to set env vars per container.

Example (Kubernetes-style):
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: pipapi-v1-6-0
spec:
  replicas: 2
  template:
    spec:
      containers:
      - name: pipapi
        image: pipapi:v1.6.0
        env:
        - name: PIPAPI_DATA_ROOT
          value: "/Data"               # Option A
          # value: "/Data/lineup-v2"   # Option B
        - name: PIPAPI_SCHEMA
          value: "lineup-v2"            # Option A only
        - name: PIPAPI_API_VERSION
          value: "v1"
        volumeMounts:
        - name: data
          mountPath: /Data
      volumes:
      - name: data
        azureFile:
          secretName: azure-storage-secret
          shareName: pip-data
          # shareName: pip-data/lineup-v2   # Option B
```

---

#### 3.4 Data Storage Architecture (ITS Decision Required)

**Two options for ITS to evaluate**:

---

##### Option A: Shared Flat Storage with Schema Filtering

**Structure**:
```
Azure Blob/File Storage: /Data/
├── 20240315_2024_02_01_PROD/
│   └── _manifest.yaml (schema: lineup-v1)
├── 20250601_2025_01_02_PROD/
│   └── _manifest.yaml (schema: lineup-v2)
└── ... (~200 vintage folders, flat)
```

**Mounting**: All containers mount `/Data` (uniform configuration)

**Container startup**: `create_versioned_lkups()` scans all folders, reads manifests, filters by `PIPAPI_SCHEMA` env var

**Pros**:
- ✅ No data migration required
- ✅ Data pipeline unchanged (continues writing vintages to flat `/Data/`)
- ✅ Operationally simple (one mount path for all containers)
- ✅ Transition releases natural (can load multiple schemas from one scan)

**Cons**:
- ❌ Startup overhead: Each container scans ~200 folders, rejects 60-80% as incompatible (~3600 file metadata checks)
- ❌ No storage isolation: Containers have filesystem access to wrong-schema data (mitigated by filtering, but not enforced)
- ❌ **Violates charter constraint**: "Code should be as efficient as possible" (`compound-gpid.md`) — scanning and rejecting data is wasteful
- ❌ Scales poorly: Scan cost grows linearly with vintage accumulation (2027-2028: 300-400 vintages)

**Effort (ITS)**: 3-5 days (routing configuration, container orchestration, no storage changes)

---

##### Option B: Schema-Partitioned Storage with Release-Specific Mounts

**Structure**:
```
Azure Blob/File Storage: /Data/
├── lineup-v1/
│   ├── 20240315_2024_02_01_PROD/
│   ├── 20240820_2024_01_02_PROD/
│   └── ... (lineup-v1 vintages only)
├── lineup-v2/
│   ├── 20250601_2025_01_02_PROD/
│   ├── 20250915_2025_02_01_PROD/
│   └── ... (lineup-v2 vintages only)
```

**Mounting**: Each container mounts only its schema partition
- Container A (pipapi:v1.4.2): `/Data/lineup-v1` → `/Data` in-container
- Container B (pipapi:v1.6.0): `/Data/lineup-v2` → `/Data` in-container

**Container startup**: `create_versioned_lkups()` scans only mounted partition (no filtering needed, all vintages compatible)

**Pros**:
- ✅ **60-80% faster startup**: Scans 50-80 vintages (partition) instead of 200+ (full storage)
- ✅ **Storage-level isolation**: Containers physically cannot access wrong-schema data (mount boundary enforces)
- ✅ **Charter alignment**: No wasteful scanning/rejection — aligns with "efficient as possible"
- ✅ **Independent lifecycle**: Archive/delete old schema partitions separately (e.g., move lineup-v1 to Azure Blob Archive tier in 2028)
- ✅ **Better long-term scalability**: lineup-v3, lineup-v4 in future don't add scan cost to existing containers

**Cons**:
- ❌ **Data migration required**: One-time reorganization of ~200 existing vintages into schema folders
- ❌ **Data pipeline changes**: ITS pipeline (ITSES-POVERTYSCORE-DATA) must route new vintages to correct partition (requires schema awareness)
- ❌ **Mount configuration complexity**: Per-release mount mapping (ITS deployment config must track release → schema → partition path)
- ❌ **Transition release handling**: Releases supporting multiple schemas (e.g., pipapi v1.5.0 → both lineup-v1 and lineup-v2) need multi-mount logic

**Effort (ITS)**: 8-12 days
- 2 days: Migration planning + vintage reorganization script
- 3 days: Data pipeline changes (schema routing logic)
- 2 days: Per-release mount configuration updates
- 2 days: Multi-mount support testing (for transition releases)
- 2-3 days: Migration execution + validation

---

##### ITS Decision Required

**Recommendation from pipapi team**: Start with **Option A** for immediate deployment (no blockers), but **prioritize Option B discussion** because Option A violates the `compound-gpid.md` efficiency charter constraint.

**Questions for ITS**:
1. Is the upfront coordination cost of Option B (data migration + pipeline changes) acceptable to achieve charter compliance and long-term efficiency?
2. If Option A is chosen initially, what is the timeline for migrating to Option B? (Recommend within 6 months to avoid compounding technical debt)
3. For Option B: Can the data pipeline (ITSES-POVERTYSCORE-DATA) route vintages by schema? What metadata does it have access to when writing vintages?

---

#### 3.5 Container Lifecycle Policy (ITS Decision Required)

**Context**: With multiple releases deployed concurrently, need rules for when to deploy new containers and when to retire old ones.

**Deployment triggers** (when to create new release containers):
- Deploy on every pipapi git tag? (Could be 10-20 releases per year)
- Deploy only on explicit request from pipapi team?
- Deploy automatically to DEV/QA, manual gate for PROD?

**Retirement triggers** (when to tear down old release containers):
- **Time-based**: Retire releases older than X months (e.g., 18 months)?
- **Usage-based**: Retire if <Y% of requests in past 90 days (e.g., <1%)?
- **Manual only**: Pipapi team explicitly requests retirement?
- **Emergency**: Critical security/data bug requires immediate removal?

**Concurrent release limit**:
- **All tagged releases forever** (n = unbounded, high resource cost)?
- **Current + previous N releases** (e.g., n = 3: current, previous, previous-previous)?
- **Current + any receiving requests** (elastic, usage-driven)?

**HTTP response for retired releases**:
```
Request: /releases/v1.2.0/api/v1/pip (v1.2.0 has been retired)
Response options:
  - HTTP 410 Gone + {"error": "Release v1.2.0 retired on 2027-01-15. Use v1.6.0 or later."}
  - HTTP 404 Not Found (silent failure)
  - HTTP 301 Redirect to /releases/latest/api/v1/pip (breaks reproducibility, not recommended)
```

**Pipapi team recommendation**: 
- **Deployment**: Automatic to DEV/QA on tag push, manual PROD gate (allows testing before production)
- **Retirement**: Usage-based with manual override (retire if <1% requests in 90 days, unless pinned as "LTS")
- **Concurrent limit**: Current + previous 2 major releases (balances reproducibility and resource cost)
- **Retired response**: HTTP 410 Gone with message listing available releases

**ITS decisions required**:
1. Which deployment trigger model fits Azure DevOps workflow?
2. Which retirement trigger model fits operational capacity (monitoring, cost management)?
3. What is acceptable concurrent release count (affects VM/container resource planning)?

---

#### 3.6 Monitoring and Observability Requirements

**Metrics needed for lifecycle decisions**:
- Request count per release (per hour/day) → informs retirement decisions
- Container startup time per release → measures Option A vs B impact
- Error rates per release (startup failures, vintage load errors, request errors)
- Storage I/O per release (for Option A: measures scan overhead; for Option B: validates efficiency gains)

**Logging requirements**:
- Container startup logs: which vintages loaded, which rejected, total startup time
- Request routing logs: which release served each request (for tracing)
- Error logs: schema mismatches, missing vintages, incompatible manifests

**Dashboards (ITS responsibility)**:
- Release usage dashboard (traffic distribution across releases)
- Container health dashboard (startup times, error rates, resource utilization)
- Vintage coverage dashboard (which vintages available per release)

---

**Phase 3 Effort (ITS)**: 5-10 days
- 2 days: Routing infrastructure configuration (load balancer rules, reverse proxy setup)
- 2 days: Container orchestration (deployment configs, env var management)
- 2 days: Build pipeline automation (parameterized Dockerfile, tagging)
- 2 days: Monitoring/logging integration
- 2-3 days: Initial release deployments (2-3 releases for pilot)

**Dependencies**: Phase 2 complete (main.R env var support, specification document delivered)

---

### Phase 4: Validation and Monitoring (Joint Responsibility)

**Goal**: Verify multi-release routing works end-to-end, establish operational monitoring.

**Test scenarios** (your team executes, ITS provides infrastructure):

1. **Basic routing**:
   - Query `/releases/v1.4.2/api/v1/pip?version=20240315_2024_02_01_PROD`
   - Verify: Routes to v1.4.2 container, returns lineup-v1 results
   - Query `/releases/v1.6.0/api/v1/pip?version=20250601_2025_01_02_PROD`
   - Verify: Routes to v1.6.0 container, returns lineup-v2 results

2. **Default behavior**:
   - Query `/api/v1/pip` (no release prefix)
   - Verify: Routes to latest release, returns current results
   - Query `/releases/latest/api/v1/pip`
   - Verify: Same result as `/api/v1/pip`

3. **Vintage resolution within release**:
   - Query `/releases/v1.6.0/api/v1/pip?release_version=2025_02_01`
   - Verify: Returns latest lineup-v2 vintage matching version criteria
   - Query `/releases/v1.6.0/api/v1/pip` (no vintage specified)
   - Verify: Returns latest lineup-v2 vintage (default behavior)

4. **Schema isolation**:
   - Query `/releases/v1.6.0/api/v1/pip?version=20240315_2024_02_01_PROD` (lineup-v1 vintage)
   - Verify: Returns error (vintage not found in lineup-v2 container) — correct isolation

5. **Retired release handling** (if retirement policy implemented):
   - Query `/releases/retired-version/api/v1/pip`
   - Verify: Returns HTTP 410 Gone (or per lifecycle policy)

6. **Load testing**:
   - Concurrent requests across multiple releases (simulate production load)
   - Verify: No routing errors, response times acceptable, containers don't interfere

7. **Container restart resilience**:
   - Restart a release container mid-test
   - Verify: Requests queue or fail gracefully, resume after startup, lkups reload correctly

**Performance benchmarks**:
- Container startup time (measure Option A scan overhead; baseline for Option B comparison)
- Request latency per release (ensure routing overhead is negligible)
- Storage I/O during startup (quantify Option A inefficiency)

**Monitoring validation**:
- Verify request count metrics are accurate (compare to ITS logs)
- Verify error rates are tracked per release
- Verify dashboards display correct data

**Effort**: 2-3 days
- 1 day: Test execution (scenarios 1-7)
- 0.5 days: Performance benchmarking
- 0.5 days: Monitoring validation
- 1 day: Issue triage + fixes

**Dependencies**: Phase 3 deployed (routing infrastructure live, at least 2 releases deployed)

**Acceptance criteria**:
- All test scenarios pass
- Monitoring dashboards operational
- Performance benchmarks documented (baseline for future optimization)
- Runbook documented (how to deploy new release, troubleshoot routing issues)

---

## Total Effort Estimate

| Phase | Owner | Effort | Dependencies |
|-------|-------|--------|--------------|
| Phase 1 (Foundation) | Your team | Already completed/in-progress | Original 2026-08-28 brainstorm |
| Phase 2 (Application) | Your team | 2-3 days | Phase 1 complete |
| Phase 3 (Infrastructure) | ITS | 5-10 days | Phase 2 spec delivered |
| Phase 4 (Validation) | Joint | 2-3 days | Phase 3 deployed |
| **Total (new work)** | - | **9-16 days** | Serialized dependencies |

**Option B migration** (if pursued later): +8-12 days (ITS-led, data reorganization + pipeline changes)

---

## Open Questions and Risks

### Critical ITS Discussion Topics

1. **Storage strategy** (Phase 3.4): Option A (shared storage) vs Option B (partitioned storage)?
   - **Impact**: Charter compliance ("efficient as possible"), startup performance, long-term scalability
   - **Recommendation**: Start with Option A for speed, but prioritize Option B migration within 6 months for charter alignment
   - **ITS input needed**: Feasibility of data migration + pipeline changes, timeline preference

2. **Lifecycle policy** (Phase 3.5): How to manage concurrent releases over time?
   - **Impact**: Resource costs (VM/container count), operational complexity, reproducibility guarantees
   - **ITS input needed**: Deployment trigger preference, retirement criteria, concurrent release limit, monitoring capacity

3. **Build triggers** (Phase 3.2): Automated (webhook on git tag) vs manual (pipeline trigger)?
   - **Impact**: Your release workflow, deployment velocity, testing gates
   - **ITS input needed**: Azure DevOps pipeline capabilities, DEV/QA/PROD promotion process

4. **Monitoring capabilities**: What metrics/logging does ITS infrastructure currently provide?
   - **Impact**: Lifecycle decision-making (usage-based retirement requires request count metrics)
   - **ITS input needed**: Available monitoring tools, dashboard creation effort, log retention

### Risks

1. **ITS coordination dependency**
   - **Risk**: Phase 3 blocked if ITS priorities shift or resource constraints emerge
   - **Likelihood**: Medium (depends on ITS roadmap)
   - **Impact**: High (blocks concurrent serving deployment)
   - **Mitigation**: Phases 1-2 deliver value independently (schema coupling solved, code pinned); concurrent serving is enhancement, not critical blocker. Maintain clear communication on timelines.

2. **Storage migration complexity** (if Option B pursued)
   - **Risk**: Vintage reorganization could break running API during migration window
   - **Likelihood**: Low (if planned carefully)
   - **Impact**: High (production downtime)
   - **Mitigation**: 
     - Ship Option A first, prove stability, then migrate to Option B during planned maintenance
     - Test migration on DEV/QA environments before PROD
     - Implement rollback plan (keep old flat storage accessible during migration)

3. **Transition release handling** (multi-schema support)
   - **Risk**: If Option B (partitioned storage) is chosen, transition releases like pipapi v1.5.0 (supports both lineup-v1 and lineup-v2) require multi-mount configuration, which is untested
   - **Likelihood**: Medium (if transition releases are cut in future)
   - **Impact**: Medium (transition release deployment blocked)
   - **Mitigation**:
     - Document multi-mount configuration in Phase 2
     - Test explicitly in Phase 4 with mock transition release
     - Fallback: transition releases use Option A mounting (shared storage) even if other releases use Option B

4. **Lifecycle policy misalignment**
   - **Risk**: ITS retirement policy conflicts with institutional reproducibility requirements (e.g., aggressive auto-retirement breaks provenance for published numbers in World Bank reports)
   - **Likelihood**: Medium (requires stakeholder alignment)
   - **Impact**: High (breaks external API contracts)
   - **Mitigation**:
     - Involve stakeholders (data consumers, external API users, World Bank publication teams) in lifecycle policy discussion before Phase 3
     - Consider "LTS" (long-term support) tagging for releases tied to major publications (never auto-retire)
     - Document retirement policy in API documentation with advance notice period (e.g., 6 months warning before retirement)

5. **Routing configuration errors**
   - **Risk**: ITS misconfigures path-stripping rules → requests fail silently or route to wrong containers (e.g., `/releases/v1.4.2/api/v1/pip` forwards as `/releases/v1.4.2/api/v1/pip` instead of `/api/v1/pip`)
   - **Likelihood**: Low (if tested properly)
   - **Impact**: High (API non-functional)
   - **Mitigation**:
     - Provide reference routing configurations in Phase 3 specification (nginx example, Azure Application Gateway example)
     - Phase 4 test suite includes path-stripping edge cases (verify container receives correct path)
     - Implement health checks per container (ITS monitoring can detect routing failures)

6. **Startup performance degradation** (Option A)
   - **Risk**: As vintages accumulate (300-400 by 2027-2028), Option A startup time becomes unacceptable (30-60 seconds), impacting container restart speed and deployment velocity
   - **Likelihood**: High (if Option A is permanent)
   - **Impact**: Medium (slower deployments, higher latency during restarts)
   - **Mitigation**:
     - Monitor startup time in Phase 4, establish baseline and alerting threshold
     - Plan Option B migration before threshold reached (e.g., migrate when startup time >20 seconds)
     - Interim optimization: implement startup caching (cache manifest scan results, invalidate on storage change)

7. **Demand validation failure**
   - **Risk**: After building infrastructure, actual usage of multi-release routing is low (most users hit `/api/v1/pip` default, rarely specify releases)
   - **Likelihood**: Low (demand was validated via explicit user requests)
   - **Impact**: Medium (wasted engineering effort, unused infrastructure)
   - **Mitigation**:
     - Track request distribution in Phase 4 (percentage hitting `/releases/*` vs `/api/v1/` default)
     - If low usage, investigate: Is the URL pattern discoverable? Do users know it exists? Update API documentation.
     - Even if external usage is low, internal reproducibility use case (debugging, auditing) justifies infrastructure

---

## Next Steps

### Immediate (Your Team Responsibility)

1. **Verify Phase 1 status** — Confirm with team:
   - Is manifest filtering (`_manifest.yaml` support in `create_versioned_lkups()`) implemented?
   - Is code pinning (Dockerfile installs tagged releases) implemented?
   - Does `pairings.yaml` exist and document release → schema mappings?

2. **Implement Phase 2** (2-3 days):
   - Add environment variable support to main.R
   - Update README.md with deployment configuration documentation
   - Write tests for env var scenarios (shared storage, partitioned storage, defaults)

3. **Write ITS specification document** (1-2 days):
   - Consolidate Phase 3 sections (3.1-3.6) into a formal spec document
   - Include URL routing examples, Dockerfile, env var table, storage options comparison, open questions
   - Format for ITS consumption (consider separate doc vs. this brainstorm as reference)

4. **Schedule ITS coordination meeting** (target: within 2 weeks):
   - Present specification document
   - Discuss storage strategy (Option A vs B), present charter alignment argument for Option B
   - Discuss lifecycle policy (deployment triggers, retirement criteria, concurrent release limit)
   - Discuss build trigger mechanism (automated vs manual)
   - Confirm timeline and resource allocation (ITS estimate for Phase 3 effort)

### Follow-Up (ITS-Led, Coordinated)

5. **ITS infrastructure implementation** (Phase 3, 5-10 days):
   - Routing configuration (load balancer/reverse proxy rules)
   - Container orchestration (deployment configs, env var management)
   - Build pipeline automation (parameterized Dockerfile, tagging, trigger setup)
   - Monitoring/logging integration

6. **Joint validation testing** (Phase 4, 2-3 days):
   - Execute test scenarios (routing, vintage resolution, schema isolation, load testing)
   - Performance benchmarking (startup time, request latency, storage I/O)
   - Monitoring validation (dashboards operational, metrics accurate)

7. **Production deployment** (pilot rollout):
   - Deploy 2-3 releases initially (e.g., v1.4.2 lineup-v1, v1.6.0 lineup-v2, latest)
   - Monitor usage patterns, error rates, resource utilization
   - Iterate on lifecycle policy based on observed demand
   - Expand to full release catalog after stability proven

### Deferred (Post-Deployment Optimization)

8. **Option B migration** (if approved, 8-12 days):
   - Detailed migration planning (vintage inventory, schema classification, rollback strategy)
   - Data reorganization (move vintages to partitioned storage)
   - Data pipeline changes (ITS updates ITSES-POVERTYSCORE-DATA to write to partitions)
   - Container mount reconfiguration (update deployment configs)
   - Validation (test all releases still load correct vintages, performance improvement measured)

9. **Monitoring dashboard refinement** (2-3 days):
   - Build release usage dashboard (track request distribution across releases)
   - Build container health dashboard (startup times, error rates per release)
   - Build vintage coverage dashboard (which vintages available per release)
   - Set up alerting (startup time threshold, error rate spike, routing failures)

10. **Operational runbook documentation** (1-2 days):
    - How to deploy a new release (tag code → build image → deploy container → update routing config)
    - How to retire an old release (update lifecycle policy → drain traffic → tear down container)
    - How to troubleshoot routing issues (verify path-stripping, check container logs, test vintage availability)
    - How to perform emergency rollback (route traffic to previous release, restart containers)

---

## Related Work

- **[2026-08-28 PIP API Code-Data Versioning Model](2026-08-28-pipapi-code-data-versioning-model.md)** — Foundation for this brainstorm; establishes Phase 1 (schema contracts) and Phase 2 (code pinning) from original plan. This brainstorm extends with multi-release routing (originally Phase 3, "Addressable Historical Pairings").

- **pipapi existing architecture** (codebase investigation):
  - `start_api()` in R/start_api.R:10-19 — API version pinned at process startup
  - `validate_version` filter in R/endpoints.R:91-130 — Per-request vintage resolution
  - `create_versioned_lkups()` in R/create_lkups.R:8-27 — Startup vintage scanning
  - `use_new_lineup_version()` in R/create_lkups.R:989-1001 — Schema detection via date heuristic (to be replaced by manifest-based detection)

- **Azure DevOps context** (from original brainstorm line 99-105):
  - Repo: `ITSES-POVERTYSCOREAPI` (ITS ownership)
  - Branches: `DEV` / `QA` / `PROD` (environment promotion)
  - Dockerfile currently installs `@DEV` floating branch (Phase 1 changes this to tagged releases)
  - Data pipeline: `ITSES-POVERTYSCORE-DATA` (writes vintages to shared storage)
