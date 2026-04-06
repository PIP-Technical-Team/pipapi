# pipapi — Architecture & Package Understanding

> **Date**: 2026-04-06
> **Package version**: 1.5.3
> **Scope**: Full review of R package + Plumber API

---

## 1. What is pipapi?

`pipapi` is an R package that powers the **Poverty and Inequality Platform (PIP)** REST API for the World Bank. It computes poverty and inequality statistics (headcount, poverty gap, severity, Watts index, Gini, decile shares, etc.) from survey microdata. The package can be used in two ways:

1. **As an R package** — users call `pip()` directly in an R session with access to the data folder.
2. **As a REST API** — a Plumber server exposes HTTP endpoints that wrap the same functions.

---

## 2. Data Architecture

### 2.1 Data Folder Structure

The package reads from a versioned data directory:

```
data-root/
├── YYYYMMDD_YYYY_MM_DD_PROD/      # e.g., 20250601_2021_01_02_PROD
│   ├── _aux/                       # Auxiliary tables (.fst files)
│   │   ├── country_list.fst
│   │   ├── countries.fst
│   │   ├── regions.fst
│   │   ├── pop.fst
│   │   ├── pop_region.fst
│   │   ├── missing_data.fst
│   │   ├── poverty_lines.fst
│   │   ├── censored.rds
│   │   ├── country_profiles.rds
│   │   └── ...
│   ├── estimations/                # Estimation metadata
│   │   ├── prod_svy_estimation.fst   # Survey-year lookup
│   │   ├── prod_ref_estimation.fst   # Reference/interpolation lookup
│   │   ├── prod_refy_estimation.fst  # New lineup lookup (post-May 2025)
│   │   ├── lineup_years.fst
│   │   ├── lineup_dist_stats.fst
│   │   └── dist_stats.fst
│   ├── survey_data/                # Individual survey microdata (.fst)
│   ├── lineup_data/                # Lineup data for new pathway (.fst)
│   └── cache/                      # DuckDB intermediate cache
│       └── cache.duckdb
├── YYYYMMDD_YYYY_MM_DD_INT/        # Internal version
└── YYYYMMDD_YYYY_MM_DD_TEST/       # Test version
```

### 2.2 The Lookup (`lkup`) Object

At startup, `create_versioned_lkups()` → `create_lkups()` builds a nested list holding all metadata needed for computation:

| Field | Type | Purpose |
|---|---|---|
| `svy_lkup` | data.table | Survey estimation metadata with file paths |
| `ref_lkup` | data.table | Reference/interpolation estimation metadata |
| `refy_lkup` | data.table | New-pathway lineup metadata (post-May 2025) |
| `dist_stats` | data.table | Distributional statistics (mean, median, Gini, etc.) |
| `lineup_dist_stats` | data.table | Lineup distribution stats for new pathway |
| `pop_region` | data.table | Regional population data |
| `aux_files` | list | Country list, regions, population, missing data |
| `cp_lkups` | list | Country profiles lookup |
| `pl_lkup` | data.table | Standard poverty lines table |
| `censored` | list | Tables controlling which stats to suppress |
| `return_cols` | list | Column specifications per endpoint |
| `query_controls` | list | Valid parameter ranges for input validation |
| `interpolation_list` | list | Pre-built lists grouping surveys by interpolation ID |
| `cache_data_id` | list | Hashes for cache invalidation |
| `use_new_lineup_version` | logical | Switches between old/new computation pathways |

### 2.3 Versioning

Data directories are named `YYYYMMDD_YYYY_MM_DD_{PROD|INT|TEST}`. The API automatically serves the latest PROD version unless the user specifies `release_version`, `ppp_version`, or `version` parameters.

---

## 3. Computation Pipeline

### 3.1 Two Pathways: Old vs New

The package is mid-transition between two computation approaches, switched by `use_new_lineup_version()` (date threshold: May 2025):

| Aspect | Old Pathway | New Pathway |
|---|---|---|
| **Switch condition** | Data folder before May 2025 | Data folder after May 2025 |
| **Lineup (fill-gap)** | `fg_pip_old()` → `wbpip:::prod_fg_*` | `fg_pip()` → `fgt_cumsum()` |
| **Survey-year** | `rg_pip_old()` → `process_dt_old()` | `rg_pip()` → `process_dt()` → `compute_fgt_dt()` |
| **Aggregation** | `pip_grp_logic()` | `pip_grp_new()` |
| **Performance** | Row-by-row via `wbpip` | Vectorized cumsum + `findInterval` |
| **Lookup** | `ref_lkup` + `interpolation_list` | `refy_lkup` + `lineup_years` |

### 3.2 Call Graph (Main `pip()` Flow)

```
pip()
  ├── validate_lkup()
  ├── if (use_new) → pip_new_lineups()
  │   ├── validate_country_codes()
  │   ├── create_duckdb_file()        # ensure cache exists
  │   ├── fg_pip() or rg_pip()        # core computation
  │   │   ├── subset_lkup()           # filter by country/year/etc.
  │   │   │   └── return_if_exists()  # check DuckDB cache
  │   │   ├── load_list_refy()        # load .fst survey data
  │   │   ├── fgt_cumsum()            # FGT via cumulative sums (fast)
  │   │   └── fg_remove_duplicates()  # dedup aggregated distributions
  │   ├── treat_cache_and_main()      # merge fresh + cached results
  │   │   └── update_master_file()    # write new results to DuckDB
  │   ├── add_agg_stats()             # aggregate urban/rural → national
  │   ├── add_vars_out_of_pipeline()  # SPL, PG, distribution_type, medians
  │   └── pip_lineups_format_output() # censor, columns, order, dedup
  │
  └── else → pip_old_lineups()        # same structure, old functions
```

### 3.3 Fill-Gap (Lineup) Computation — New Pathway Detail

The fill-gap computation creates poverty estimates for years without surveys by interpolating between available surveys:

```
fg_pip()
  ├── subset_lkup()           # filter refy_lkup → slkup
  │   └── return_if_exists()  # check DuckDB for pre-computed results
  ├── load_list_refy()         # read .fst files for all needed surveys
  ├── format_lfst()            # standardize and flatten survey data
  ├── build_pair_dict()        # encode (cache_id, reporting_level) → integer pairs
  ├── fgt_cumsum()             # THE CORE: cumulative-sum FGT computation
  │   ├── findInterval()       # O(n log m) poverty line lookup
  │   └── collapse aggregation # fsum/fmean over groups
  ├── decode_pairs()           # restore human-readable IDs
  └── fg_remove_duplicates()   # handle multi-survey deduplication
```

### 3.4 Survey-Year Computation

Survey-year estimates use actual survey data directly (no interpolation):

```
rg_pip()
  ├── subset_lkup()        # filter svy_lkup
  ├── load_data_list()      # load .fst survey microdata
  ├── process_dt()          # for each survey:
  │   └── compute_fgt_dt()  # grouped FGT (vectorized over poverty lines)
  └── merge with metadata   # join FGT results back to survey metadata
```

### 3.5 Aggregation Pipeline

Regional and custom aggregations go through `pip_agg()`:

```
pip_agg()
  ├── .check_group_by()     # validate grouping variable
  ├── if (use_new) → pip_grp_new()
  │   ├── fg_pip()           # compute all countries at lineup years
  │   ├── add_agg_stats()
  │   ├── pip_aggregate_by() # weighted means by region
  │   └── estimate_type_var()
  └── else → pip_grp_logic() # old complex imputation logic
```

### 3.6 FGT Core Algorithm (`fgt_cumsum`)

The most performance-critical function. For each survey:

1. Sort observations by welfare value
2. Compute cumulative population weights
3. For each poverty line, use `findInterval()` to find the cutoff
4. Headcount = cumulative population weight at cutoff
5. Poverty gap = headcount − (cumulative welfare share at cutoff) / poverty_line
6. Severity, Watts computed analogously

This runs in O(n log m) per survey (n = observations, m = poverty lines), compared to O(n × m) in the old row-by-row approach.

### 3.7 DuckDB Caching Layer

A two-level caching system:

1. **DuckDB intermediate cache** — stores pre-computed FGT results keyed by (interpolation_id, poverty_line) for fill-gaps or (cache_id, reporting_level, poverty_line) for survey-years.
2. **`memoise`/`cachem` response cache** — full endpoint responses cached on disk when `PIPAPI_APPLY_CACHING=TRUE`.

The DuckDB cache avoids recomputing common poverty lines across requests.

---

## 4. Plumber API Architecture

### 4.1 Server Structure

```
inst/plumber/v1/
├── plumber.R       # Router setup, hooks, error handling
├── endpoints.R     # All endpoint definitions + filters
└── openapi.yaml    # API specification
```

`start_api()` sources `plumber.R`, which builds the router from `endpoints.R`.

### 4.2 Request Processing Pipeline (Filters)

Each request goes through a chain of plumber filters before reaching an endpoint:

```
Request → [ctx]                   # Assign request ID, start timer
        → [validate_version]      # Resolve data version
        → [validate_query_parameters]  # Remove unknown parameters
        → [parse_parameters]      # Type-coerce strings to proper types
        → [check_parameters_values]    # Validate ranges, assign defaults
        → [response_headers]      # Security headers, caching, ETag
        → [endpoint handler]      # Execute computation
        → [preserialize hook]     # Start serialization timer
        → [postserialize hook]    # Log serialization time
        → [postroute hook]        # Log total duration
        → Response
```

### 4.3 Endpoint Inventory

| Endpoint | Method | Purpose |
|---|---|---|
| `/api/v1/pip` | GET | Main poverty/inequality statistics |
| `/api/v1/pip-grp` | GET | Regional/group aggregations |
| `/api/v1/aux` | GET | Auxiliary data tables |
| `/api/v1/versions` | GET | Available data versions |
| `/api/v1/version` | GET | Single version info |
| `/api/v1/valid-params` | GET | Valid parameter values |
| `/api/v1/pip-info` | GET | Package version info |
| `/api/v1/health-check` | GET | Liveness probe |
| `/api/v1/data-timestamp` | GET | Data update timestamp |
| `/api/v1/data-signature` | GET | Data hash |
| `/api/v1/grouped-stats` | GET | Grouped data FGT from user vectors |
| `/api/v1/regression-params` | GET | Lorenz curve regression params |
| `/api/v1/lorenz-curve` | GET | Lorenz curve data points |
| `/api/v1/dir-info` | GET | Server directory listing |
| `/api/v1/gh-hash` | GET | Git commit SHAs |
| `/api/v1/pkgs-version` | GET | Package versions |
| `/api/v1/cache-reset` | GET | Reset response cache |
| `/api/v1/cache-delete` | GET | Delete cache directory |
| `/api/v1/cache-get` | GET | Get cached value |
| `/api/v1/cache-keys` | GET | List cache keys |
| `/api/v1/cache-info` | GET | Cache stats |
| `/api/v1/duckdb-reset` | GET | Reset DuckDB cache |
| **UI endpoints** | | |
| `/api/v1/hp-stacked` | GET | Home page stacked chart |
| `/api/v1/hp-countries` | GET | Home page country charts |
| `/api/v1/pc-charts` | GET | Poverty calculator charts |
| `/api/v1/pc-download` | GET | Poverty calculator CSV download |
| `/api/v1/pc-regional-aggregates` | GET | Regional aggregates for PC |
| `/api/v1/cp-key-indicators` | GET | Country profile key indicators |
| `/api/v1/cp-charts` | GET | Country profile charts |
| `/api/v1/cp-download` | GET | Country profile download |
| `/api/v1/ui_aux` | GET | UI auxiliary data |
| `/api/v1/survey-metadata` | GET | Survey metadata |
| `/api/v1/valid-years` | GET | Valid survey/lineup years |
| `/api/v1/wld-lineup-year` | GET | World lineup year |
| `/api/v1/poverty-lines` | GET | Standard poverty lines |
| `/api/v1/indicators` | GET | Indicators master table |
| `/api/v1/decomposition-vars` | GET | Decomposition variables |
| `/api/v1/citation` | GET | Citation text |

### 4.4 Response Formats

The API supports four response formats via `assign_serializer()`:
- **JSON** (default)
- **CSV**
- **RDS** (R binary)
- **Arrow** (Apache Arrow IPC)

### 4.5 Error Handling

- `safe_endpoint()` wraps heavy-compute endpoints with `tryCatch` and returns structured 500 errors.
- `with_req_timeout()` enforces a configurable request timeout.
- `pr_set_error()` in `plumber.R` provides a global error handler.
- Filters return 400/404 for invalid parameters with structured error messages.

### 4.6 Telemetry

The API logs structured JSON to stderr for every request:

```json
{"type":"access","id":"<request-id>","method":"GET","path":"/api/v1/pip","status":200,"dur_s":0.123456}
{"type":"serialize","id":"<request-id>","path":"/api/v1/pip","dur_s":0.001234}
{"type":"route","id":"<request-id>","method":"GET","path":"/api/v1/pip","status":200,"dur_s":0.122222}
```

---

## 5. Package Infrastructure

### 5.1 Dependencies

**Core computation**: `data.table`, `collapse`, `wbpip`
**I/O**: `fst`, `qs2`, `arrow`, `DBI`, `duckdb`, `readr`
**API**: `plumber`, `jsonlite`
**Caching**: `memoise`, `cachem`
**Utilities**: `fs`, `rlang`, `cli`, `glue`, `joyn`, `yaml`, `purrr`, `assertthat`, `urltools`

### 5.2 Testing

44 test files in `tests/testthat/` covering core functions, API endpoints, UI endpoints, input validation, linting, and snapshot regression tests. Some tests require live data access (marked `-local`).

### 5.3 Exported Functions

66 exported functions (per NAMESPACE), including core computation (`pip`, `pip_agg`, `pip_grp`), UI helpers (`ui_cp_*`, `ui_hp_*`, `ui_pc_*`), utilities, and infrastructure functions.

---

## 6. Key Design Decisions

1. **Dual pathway** — Old/new computation paths coexist, selected by data date. This is a pragmatic transition strategy.
2. **Vectorized FGT** — The new `fgt_cumsum()` avoids row-by-row computation, yielding orders-of-magnitude speedup.
3. **DuckDB as intermediate cache** — Pre-computed FGT results for standard poverty lines avoid redundant computation.
4. **`memoise` response caching** — Full API responses cached to disk with LRU eviction.
5. **Version-aware endpoints** — Every endpoint accepts `version`/`release_version`/`ppp_version` parameters for reproducibility.
6. **Filter chain for validation** — Clean separation of concerns: version resolution → parameter validation → parsing → value checking → headers.
