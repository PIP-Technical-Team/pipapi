# pipapi — Implementation Roadmap

> **Date**: 2026-04-06
> **Package version**: 1.5.3

This roadmap organizes the improvements from the triage document into sequential phases. Each phase builds on the previous one, prioritizing safety and correctness before performance and polish.

---

## Phase 0: Quick Wins (1-2 days)

**Goal**: Fix bugs and security issues that require minimal code changes.

| ID | Task | Triage | Effort | Risk |
|---|---|---|---|---|
| P1-01 | Fix `sprintf("%n",...)` → `"%d"` in `clear_cache()` | Critical | 5 min | None |
| P1-02 | Move `cd` from `.GlobalEnv` to `.pipapienv` | Critical | 1 hr | Low — update `endpoints.R` references |
| P3-02 | Fix `&` → `&&` in `is_empty()` | Minor | 5 min | None |
| P3-04 | Remove duplicate `%||%` definitions | Minor | 15 min | None |
| P1-06 | Strip absolute paths from `dir-info` endpoint | Critical | 30 min | None |

**Validation**: Run `R CMD check` and existing test suite after each fix.

---

## Phase 1: Security & Dead Code Removal (3-5 days)

**Goal**: Remove security risks and eliminate dead code that creates confusion.

### 1a. Secure Cache Endpoints

| ID | Task | Notes |
|---|---|---|
| P1-05 | Add authentication to `cache-reset`, `cache-delete`, `cache-get`, `cache-keys`, `cache-info` | Mirror the password check from `duckdb-reset` |
| P3-03 | Tighten CSP header | The API serves data, not HTML; CSP can be restrictive |

### 1b. Remove Dead Code

| ID | Task | Notes |
|---|---|---|
| P1-03 + P2-01 | Delete `R/pip_old.R` entirely | Contains duplicate `rg_pip_old()` and duplicate `pip_old()` |
| P2-02 | Remove `**** TO BE REMOVED ****` blocks from `pip_old_lineups()` | Remove `group_by` parameter from this function |
| P3-08 | Deprecate `pip_grp()` export | Add `.Deprecated("pip_agg")` call |

**Validation**: Full test suite + manual smoke test of API endpoints.

---

## Phase 2: Test Coverage (1-2 weeks)

**Goal**: Fill critical test gaps before refactoring.

| ID | File to Test | Key Functions | Priority |
|---|---|---|---|
| P2-09 | `R/duckdb_func.R` | `return_if_exists()`, `update_master_file()`, `connect_with_retry()` | High |
| P2-09 | `R/infer_poverty_line.R` | `infer_poverty_line()` | High |
| P2-09 | `R/pip_grp_new.R` | `pip_grp_new()`, `get_country_code_subset()` | High |
| P2-09 | `R/copy_functions.R` | `pipgd_params()`, `pipgd_select_lorenz()`, `pipgd_lorenz_curve()` | Medium |
| P2-09 | `R/fgt_cumsum.R` (edge cases) | Empty surveys, zero population, single observation, single poverty line | Medium |

**Test patterns**:
- Use `withr::local_tempdir()` for DuckDB tests
- Create small synthetic data.tables for FGT edge cases
- Test `infer_poverty_line()` with known distributional quantiles

**Validation**: Achieve ≥80% line coverage on the listed files.

---

## Phase 3: Refactor Core Infrastructure (2-3 weeks)

**Goal**: Improve maintainability of the most complex code paths.

### 3a. Break Down `create_lkups()`

| Step | New Function | Lines Extracted |
|---|---|---|
| 1 | `load_aux_files(data_dir)` | ~30 lines |
| 2 | `build_svy_lkup(data_dir, paths_ids, countries)` | ~20 lines |
| 3 | `build_ref_lkup(data_dir, paths_ids, countries)` | ~30 lines |
| 4 | `build_refy_lkup(data_dir, ref_lkup, country_list, pop, ...)` | ~120 lines |
| 5 | `build_interpolation_list(ref_lkup)` | ~30 lines |
| 6 | `compute_cache_hashes(...)` | ~50 lines |

### 3b. Simplify `return_if_exists()`

| Step | New Function | Purpose |
|---|---|---|
| 1 | `check_cache_available(slkup, cache_file_path, fill_gaps)` | Returns cached data.table or NULL |
| 2 | `partition_lkup_by_cache(slkup, cached, key_vars)` | Splits into cached vs needs-computation |
| 3 | `adjust_povline_from_cache(lk_not_ms)` | Returns reduced poverty line vector |

### 3c. Remove `get_caller_names()` pattern

Replace call-stack inspection in `select_years()` and `subset_ctry_years()` with an explicit parameter:

```r
# Before
select_years <- function(...) {
  callers <- get_caller_names()
  if (any(grepl("pip_grp", callers))) { ... }
}

# After
select_years <- function(..., context = c("pip", "agg")) { ... }
```

### 3d. Standardize Join Style

Audit all joins and standardize per the collapse > data.table > tidyverse hierarchy:
- `collapse::join()` for simple equi-joins
- `data.table` `X[Y, on=]` for complex joins (rolling, non-equi)
- Phase out `joyn::joyn()` where `collapse::join()` suffices

**Validation**: Full test suite at each step. No behavior changes — pure refactoring.

---

## Phase 4: Code Consolidation (1 week)

**Goal**: Eliminate remaining duplication and technical debt.

| ID | Task | Notes |
|---|---|---|
| P2-06 | Replace `fillin_list()` | Use explicit parameter passing |
| P2-08 | Externalize hardcoded `UKR` exclusion | Move to aux data or config file |
| P2-11 | Resolve `copy_functions.R` | Either use upstream packages or document divergences |
| P3-01 | Standardize error handling | `cli::cli_abort()` everywhere |
| P3-05 | Resolve or document all TODOs | Either fix or convert to GitHub issues |
| P3-06 | Simplify `unnest_dt_longer()` | Plain data.table syntax |
| P3-12 | Standardize on `|>` pipe | Replace any remaining `%>%` |

---

## Phase 5: API Improvements (1-2 weeks)

**Goal**: Improve the API layer's robustness and documentation.

### 5a. Documentation

| Task | Notes |
|---|---|
| P3-09 | Complete OpenAPI spec | Add all endpoints, response schemas, error definitions |
| P4-06 | Add response schemas | Enable auto-generated SDKs |
| P3-07 | Add `@family` tags to roxygen2 | Group related functions |
| P4-11 | Write pkgdown articles | Setup guide, API usage, extension guide |

### 5b. Robustness

| Task | Notes |
|---|---|
| P4-04 | Deep health check | Verify data dirs, lkups, DuckDB connectivity |
| P4-01 | Rate limiting | Via plumber filter or reverse proxy |
| P4-09 | Request correlation IDs | Propagate IDs through computation |
| P1-04 | Fix `filter_for_aggregate_by()` | Correct the selection algorithm |

---

## Phase 6: Performance & Infrastructure (2-4 weeks, as capacity allows)

**Goal**: Long-term infrastructure improvements.

| Task | Notes | Effort |
|---|---|---|
| P2-05 | DuckDB connection pooling | Single reader + write mutex | Medium |
| P4-05 | Evaluate Parquet migration | Benchmark vs fst, assess compatibility | Large |
| P4-07 | Dockerize the API | Dockerfile + docker-compose | Medium |
| P4-08 | Abstract caching backend | Pluggable disk/Redis/memcached | Medium |
| P4-10 | Parallel request handling | `future`-based workers or multi-process | Large |
| P3-10 | Selective `@importFrom` | Reduce namespace surface area | Large (incremental) |

---

## Summary Timeline

```
Week 1:       Phase 0 (Quick Wins) + Phase 1 (Security + Dead Code)
Week 2-3:     Phase 2 (Test Coverage)
Week 3-5:     Phase 3 (Refactor Core)
Week 5-6:     Phase 4 (Consolidation)
Week 6-8:     Phase 5 (API Improvements)
Week 8+:      Phase 6 (Performance & Infrastructure) — ongoing
```

**Key principle**: Each phase should leave the package in a better state with all tests passing. Never skip Phase 2 (testing) before Phase 3 (refactoring).

---

## Dependencies Between Phases

```
Phase 0 ─→ Phase 1 ─→ Phase 2 ─→ Phase 3 ─→ Phase 4
                                     │
                                     └─→ Phase 5 (can start in parallel)
                                     
Phase 6 can start after Phase 3 is stable.
```
