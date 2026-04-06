# pipapi Review Findings

> **Date**: 2026-04-06 | **Package version**: 1.5.3 | **Reviewer**: Copilot

This folder contains a thorough technical review of the `pipapi` R package and its Plumber API implementation.

---

## Contents

| File | Description |
|---|---|
| [01-architecture.md](01-architecture.md) | **Package & API architecture**. How the package works: data layout, the `lkup` object, computation pipeline (old vs new pathways), FGT algorithm, DuckDB caching, Plumber filter chain, full endpoint inventory, response formats, error handling, and telemetry. Start here. |
| [02-improvement-triage.md](02-improvement-triage.md) | **Improvement triage**. All findings organized by severity: 6 Critical (P1), 11 Major (P2), 10 Minor (P3), 12 Nice-to-have (P4). Each item includes the affected file(s), a description of the issue, and a suggested fix. |
| [03-roadmap.md](03-roadmap.md) | **Implementation roadmap**. A phased plan (0–6) with time estimates, dependencies, and validation steps. Ranges from quick wins (day 1) to long-term infrastructure improvements (week 8+). |

---

## Reading Order

1. **01-architecture.md** — Read this first to understand how the package works. Sections 3 (Computation Pipeline) and 4 (Plumber API Architecture) are the most important.

2. **02-improvement-triage.md** — Skim the Critical (P1) items, then read Major (P2) items. Minor and Nice-to-have can be referenced as needed.

3. **03-roadmap.md** — Use this to plan implementation. Phase 0 can be done immediately.

---

## Key Findings at a Glance

### Strengths

- The new FGT computation (`fgt_cumsum()`) is well-optimized and represents a major performance improvement.
- The dual old/new pathway architecture allows a gradual transition.
- The Plumber filter chain provides clean separation of validation concerns.
- `safe_endpoint()` and `with_req_timeout()` provide solid API-level error handling.
- Good test coverage (44 test files) for core functionality.
- The `validate_lkup()` design is clean and extensible.

### Top Concerns

1. **`sprintf` bug** in `clear_cache()` — will crash at runtime (P1-01).
2. **Global environment pollution** — `assign("cd", ..., envir = .GlobalEnv)` violates R packaging rules (P1-02).
3. **Unauthenticated destructive endpoints** — `cache-delete` can wipe the cache directory (P1-05).
4. **Server path leakage** — `dir-info` exposes filesystem paths (P1-06).
5. **Duplicate function definitions** — `rg_pip_old()` defined in two files (P1-03).
6. **`create_lkups()` is 500+ lines** — hard to maintain and test (P2-03).
7. **Missing tests** for DuckDB caching, poverty line inference, and new aggregation path (P2-09).

---

## How to Use These Findings

- **For immediate action**: Start with Phase 0 in the roadmap — the 5 quick wins take about 2 hours total and fix the most critical bugs.
- **For sprint planning**: Use the triage document's P1/P2 items to prioritize the backlog.
- **For architecture decisions**: The architecture document provides the context needed to evaluate trade-offs.
- **For new team members**: The architecture document serves as an onboarding guide to the codebase.
