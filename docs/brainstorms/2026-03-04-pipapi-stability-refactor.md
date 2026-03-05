---
date: 2026-03-04
title: "pipapi core pipeline stability and refactoring"
status: decided
chosen-approach: "Stabilize, Prune, Then Restructure (Hybrid)"
tags: [refactoring, stability, technical-debt, core-pipeline]
---

# pipapi Core Pipeline Stability & Refactoring

## Context

The `pipapi` R package has accumulated significant technical debt over years of
development. The codebase has ~100+ functions, 11 old/new function pairs
coexisting, massive code duplication, a god object (`lkup`), and tightly coupled
functions that make debugging cascading failures very difficult. Small changes in
data or code trigger unpredictable downstream errors.

## Requirements

- Focus on the **new pathway only** (old pathway left as-is, functional but frozen)
- Focus on **core pipeline R functions** (not plumber endpoints, not UI functions)
- Scope: `pip` → `fg_pip`/`rg_pip` → `compute_fgt` → `pip_grp_new` → aggregation, plus supporting utilities
- Must be done in small, safe, independently mergeable phases
- Two-layer plan: a master roadmap + detailed phase plans
- Nothing should break between phases

## Out of Scope (Future Work)

- `lkup` object redesign (schema, validation, possible R6/S3 class)
- DuckDB caching layer review (`duckdb_func.R`)
- UI endpoint functions (`ui_country_profile.R`, `ui_home_page.R`, `ui_poverty_indicators.R`, `ui_miscellaneous.R`)
- Plumber endpoint hardening (`plumber.R`, `utils-plumber.R`, `start_api.R`)
- Old pathway deprecation/removal strategy
- CI/CD pipeline improvements

## Codebase Findings

- **~100+ functions** across 20 R files
- **11 old/new function pairs** (e.g., `fg_pip`/`fg_pip_old`, `rg_pip`/`rg_pip_old`)
- **Massive duplication**: `pip_new_lineups` and `pip_old_lineups` share ~150+ lines
- **God object**: `lkup` passed everywhere, ~20 fields, no schema/validation
- **Dead code**: commented-out blocks, `**** TO BE REMOVED ****` markers, debug prints, duplicate function definitions
- **Mega-files**: `utils.R` (~900 lines, 30+ functions), `create_lkups.R` (~570 lines)
- **Deep nesting**: `pip_grp_logic` ~180 lines, 3-4 levels of if/else + nested for loops

## Approaches Considered

### Approach 1: Bottom-Up Cleanup (Leaves First)
Clean leaf functions first, work upward to `pip()`. Independently testable but
slow to deliver value at the pipeline level. **Not recommended** — too slow.

### Approach 2: Pipeline-Down Decomposition (Top First)
Start at `pip()`/`pip_new_lineups`, extract shared helpers, clean downward.
Immediate deduplication wins but risky without test coverage at the top level.
**Not recommended** — too risky.

### Approach 3: Stabilize, Prune, Then Restructure (Hybrid)
Three macro-stages: (A) safety net + dead code removal, (B) split and
deduplicate, (C) validation and proper tests. **Recommended** — best balance
of safety, incremental progress, and manageable scope.

## Decision

**Approach 3: Stabilize, Prune, Then Restructure** was chosen because:
- Lowest risk: dead code removal and file splitting are zero-logic-change operations
- Snapshot tests protect from day one
- Each phase is small and independently mergeable
- Addresses the root cause (tight coupling, duplication) systematically

## Master Roadmap

### Stage A — Stabilize (make it safe to change)
- **A1**: Snapshot baseline tests for new pathway (requires data folder)
- **A2**: Remove dead code (debug prints, commented-out blocks, `TO BE REMOVED` markers, unused functions)

### Stage B — Restructure (improve the code)
- **B1**: Split `utils.R` into focused files
- **B2**: Deduplicate `pip_new_lineups` / `pip_old_lineups` shared logic
- **B3**: Simplify `compute_fgt_new.R` (consolidate overlapping FGT approaches)

### Stage C — Harden (lock it down)
- **C1**: Add input validation to key functions (especially `lkup` field access)
- **C2**: Write proper unit tests for the cleaned-up new pathway
- **C3**: Add roxygen2 documentation for all new-pathway functions

## Next Steps

1. Write snapshot generation script for A1 (user runs locally with data)
2. Begin A2 (dead code removal) in parallel — safe without snapshot tests
3. Create detailed phase plans via `/cg-plan` for each phase
