---
date: 2026-04-17
title: "OpenAPI Documentation Overhaul"
status: decided
scope: "Standard"
chosen-approach: "Full rewrite with response schemas"
tags: [documentation, openapi, api, swagger]
---

# OpenAPI Documentation Overhaul

## Context

The existing `openapi.yaml` documented only 12 endpoints with minimal
descriptions, no response schemas, no tags, and placeholder examples.
The API actually has 31+ endpoints covering core poverty statistics,
grouped-data computations, metadata, system diagnostics, homepage feeds,
poverty calculator, and country profiles.

## Requirements

- Document all exported endpoints (31 total).
- Exclude 7 admin/ops endpoints: `cache-reset`, `cache-delete`,
  `cache-get`, `cache-keys`, `cache-info`, `duckdb-reset`, `dir-info`.
- Include full response schemas with field names, types, and descriptions.
- Organize endpoints into logical tag groups for Swagger UI navigation.
- Use `$ref` components for reusable parameters and schemas to keep DRY.
- Add realistic examples for parameters and response fields.
- Improve descriptions explaining key behaviours (e.g. `fill_gaps` effects).

## Approaches Considered

### Approach 1: Descriptions + examples only (lightweight)

Improve summaries, parameter descriptions, and add examples, but keep
responses as simple `200: successful message`.

**Pros:** Quick, low risk.
**Cons:** Swagger UI still not useful for understanding response shape.
**Effort:** Small.

### Approach 2: Full response schemas (comprehensive)

Complete rewrite with tags, reusable components, full response schemas,
error responses, and realistic examples for all 31 endpoints.

**Pros:** Makes Swagger UI genuinely useful for API consumers. Self-
documenting API. Enables client code generation.
**Cons:** Larger initial effort. Schemas need updating when response
columns change.
**Effort:** Medium.

### Approach 3: Auto-generate from plumber annotations

Use plumber's auto-generated spec and enhance it rather than maintaining
a hand-written YAML.

**Pros:** Stays in sync with code automatically.
**Cons:** Plumber auto-generation doesn't capture response schemas.
Limited control over descriptions and examples.
**Effort:** Medium, but limited ceiling.

## Decision

**Approach 2: Full response schemas.** The comprehensive spec makes the
Swagger UI genuinely useful for external and internal consumers, and the
schema documentation reduces support burden. The maintenance cost is
acceptable given that response columns rarely change.

## Next Steps

- Implemented in this session: complete `openapi.yaml` rewrite.
- 31 endpoints documented, 17 reusable schemas, 12 reusable parameters.
- 8 tag groups: Poverty Statistics, Grouped Data, Metadata, System,
  Homepage, Poverty Calculator, Country Profiles, UI Miscellaneous.
- Future: validate against live API responses; update when columns change.
