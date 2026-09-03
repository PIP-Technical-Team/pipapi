---
date: 2026-09-03
title: "Decomposition variables endpoint used a stale auxiliary table name"
category: "bugs"
type: "bug"
language: "R"
tags: [plumber, decomposition-vars, auxiliary-tables, fst]
root-cause: "The endpoint requested decomposition_master.fst, but published data versions provide decomposition.fst."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
red-phase-confirmed: "yes"
expected-behavior-source: "documentation"
test-gap: "missing-test"
---

# Decomposition Variables Endpoint Used a Stale Auxiliary Table Name

## Symptom

`GET /api/v1/decomposition-vars` returned HTTP 500 with an error that said the
FST file could not be opened.

## Expected Behavior Source

Documentation - `inst/plumber/v1/openapi.yaml` defines a successful HTTP 200
response containing an array of decomposition variable definition objects.

## Root Cause

The endpoint passed `decomposition_master` to `get_aux_table()`, which tried to
open `_aux/decomposition_master.fst`. Available data versions contain
`_aux/decomposition.fst` instead.

## Reproduction Test

The test `Decomposition variables endpoint is working` was added to
`tests/testthat/test-plumber-ui.R`. It sends a request to the endpoint and
checks for HTTP 200 and a non-empty list response.

## Test Gap

`missing-test` - no existing Plumber UI test called the `decomposition-vars`
endpoint. Other auxiliary and UI endpoints had integration coverage, but this
stale table name was not exercised.

## Fix

The endpoint now requests the auxiliary table that exists in each data version:

```r
pipapi::get_aux_table(
  data_dir = lkups$versions_paths[[req$argsQuery$version]]$data_root,
  table = "decomposition"
)
```

The OpenAPI auxiliary-table example was also changed from
`decomposition_master` to `decomposition`.

## Lessons Learned

The `missing-test` gap allowed a hard-coded endpoint table name to diverge from
the published auxiliary files. Each dedicated auxiliary-data endpoint must have
an integration test that checks its HTTP status and basic response shape.

## Related

None.
