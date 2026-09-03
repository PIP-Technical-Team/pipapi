---
date: 2026-09-03
title: "Cache info endpoint failed when disk caching was disabled"
category: "bugs"
type: "bug"
language: "R"
tags: [plumber, cache-info, cachem, disabled-cache]
root-cause: "The cache-info endpoint referenced the global cd object even when package startup had not created it."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
red-phase-confirmed: "yes"
expected-behavior-source: "user-requirement"
test-gap: "edge-case-gap"
---

# Cache Info Endpoint Failed When Disk Caching Was Disabled

## Symptom

`GET /api/v1/cache-info` returned HTTP 500 with the message
`object 'cd' not found` when `PIPAPI_APPLY_CACHING=FALSE`.

## Expected Behavior Source

User requirement - the endpoint must return HTTP 200 when disk caching is
disabled, not an internal server error.

## Root Cause

Package startup creates the global `cd` cache object only when
`PIPAPI_APPLY_CACHING` is `TRUE`. The endpoint accessed `cd` without first
checking whether that object existed.

## Reproduction Test

`tests/testthat/test-plumber-cache-disabled.R` starts the API with caching
disabled, requests `/api/v1/cache-info`, and asserts an HTTP 200 response.

## Test Gap

`edge-case-gap` - the existing caching test covered only the enabled-cache
path. It did not exercise package startup with caching disabled, so the missing
global cache object was not detected.

## Fix

The endpoint now returns explicit disabled-cache information before accessing
`cd`:

```r
if (!exists("cd", envir = .GlobalEnv, inherits = FALSE)) {
  return(list(enabled = FALSE, n_items = 0))
}
```

The existing cache information response is unchanged when `cd` exists.

## Lessons Learned

The `edge-case-gap` shows that configuration-dependent endpoints need tests for
each supported configuration state. Tests for cache management must cover both
enabled and disabled startup paths instead of assuming the global cache object
always exists.

## Related

None.
