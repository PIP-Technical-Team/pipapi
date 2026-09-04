---
date: 2026-09-03
title: "Cache keys endpoint failed when disk caching was disabled"
category: "bugs"
type: "bug"
language: "R"
tags: [plumber, cache-keys, cachem, disabled-cache]
root-cause: "The cache-keys endpoint referenced the global cd object even when package startup had not created it."
severity: "P2"
test-written: "yes"
fix-confirmed: "yes"
red-phase-confirmed: "yes"
expected-behavior-source: "user-requirement"
test-gap: "missing-test"
---

# Cache Keys Endpoint Failed When Disk Caching Was Disabled

## Symptom

`GET /api/v1/cache-keys` returned HTTP 500 with the message
`object 'cd' not found` when `PIPAPI_APPLY_CACHING=FALSE`.

## Expected Behavior Source

User requirement - the endpoint must return HTTP 200 and an empty key
collection when disk caching is disabled.

## Root Cause

Package startup creates the global `cd` cache object only when
`PIPAPI_APPLY_CACHING` is `TRUE`. The endpoint called `cd$keys()` without first
checking whether that object existed.

## Reproduction Test

`tests/testthat/test-plumber-cache-keys-disabled.R` starts the API with caching
disabled, requests `/api/v1/cache-keys`, and asserts an HTTP 200 response with
no keys.

## Test Gap

`missing-test` - no existing test called the `cache-keys` endpoint. The test
suite therefore covered neither its enabled nor its disabled-cache behavior.

## Fix

The endpoint now returns an empty character vector before accessing `cd` when
the cache object does not exist:

```r
if (!exists("cd", envir = .GlobalEnv, inherits = FALSE)) {
  return(character())
}
```

The existing `cd$keys()` response is unchanged when caching is enabled.

## Lessons Learned

The `missing-test` gap allowed another cache management endpoint to assume that
the global cache object always exists. Every cache endpoint needs explicit
coverage for disabled startup as well as normal enabled operation.

## Related

- [Cache info endpoint failed when disk caching was disabled](2026-09-03-cache-info-disabled-cache.md)
