---
date: 2026-04-20
title: "withr::local_envvar must set ALL env vars the implementation reads independently"
category: "testing-patterns"
language: "R"
tags: [withr, local_envvar, env-vars, auth, testthat, CI]
root-cause: "A test only set the env var matched by the pass argument, not the one checked independently by the auth helper — causing CI failures that never appeared locally."
severity: "P0"
---

# `withr::local_envvar` Must Set All Env Vars Read by the Implementation

## Problem

After extracting `.check_cache_auth()` as an internal helper, the
`delete_cache` "no-file" test was simplified to only set:

```r
withr::local_envvar(PIP_CACHE_SERVER_KEY = "test-key")
```

The reasoning was: *the test supplies `pass` directly, so `PIP_CACHE_LOCAL_KEY`
is unused.* But `.check_cache_auth()` reads **both** env vars independently:

```r
.check_cache_auth <- function(pass) {
  # checks PIP_CACHE_LOCAL_KEY directly from environment:
  if (!nzchar(Sys.getenv("PIP_CACHE_LOCAL_KEY", unset = ""))) { ... }
  # then compares pass against PIP_CACHE_SERVER_KEY:
  if (pass != Sys.getenv("PIP_CACHE_SERVER_KEY")) { ... }
}
```

On a developer machine where `PIP_CACHE_LOCAL_KEY` happens to be set in
`.Renviron`, the test passes. On a clean CI runner (no `.Renviron`), the
auth helper aborts before reaching the test assertion, throwing an unexpected
error.

## Root Cause

The test author reasoned about which env var the *function argument* traced to,
not which env vars the **implementation** reads. When a helper checks env vars
by side-channel (not through the public function argument), all such vars must
be controlled in tests regardless of how the public argument is passed.

## Solution

Always set **every** env var that the function-under-test reads, even if the
test supplies the derived value directly via an argument:

```r
# ❌ WRONG — PIP_CACHE_LOCAL_KEY is read by the auth helper independently
test_that("delete_cache does not error when cache file does not exist", {
  withr::local_envvar(PIP_CACHE_SERVER_KEY = "test-key")
  expect_no_error(delete_cache(pass = "test-key", lkup = lkup_mock))
})

# ✅ CORRECT — both vars set so the helper's independent reads succeed
test_that("delete_cache does not error when cache file does not exist", {
  withr::local_envvar(
    PIP_CACHE_LOCAL_KEY  = "test-key",
    PIP_CACHE_SERVER_KEY = "test-key"
  )
  expect_no_error(delete_cache(pass = "test-key", lkup = lkup_mock))
})
```

## Prevention

**Rule**: When writing tests for a function that calls an internal auth/validation
helper, read the helper's source and enumerate *all* `Sys.getenv()` calls it
makes. Set every one of them in `withr::local_envvar()`.

**Rule**: Auth-failure tests (`expect_error(...)`) should unset *only* the specific
var being tested; happy-path tests should set *all* required vars.

**Detection heuristic**: If a test passes locally but fails in CI on an
`expect_no_error()` / `expect_no_warning()` for an auth-adjacent function — the
first thing to check is whether all env vars read by the implementation are set
in `local_envvar`.

## Related

- Auth bypass root cause: [2026-04-20-getenv-empty-string-auth-bypass.md](../bugs/2026-04-20-getenv-empty-string-auth-bypass.md)
- DuckDB testing patterns: [2026-04-16-testing-duckdb-functions.md](2026-04-16-testing-duckdb-functions.md)
