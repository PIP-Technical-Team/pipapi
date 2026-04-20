---
date: 2026-04-20
title: "Sys.getenv() empty-string auth bypass: \"\" != \"\" is FALSE"
category: "bugs"
language: "R"
tags: [auth, security, Sys.getenv, env-vars, duckdb, cache]
root-cause: "Sys.getenv() returns \"\" for unset vars; comparing two empty strings with != returns FALSE, so the guard never fires."
severity: "P0"
---

# `Sys.getenv()` Empty-String Auth Bypass

## Problem

`reset_cache()` and `delete_cache()` had a key-validation guard:

```r
if (pass != Sys.getenv('PIP_CACHE_SERVER_KEY')) {
  rlang::abort("Either key not set or incorrect key!")
}
```

When **both** `PIP_CACHE_LOCAL_KEY` and `PIP_CACHE_SERVER_KEY` are unset, `pass`
defaults to `Sys.getenv('PIP_CACHE_LOCAL_KEY')` → `""`, and
`Sys.getenv('PIP_CACHE_SERVER_KEY')` → `""`. The condition `"" != ""` is
`FALSE`, so the guard is silently skipped and the destructive operation proceeds
without any authentication.

## Root Cause

`Sys.getenv()` returns `""` (not `NA`) for unset variables unless `unset` is
explicitly specified. A guard written as `value != Sys.getenv(key)` is therefore
vacuously TRUE when the env var is set *and the value matches*, and vacuously
FALSE when both sides are empty — the opposite of the intended behaviour.

## Solution

Extract an internal helper that uses `unset = ""` and tests `nzchar()`:

```r
.check_cache_auth <- function(pass) {
  server_key <- Sys.getenv("PIP_CACHE_SERVER_KEY", unset = "")
  if (
    !nzchar(Sys.getenv("PIP_CACHE_LOCAL_KEY", unset = "")) ||
      !nzchar(server_key)
  ) {
    cli::cli_abort(
      "Cache key env var(s) not set \\
      ({.envvar PIP_CACHE_LOCAL_KEY} / {.envvar PIP_CACHE_SERVER_KEY})."
    )
  }
  if (pass != server_key) {
    cli::cli_abort("Cache key mismatch: supplied key does not match server key.")
  }
  invisible(TRUE)
}
```

Key decisions:
- Use `nzchar()` rather than `!is.na() && nchar() > 0` — cleaner and handles the `""` case directly.
- Separate the "vars not set" error from the "key mismatch" error so callers get actionable messages.
- Use `cli::cli_abort()` with `{.envvar}` markup for consistent error formatting across the codebase.

## Prevention

**When writing guards against `Sys.getenv()` output**:

```r
# ❌ WRONG — passes silently when both vars are unset
if (pass != Sys.getenv("KEY")) { ... }

# ✅ CORRECT — fails explicitly when var is unset
key <- Sys.getenv("KEY", unset = "")
if (!nzchar(key)) cli::cli_abort("KEY env var not set.")
if (pass != key)  cli::cli_abort("Key mismatch.")
```

Always guard with `nzchar()` **before** comparing values.  
Never copy the env var read directly into the comparison without first checking it is non-empty.

## Related

- See `R/duckdb_func.R` `.check_cache_auth()` for the canonical implementation.
- Related testing pattern: [2026-04-20-withr-local-envvar-both-keys-required.md](../testing-patterns/2026-04-20-withr-local-envvar-both-keys-required.md)
