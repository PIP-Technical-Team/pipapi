---
date: 2026-04-20
title: "Using 'here' package in tests without declaring it in DESCRIPTION Suggests"
category: "build-errors"
language: "R"
tags: [here, rprojroot, DESCRIPTION, Suggests, testthat, CI, package-check]
root-cause: "here::here() used in a fallback path inside a test; 'here' not listed in DESCRIPTION Suggests — works locally (package coincidentally installed) but errors on clean CI runners."
severity: "P1"
---

# `here` Package Used in Tests Without `DESCRIPTION` Suggests Entry

## Problem

`test-openapi-spec.R` used `here::here()` to locate source files when the
package was not yet installed:

```r
if (!nzchar(spec_path)) {
  spec_path     <- here::here("inst", "plumber", "v1", "openapi.yaml")
  endpoint_path <- here::here("inst", "plumber", "v1", "endpoints.R")
}
```

`here` was not listed in `DESCRIPTION Suggests`. On a developer machine where
`here` happens to be installed library-wide, this is invisible. On a clean CI
runner that only installs packages declared in `DESCRIPTION`, `library(here)`
errors with *"there is no package called 'here'"* and the entire test file
fails to load.

## Root Cause

`rprojroot` (which `here` wraps) **was** listed in Suggests. The test author
reached for the more familiar `here::here()` API without checking whether `here`
itself was declared.

## Solution

Use `rprojroot` directly — it is already a declared Suggest and provides the
same capability:

```r
# ❌ undeclared dependency
spec_path <- here::here("inst", "plumber", "v1", "openapi.yaml")

# ✅ uses declared rprojroot
pkg_root  <- rprojroot::find_package_root_file()
spec_path <- file.path(pkg_root, "inst", "plumber", "v1", "openapi.yaml")
```

Alternatively, add `here` to `DESCRIPTION Suggests` and be consistent.

## Prevention

**Rule**: Before using `pkg::fun()` in a test file, verify `pkg` is listed in
`DESCRIPTION Imports` or `DESCRIPTION Suggests`. The test runner on CI will only
have those packages available.

**Quick check**:
```r
desc::desc_get_deps()  # lists all declared dependencies
```

**Prefer `rprojroot` over `here` in package tests** — `rprojroot` is purpose-built
for finding package roots in R packages and is more likely to already be declared.

The canonical test pattern for finding source-tree files:

```r
# Inside devtools::load_all() context, system.file() returns ""
spec_path <- system.file("plumber", "v1", "openapi.yaml", package = "mypkg")
if (!nzchar(spec_path)) {
  pkg_root  <- rprojroot::find_package_root_file()
  spec_path <- file.path(pkg_root, "inst", "plumber", "v1", "openapi.yaml")
}
skip_if(!file.exists(spec_path), "File not found")
```

## Related

- `tests/testthat/test-openapi-spec.R` — fixed instance
- `DESCRIPTION` L33 Suggests — `rprojroot` is the declared alternative
