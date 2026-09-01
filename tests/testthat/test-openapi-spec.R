test_that("openapi.yaml documents only public mounted routes", {
  skip_if_not_installed("yaml")

  spec_path <- system.file("plumber", "v1", "openapi.yaml", package = "pipapi")
  endpoint_path <- system.file(
    "plumber",
    "v1",
    "endpoints.R",
    package = "pipapi"
  )

  # Fall back to the source tree when running during development (devtools::load_all)
  if (!nzchar(spec_path)) {
    pkg_root <- rprojroot::find_package_root_file()
    spec_path <- file.path(pkg_root, "inst", "plumber", "v1", "openapi.yaml")
    endpoint_path <- file.path(pkg_root, "inst", "plumber", "v1", "endpoints.R")
  }

  skip_if(!file.exists(spec_path), "openapi.yaml not found")
  skip_if(!file.exists(endpoint_path), "endpoints.R not found")

  # --- paths from openapi.yaml spec ---
  spec <- yaml::read_yaml(spec_path)
  spec_paths <- names(spec$paths)

  # --- paths from endpoints.R @get / @post annotations ---
  endpoint_lines <- readLines(endpoint_path, warn = FALSE)
  route_pattern <- "^#\\*\\s+@(get|post|put|delete|patch)\\s+(/[^[:space:]]+)"
  route_lines <- grep(route_pattern, endpoint_lines, value = TRUE)
  router_paths <- regmatches(
    route_lines,
    regexpr("/api/v1/[^[:space:]]+", route_lines)
  )

  # Paths documented in YAML but not found in endpoints.R
  missing_from_router <- setdiff(spec_paths, router_paths)

  # Internal routes intentionally omitted from the public specification
  missing_from_spec <- setdiff(router_paths, spec_paths)
  internal_routes <- c(
    "/api/v1/cache-reset",
    "/api/v1/cache-delete",
    "/api/v1/cache-get",
    "/api/v1/cache-keys",
    "/api/v1/cache-info",
    "/api/v1/duckdb-reset",
    "/api/v1/dir-info",
    "/api/v1/gh-hash",
    "/api/v1/pkgs-version"
  )

  expect_equal(
    length(missing_from_router),
    0L,
    label = paste(
      "Paths in openapi.yaml but not in endpoints.R:",
      paste(missing_from_router, collapse = ", ")
    )
  )
  expect_setequal(
    missing_from_spec,
    internal_routes
  )
})

test_that("openapi.yaml tags are declared and assigned", {
  skip_if_not_installed("yaml")

  spec_path <- system.file("plumber", "v1", "openapi.yaml", package = "pipapi")
  if (!nzchar(spec_path)) {
    pkg_root <- rprojroot::find_package_root_file()
    spec_path <- file.path(pkg_root, "inst", "plumber", "v1", "openapi.yaml")
  }

  skip_if(!file.exists(spec_path), "openapi.yaml not found")

  spec <- yaml::read_yaml(spec_path)
  declared_tags <- vapply(spec$tags, `[[`, character(1), "name")
  operation_tags <- lapply(spec$paths, function(path) path$get$tags)

  expect_equal(anyDuplicated(declared_tags), 0L)
  expect_true(all(lengths(operation_tags) > 0L))
  expect_setequal(unique(unlist(operation_tags)), declared_tags)
})
