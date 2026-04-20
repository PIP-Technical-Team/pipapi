test_that("openapi.yaml paths match mounted routes in endpoints.R", {
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

  # Paths in endpoints.R not documented in YAML
  missing_from_spec <- setdiff(router_paths, spec_paths)

  expect_equal(
    length(missing_from_router),
    0L,
    label = paste(
      "Paths in openapi.yaml but not in endpoints.R:",
      paste(missing_from_router, collapse = ", ")
    )
  )
  expect_equal(
    length(missing_from_spec),
    0L,
    label = paste(
      "Paths in endpoints.R but not in openapi.yaml:",
      paste(missing_from_spec, collapse = ", ")
    )
  )
})
