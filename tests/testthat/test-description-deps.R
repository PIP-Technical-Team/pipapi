# test-description-deps.R
#
# Validates that DESCRIPTION Imports/Suggests match actual package usage in R/
# source files. Packages used via `pkg::fun()` in R/ must be in Imports.
# Packages in Imports must be used somewhere in R/.
#
# These tests catch two failure modes:
#   1. Missing import: a package used in R/ is absent from DESCRIPTION Imports.
#   2. Ghost import: a package in DESCRIPTION Imports is never used in R/.

local({
  # ---- helpers ------------------------------------------------------------

  # Parse DESCRIPTION Imports/Suggests into character vectors
  desc_field <- function(field) {
    desc <- read.dcf(
      system.file("DESCRIPTION", package = "pipapi"),
      fields = field
    )[1L, 1L]
    if (is.na(desc)) {
      return(character(0L))
    }
    pkgs <- strsplit(desc, "[,\n]+")[[1L]]
    pkgs <- trimws(pkgs)
    pkgs <- gsub("\\s*\\(.*\\)", "", pkgs) # strip version constraints
    pkgs[nzchar(pkgs)]
  }

  # Scan R/ and inst/plumber/ source files for unique package names used via
  # `pkg::fun()`. Comment lines are stripped before scanning to avoid false
  # positives. @importFrom declarations are covered by reading the NAMESPACE.
  used_in_source <- function() {
    # Locate source directories (works under devtools::test() and R CMD check)
    pkg_root <- tryCatch(
      rprojroot::find_package_root_file("."),
      error = function(e) getwd()
    )
    scan_dirs <- c(
      file.path(pkg_root, "R"),
      file.path(pkg_root, "inst", "plumber")
    )
    r_files <- unlist(lapply(
      scan_dirs[dir.exists(scan_dirs)],
      list.files,
      pattern = "\\.R$",
      full.names = TRUE,
      recursive = TRUE
    ))

    src_lines <- unlist(lapply(r_files, readLines, warn = FALSE))
    # Strip full-line comments and inline comments to avoid false positives
    # (e.g. `# tidyr::unnest_longer` should not count as a tidyr usage).
    # NOTE: gsub("#.*$") may mangle string literals containing "#" (e.g. hex
    # colours, URL anchors). This is acceptable for this package's R source
    # which contains no such cases.
    src_lines <- src_lines[!grepl("^\\s*#", src_lines)]
    src_lines <- gsub("#.*$", "", src_lines)

    # Collect pkg:: references
    m_colons <- regmatches(
      src_lines,
      gregexpr("[A-Za-z][A-Za-z0-9.]+(?=::)", src_lines, perl = TRUE)
    )
    # Read the compiled NAMESPACE for importFrom() entries — this is the
    # authoritative source for packages imported without `::` (e.g. via
    # @importFrom in roxygen blocks).
    ns_file <- file.path(pkg_root, "NAMESPACE")
    ns_lines <- if (file.exists(ns_file)) {
      readLines(ns_file, warn = FALSE)
    } else {
      character(0L)
    }
    m_ns <- regmatches(
      ns_lines,
      gregexpr("(?<=importFrom\\()[A-Za-z][A-Za-z0-9.]+", ns_lines, perl = TRUE)
    )

    pkgs <- unique(c(unlist(m_colons), unlist(m_ns)))
    # exclude self-references and base/recommended packages always available
    exclude <- c(
      "pipapi",
      "base",
      "stats",
      "utils",
      "methods",
      "graphics",
      "grDevices",
      "tools",
      "parallel"
    )
    setdiff(pkgs, exclude)
  }

  # ---- data ---------------------------------------------------------------
  imports <- desc_field("Imports")
  suggests <- desc_field("Suggests")
  used <- used_in_source()

  # ---- test 1: no package in Imports should be completely absent from R/ ------
  test_that("all Imports are actually used in R/ source", {
    # Packages that are ALLOWED to be import-only (used implicitly or by plumber)
    # tools is in base, parallel/methods/stats/utils excluded above
    ghost <- setdiff(imports, c(used, "methods", "parallel"))
    expect_equal(
      ghost,
      character(0L),
      info = paste(
        "Packages in Imports but not used via `::` in R/:",
        paste(ghost, collapse = ", ")
      )
    )
  })

  # ---- test 2: packages used via `pkg::` in R/ must be in Imports ----------
  test_that("all packages used in R/ source are listed in Imports", {
    missing_imports <- setdiff(
      used,
      c(imports, suggests, "methods", "parallel")
    )
    expect_equal(
      missing_imports,
      character(0L),
      info = paste(
        "Packages used in R/ but absent from DESCRIPTION Imports:",
        paste(missing_imports, collapse = ", ")
      )
    )
  })

  # ---- test 3: qs2 specifically must be in Imports (regression guard) ------
  test_that("qs2 is listed in DESCRIPTION Imports", {
    expect_true(
      "qs2" %in% imports,
      info = "qs2 is used in R/zzz.R (qs2::qs_read, qs2::qs_save) but is not in DESCRIPTION Imports"
    )
  })

  # ---- test 4: qs must NOT be in Imports (it was replaced by qs2) ----------
  test_that("qs is not listed in DESCRIPTION Imports (replaced by qs2)", {
    expect_false(
      "qs" %in% imports,
      info = "qs is in Imports but is never used — it was replaced by qs2"
    )
  })

  # ---- test 5: assertthat must be in Imports (used in production R/) -------
  test_that("assertthat is in Imports (used in R/utils-aux.R)", {
    expect_true(
      "assertthat" %in% imports,
      info = "assertthat::assert_that() is called in R/utils-aux.R but is only in Suggests"
    )
  })
})
