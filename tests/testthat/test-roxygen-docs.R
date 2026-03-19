# tests/testthat/test-roxygen-docs.R
#
# Source-level guards that catch malformed roxygen2 documentation before
# devtools::document() is run.  These tests fail when a documentation issue is
# present and pass once it is fixed.
#
# rprojroot locates the package root regardless of working directory, so these
# tests work both via devtools::test() and when run interactively.
# The tryCatch ensures a graceful skip rather than an opaque error if rprojroot
# cannot find the package root (e.g., unusual CI working directories).
pkg_root <- tryCatch(
  rprojroot::find_package_root_file(),
  error = function(e) NULL
)

# ── infer_poverty_line: unescaped bracket interval ───────────────────────────
# Roxygen2 treats [text] as a cross-reference link.  "[0,1]" inside a @param
# triggers: "Could not resolve link to topic '0,1'".
# Fix: use \\[0, 1\\] (escaped) or backtick notation.

test_that("infer_poverty_line @param does not contain unescaped [0,1] link syntax", {
  skip_if(is.null(pkg_root), "Cannot locate package root via rprojroot")
  src <- readLines(file.path(pkg_root, "R", "infer_poverty_line.R"))
  roxy_lines <- src[startsWith(trimws(src), "#'")]
  bad <- grep("\\[0,1\\]", roxy_lines, value = TRUE)
  expect_length(bad, 0L)
})

# ── lkup_filter: missing roxygen title ───────────────────────────────────────
# A roxygen2 block that starts with @keywords (or any @tag) and has no
# description line produces: "Skipping; no name and/or title."
# Fix: add a one-line title as the very first #' line of the block.

test_that("lkup_filter roxygen block has a title line", {
  skip_if(is.null(pkg_root), "Cannot locate package root via rprojroot")
  src <- readLines(file.path(pkg_root, "R", "utils-lkup.R"))
  func_line <- grep("^lkup_filter <- function", src)
  expect_length(func_line, 1L)

  # Collect the roxygen block immediately above the function
  block <- character(0)
  i <- func_line - 1L
  while (i >= 1L && startsWith(trimws(src[i]), "#'")) {
    block <- c(src[i], block)
    i <- i - 1L
  }

  # Strip the leading #' marker and optional single space
  content <- sub("^#'\\s?", "", block)

  # A title exists when at least one non-empty, non-@tag line is present
  non_tag_lines <- content[
    !startsWith(content, "@") & nzchar(trimws(content))
  ]
  expect_gt(
    length(non_tag_lines),
    0L,
    label = "lkup_filter roxygen block must have at least one title/description line before @tags"
  )
})
