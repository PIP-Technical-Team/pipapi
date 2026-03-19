# tests/testthat/test-blocked-aux-tables.R
#
# Tests for the blocked_aux_tables() mechanism.
#
# Unit tests:  run without data (no skip_if_no_lkup()).
# Integration: require test_lkup (skip_if_no_lkup()).

# ── Unit tests ─────────────────────────────────────────────────────────────

test_that("blocked_aux_tables() returns a character vector", {
  result <- blocked_aux_tables()
  expect_true(is.character(result))
})

test_that("blocked_aux_tables() currently blocks national_poverty_lines", {
  expect_true("national_poverty_lines" %in% blocked_aux_tables())
})

test_that("blocked_aux_tables() returns no duplicates", {
  result <- blocked_aux_tables()
  expect_identical(result, unique(result))
})

# ── Integration tests ───────────────────────────────────────────────────────

test_that("lkup$aux_tables does not contain any blocked table", {
  skip_if_no_lkup()
  blocked <- blocked_aux_tables()
  in_lkup <- intersect(test_lkup$aux_tables, blocked)
  expect_identical(
    in_lkup,
    character(0),
    info = paste(
      "Blocked tables still in lkup$aux_tables:",
      paste(in_lkup, collapse = ", ")
    )
  )
})

test_that("lkup$query_controls$table$values does not contain any blocked table", {
  skip_if_no_lkup()
  blocked <- blocked_aux_tables()
  in_ctrl <- intersect(test_lkup$query_controls$table$values, blocked)
  expect_identical(
    in_ctrl,
    character(0),
    info = paste(
      "Blocked tables still in query_controls$table$values:",
      paste(in_ctrl, collapse = ", ")
    )
  )
})

test_that("get_aux_table() still works for a blocked table", {
  skip_if_no_lkup()
  # national_poverty_lines is currently blocked; confirm it is still readable
  # directly from disk via get_aux_table()
  result <- get_aux_table(
    data_dir = test_lkup$data_root,
    table = "national_poverty_lines"
  )
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0L)
})
