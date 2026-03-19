# tests/testthat/test-blocked-aux-tables.R
#
# Tests for the blocked_aux_tables() / reset_blocked_aux_tables() mechanism.
#
# Unit tests:  run without data (no skip_if_no_lkup()).
#              Tests that modify state use withr::defer() for safe cleanup.
# Integration: require test_lkup (skip_if_no_lkup()).

# ── Getter ──────────────────────────────────────────────────────────────────

test_that("blocked_aux_tables() returns a character vector", {
  result <- blocked_aux_tables()
  expect_type(result, "character")
})

test_that("blocked_aux_tables() returns .default_blocked_tables when nothing is set", {
  withr::defer(reset_blocked_aux_tables())
  reset_blocked_aux_tables() # ensure clean state

  # The expected value mirrors .default_blocked_tables in R/blocked_aux_tables.R.
  # Update both this test and that constant when the default changes.
  expect_identical(blocked_aux_tables(), c("national_poverty_lines"))
})

test_that("blocked_aux_tables() returns no duplicates", {
  result <- blocked_aux_tables()
  expect_identical(result, unique(result))
})

# ── Setter ───────────────────────────────────────────────────────────────────

test_that("blocked_aux_tables(tables = ...) overrides the default", {
  withr::defer(reset_blocked_aux_tables())

  blocked_aux_tables(tables = c("foo", "bar"))

  expect_identical(blocked_aux_tables(), c("foo", "bar"))
})

test_that("blocked_aux_tables(tables = ...) returns the new list invisibly", {
  withr::defer(reset_blocked_aux_tables())

  result <- withVisible(blocked_aux_tables(tables = c("baz")))

  expect_false(result$visible)
  expect_identical(result$value, c("baz"))
})

test_that("blocked_aux_tables(tables = character(0)) blocks nothing", {
  withr::defer(reset_blocked_aux_tables())

  blocked_aux_tables(tables = character(0))

  expect_identical(blocked_aux_tables(), character(0))
})

test_that("blocked_aux_tables() errors when tables is not a character vector", {
  expect_error(blocked_aux_tables(tables = 123))
  expect_error(blocked_aux_tables(tables = TRUE))
  expect_error(blocked_aux_tables(tables = list("a")))
})

# ── Reset ────────────────────────────────────────────────────────────────────

test_that("reset_blocked_aux_tables() restores the default after an override", {
  withr::defer(reset_blocked_aux_tables())

  blocked_aux_tables(tables = c("something_else"))
  reset_blocked_aux_tables()

  expect_identical(blocked_aux_tables(), c("national_poverty_lines"))
})

test_that("reset_blocked_aux_tables() is safe to call when nothing is set", {
  reset_blocked_aux_tables() # idempotent — should never error

  expect_no_error(reset_blocked_aux_tables())
  expect_identical(blocked_aux_tables(), c("national_poverty_lines"))
})

# ── get_aux_table() enforcement ───────────────────────────────────────────────
# The blocked check fires before fst::read_fst(), so no real data is needed.

test_that("get_aux_table() rejects a blocked table as if it does not exist", {
  withr::defer(reset_blocked_aux_tables())
  blocked_aux_tables(tables = c("national_poverty_lines"))

  expect_error(
    get_aux_table(data_dir = "fake/path", table = "national_poverty_lines")
  )
})

test_that("get_aux_table_ui() rejects a blocked table as if it does not exist", {
  withr::defer(reset_blocked_aux_tables())
  blocked_aux_tables(tables = c("national_poverty_lines"))

  expect_error(
    get_aux_table_ui(
      data_dir = "fake/path",
      table    = "national_poverty_lines",
      lkup     = list()
    )
  )
})

test_that("blocking a nonexistent table name is a harmless no-op for other tables", {
  withr::defer(reset_blocked_aux_tables())

  # "nonexistent_table" is blocked, but "countries" is not.
  # "countries" should get past the blocked check and fail at disk read,
  # NOT with a "not an available auxiliary table" error.
  blocked_aux_tables(tables = c("nonexistent_table"))

  err <- tryCatch(
    get_aux_table(data_dir = "fake/path", table = "countries"),
    error = function(e) conditionMessage(e)
  )
  expect_false(
    grepl("not an available auxiliary table", err),
    info = "Error should come from fst disk read, not from the blocked-table check"
  )
})

# ── Integration tests ─────────────────────────────────────────────────────────

test_that("lkup$aux_tables does not contain any blocked table", {
  skip_if_no_lkup()
  withr::defer(reset_blocked_aux_tables())
  reset_blocked_aux_tables() # ensure default is active

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
  withr::defer(reset_blocked_aux_tables())
  reset_blocked_aux_tables()

  blocked  <- blocked_aux_tables()
  in_ctrl  <- intersect(test_lkup$query_controls$table$values, blocked)
  expect_identical(
    in_ctrl,
    character(0),
    info = paste(
      "Blocked tables still in query_controls$table$values:",
      paste(in_ctrl, collapse = ", ")
    )
  )
})

test_that("get_aux_table() serves an unblocked table normally", {
  skip_if_no_lkup()
  withr::defer(reset_blocked_aux_tables())

  # Temporarily unblock everything to confirm an unblocked table is served
  blocked_aux_tables(tables = character(0))

  result <- get_aux_table(
    data_dir = test_lkup$data_root,
    table    = "countries"
  )
  expect_s3_class(result, "data.table")
  expect_gt(nrow(result), 0L)
})
