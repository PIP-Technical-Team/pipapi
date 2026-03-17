# test-input-validation.R
#
# Tests that entry-point functions abort with clear errors when passed a
# structurally invalid lkup.  All tests use synthetic inline data and do NOT
# require PIPAPI_DATA_ROOT_FOLDER_LOCAL to be set.

# Minimal stub lkup structures -----------------------------------------------

.empty_lkup <- list()

.core_lkup <- list(
  svy_lkup      = list(),
  data_root     = tempdir(),
  return_cols   = list(),
  aux_files     = list(),
  cache_data_id = list(hash_pip = "abc123", hash_pip_grp = "def456")
)

.full_lkup <- c(
  .core_lkup,
  list(
    use_new_lineup_version = TRUE,
    interpolation_list     = list(),
    refy_lkup              = list(),
    query_controls         = list(region = list(values = character(0)))
  )
)


# pip() validation -----------------------------------------------------------

test_that("pip() aborts when lkup is empty list", {
  expect_error(
    pip(lkup = .empty_lkup),
    class = "rlang_error"
  )
})

test_that("pip() error message names a specific missing field", {
  err <- tryCatch(pip(lkup = .empty_lkup), error = function(e) e)
  # The error should mention one of the core required fields
  expect_match(conditionMessage(err), "svy_lkup|data_root|return_cols|aux_files|cache_data_id")
})

test_that("pip() aborts when core fields present but new_pathway fields absent", {
  expect_error(
    pip(lkup = .core_lkup),
    class = "rlang_error"
  )
})


# pip_new_lineups() validation -----------------------------------------------

test_that("pip_new_lineups() aborts when lkup is empty list", {
  expect_error(
    pip_new_lineups(lkup = .empty_lkup),
    class = "rlang_error"
  )
})

test_that("pip_new_lineups() aborts when core fields present but new_pathway absent", {
  expect_error(
    pip_new_lineups(lkup = .core_lkup),
    class = "rlang_error"
  )
})


# pip_agg() validation -------------------------------------------------------

test_that("pip_agg() aborts when lkup is empty list", {
  expect_error(
    pip_agg(lkup = .empty_lkup),
    class = "rlang_error"
  )
})

test_that("pip_agg() aborts when query_controls is missing", {
  lkup_no_query <- .full_lkup
  lkup_no_query$query_controls <- NULL
  expect_error(
    pip_agg(lkup = lkup_no_query),
    class = "rlang_error"
  )
})

test_that("pip_agg() error message mentions query_controls when that field is missing", {
  lkup_no_query <- .full_lkup
  lkup_no_query$query_controls <- NULL
  err <- tryCatch(pip_agg(lkup = lkup_no_query), error = function(e) e)
  expect_match(conditionMessage(err), "query_controls")
})
