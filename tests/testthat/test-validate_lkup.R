test_that("validate_lkup passes with all core fields present", {
  lkup <- list(
    svy_lkup     = list(),
    data_root    = "/data",
    return_cols  = list(),
    aux_files    = list(),
    cache_data_id = list()
  )
  expect_invisible(validate_lkup(lkup, "core"))
})

test_that("validate_lkup aborts on missing core field", {
  lkup <- list(
    svy_lkup  = list(),
    data_root = "/data"
    # return_cols, aux_files, cache_data_id missing
  )
  expect_error(validate_lkup(lkup, "core"), class = "rlang_error")
})

test_that("validate_lkup error message names the missing field", {
  lkup <- list(svy_lkup = list(), data_root = "/data",
               return_cols = list(), aux_files = list())
  # cache_data_id is missing
  err <- tryCatch(validate_lkup(lkup, "core"), error = function(e) e)
  expect_match(conditionMessage(err), "cache_data_id")
})

test_that("validate_lkup passes with multiple contexts when all fields present", {
  lkup <- list(
    svy_lkup              = list(),
    data_root             = "/data",
    return_cols           = list(),
    aux_files             = list(),
    cache_data_id         = list(),
    use_new_lineup_version = TRUE,
    interpolation_list    = list(),
    refy_lkup             = list()
  )
  expect_invisible(validate_lkup(lkup, c("core", "new_pathway")))
})

test_that("validate_lkup aborts when new_pathway field missing", {
  lkup <- list(
    svy_lkup              = list(),
    data_root             = "/data",
    return_cols           = list(),
    aux_files             = list(),
    cache_data_id         = list(),
    use_new_lineup_version = TRUE
    # interpolation_list and refy_lkup missing
  )
  expect_error(validate_lkup(lkup, c("core", "new_pathway")), class = "rlang_error")
})

test_that("validate_lkup aborts on unknown context", {
  lkup <- list()
  expect_error(validate_lkup(lkup, "nonexistent_ctx"), class = "rlang_error")
})

test_that("assert_lkup_field passes when field is present", {
  lkup <- list(my_field = 1)
  expect_invisible(assert_lkup_field(lkup, "my_field"))
})

test_that("assert_lkup_field aborts when field is absent", {
  lkup <- list()
  expect_error(assert_lkup_field(lkup, "my_field"), class = "rlang_error")
})

test_that("assert_lkup_field error message names the missing field", {
  lkup <- list()
  err <- tryCatch(assert_lkup_field(lkup, "svy_lkup"), error = function(e) e)
  expect_match(conditionMessage(err), "svy_lkup")
})
