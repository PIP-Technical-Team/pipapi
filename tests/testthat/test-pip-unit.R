# Pure unit tests for pip() that require no file system access.
# Integration tests (fill_gaps, distributional stats, popshare, etc.) live in
# tests/testthat/test-integ-survey-years.R and test-integ-lineup-years.R.

test_that("pip errors when a multi-dataset lkups list is passed instead of single lkup", {
  # validate_lkup() catches missing svy_lkup field first (lkups wraps multiple datasets).
  # Use test_lkups (defined in helper-lkup.R) — never rely on interactive-session globals.
  skip_if(
    is.null(test_lkups),
    "test_lkups not available — set PIPAPI_DATA_ROOT_FOLDER_LOCAL"
  )
  expect_error(
    pip(country = "all", year = "all", povline = 1.9, lkup = test_lkups),
    "svy_lkup"
  )
})

test_that("pip works for multiple povline values", {
  skip_if_no_lkup()
  out1 <- pip(country = "AGO", year = 2000, povline = 1.9,     lkup = test_lkup)
  out2 <- pip(country = "AGO", year = 2000, povline = 1.675,   lkup = test_lkup)
  out3 <- pip(country = "AGO", year = 2000, povline = c(1.675, 1.9), lkup = test_lkup)

  expect_identical(rbind(out2, out1), out3)
})
