# Tests for miscellaneous utility functions in utils.R
# Integration-level tests for select_*, subset_lkup, and select_years are in
# test-utils-lkup.R (pure-unit) and tests/testthat/test-integ-*.R (integration).

test_that("unnest_dt_longer works as expected", {
  df <- data.frame(
    a = LETTERS[1:5],
    b = LETTERS[6:10]
  )

  df$list_column1 <- list(c(LETTERS[1:5]), "F", "G", "H", "I")
  df$list_column2 <- list(c(LETTERS[1:5]), "F", "G", "H", "K")

  out <- unnest_dt_longer(df, c("list_column1", "list_column2"))
  expect_equal(dim(out), c(9L, 4L))
})
