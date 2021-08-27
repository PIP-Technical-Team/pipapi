lkups <- readRDS("../testdata/query-controls.rds")

test_that("get_param_values() works", {
  res <- get_param_values('country', lkup = lkups)
  expect_equal(res, data.frame(country = lkups$query_controls$country$values))
  res <- get_param_values('year', lkup = lkups)
  expect_equal(res, data.frame(year = lkups$query_controls$year$values))
  res <- get_param_values('parameter', lkup = lkups)
  expect_equal(res, data.frame(parameter = lkups$query_controls$parameter$values[1:11]))
})
