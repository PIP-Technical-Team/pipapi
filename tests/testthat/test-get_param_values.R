# lkups <- readRDS("../testdata/query-controls.rds")
#
# test_that("get_param_values() works", {
#   res <- get_param_values(lkup = lkups)
#   expect_equal(res, data.frame(country = lkups$query_controls$country$values))
#   res <- get_param_values('year', lkup = lkups)
#   expect_equal(res, data.frame(year = lkups$query_controls$year$values))
#   res <- get_param_values('parameter', lkup = lkups)
#   expect_equal(res, data.frame(parameter = lkups$query_controls$parameter$values[1:11]))
# })

test_that("all objects are correctly passed and used", {
  root <- rprojroot::is_r_package
  tmp <- lintr::lint(filename = root$find_file("R/get_param_values.R"),
                     linters = lintr::object_usage_linter())

  expect_equal(length(tmp), 0)
})
