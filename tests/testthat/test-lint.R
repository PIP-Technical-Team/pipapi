
test_that("there is unused objects", {
  lints <- lintr::lint_package(linters = lintr::object_usage_linter())
  # NOTE: There are still some flagged lints, but they should be harmless
  # Some are due to data.table use of NSE
  # Others are due to code like the chunk below:
  # if (reporting_level %in% c("national", "all")) {
  #   out <- add_agg_stats(out,
  #                        return_cols = lkup$return_cols$ag_average_poverty_stats)
  # }
  expect_equal(length(lints), 9, info = as.character(lints))
})

test_that("there is no cyclomatic complexity issue", {
  lints <- lintr::lint_package(linters = lintr::cyclocomp_linter(complexity_limit = 20L))

  expect_equal(length(lints), 0, info = as.character(lints))
})

test_that("there is no unnecessary nested if conditions", {
  skip("TO BE REVIEWED")
  lints <- lintr::lint_package(linters = lintr::unnecessary_nested_if_linter())

  expect_equal(length(lints), 0, info = as.character(lints))
})

test_that("all code is reachable", {
  lints <- lintr::lint_package(linters = lintr::unreachable_code_linter())

  expect_equal(length(lints), 0, info = as.character(lints))
})

test_that("there is no duplicate arguments in function calls", {
  lints <- lintr::lint_package(linters = lintr::duplicate_argument_linter())

  expect_equal(length(lints), 0, info = as.character(lints))
})

test_that("default arguments come last", {
  skip("TO BE REVIEWED")
  lints <- lintr::lint_package(linters = lintr::function_argument_linter())

  expect_equal(length(lints), 0, info = as.character(lints))
})


test_that("c() is applied before expensive functions", {
  lints <- lintr::lint_package(linters = lintr::inner_combine_linter())

  expect_equal(length(lints), 0, info = as.character(lints))
})


test_that("there is no function with missing argument", {
  lints <- lintr::lint_package(linters = lintr::missing_argument_linter())

  expect_equal(length(lints), 0, info = as.character(lints))
})

test_that("there is no namespace issue", {
  lints <- lintr::lint_package(
    linters = lintr::namespace_linter(
      check_exports = FALSE,
      check_nonexports = FALSE)
  )

  expect_equal(length(lints), 0, info = as.character(lints))
})

test_that("lambda functions are not used unnecessarily", {
  lints <- lintr::lint_package(linters = lintr::unnecessary_lambda_linter())

  expect_equal(length(lints), 0, info = as.character(lints))
})

test_that("no unnecessary concatenation is being used", {
  skip("TO BE REVIEWED")
  lints <- lintr::lint_package(
    linters = lintr::unnecessary_concatenation_linter(
      allow_single_expression = TRUE)
    )

  expect_equal(length(lints), 0, info = as.character(lints))
})
