

# test_that("Package code adheres to lintr recommendations", {
#   # Get the path to your package
#   pkg_path <- system.file(package = "pipapi")
#
#   # Lint the entire package
#   lint_results <- lintr::lint_package(pkg_path)
#
#   # Expect no linting issues
#   expect_equal(length(lint_results), 0,
#                info = paste(sapply(lint_results, function(l) l$message), collapse = "\n"))
# })
#
# tmp <- lintr::lint_package(linters = linters_with_tags(tags = "package_developement"))
#
# tmp <- lintr::lint_package(linters = object_usage_linter())
#
# tmp <- lintr::lint(filename = "./R/add_agg_stats.R",
#                    linters = object_usage_linter())
