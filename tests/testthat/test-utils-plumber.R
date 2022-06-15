if (Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") != "") {
  lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
  lkups <- lkups$versions_paths[[lkups$latest_release]]
} else {
  # lkups$query_controls$version <- NULL
  # saveRDS(list(query_controls = lkups$query_controls),
  #         'tests/testdata/query-controls.rds')
  lkups <- readRDS("../testdata/query-controls.rds")
}

test_that("validate_query_parameters() works", {

  # Test that all pip() parameters are accepted
  req <- list(argsQuery = list(
    country = "AGO",
    year = "2018",
    povline = "1.9",
    popshare = "NULL",
    fill_gaps = "FALSE",
    aggregate = "FALSE",
    group_by = "none",
    welfare_type = "all",
    reporting_level = "all",
    ppp = "NULL",
    format = "json"
  ))
  tmp <- validate_query_parameters(req)
  expect_identical(req$argsQuery, tmp)

  # Test that non-accepted parameter are removed
  req <- list(argsQuery = list(
    country = "AGO",
    test = "hello"
  ))
  tmp <- validate_query_parameters(req)
  expect_identical(list(country = "AGO"), tmp)
})

test_that("parse_parameters() works as expected", {
  params <- list(
    country = "AGO",
    year = "2018",
    povline = "1.9",
    fill_gaps = "FALSE",
    aggregate = "FALSE",
    group_by = "none",
    welfare_type = "all",
    reporting_level = "all",
    popshare = "0.5",
    ppp = "1",
    format = "json"
  )
  tmp <- parse_parameters(params)
  expect_type(tmp$country, "character")
  expect_type(tmp$year, "integer")
  expect_true(is.numeric(tmp$povline))
  expect_type(tmp$fill_gaps, "logical")
  expect_type(tmp$aggregate, "logical")
  expect_type(tmp$group_by, "character")
  expect_type(tmp$welfare_type, "character")
  expect_type(tmp$reporting_level, "character")
  expect_true(is.numeric(tmp$popshare))
  expect_true(is.numeric(tmp$ppp))
  expect_type(tmp$format, "character")
})

test_that("check_parameters() works as expected", {

  # Test that all pip() parameters are accepted
  req <- list(argsQuery = list(
    country = "AGO",
    year = 2018,
    povline = 1.9,
    popshare = NULL,
    fill_gaps = FALSE,
    aggregate = FALSE,
    group_by = "none",
    welfare_type = "all",
    reporting_level = "all",
    ppp = NULL
  ))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_true(all(tmp))

  # Invalid country parameters
  req <- list(argsQuery = list(country = "XYZ"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)
  req <- list(argsQuery = list(country = 1.9))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

  # Invalid year parameter
  req <- list(argsQuery = list(country = 2050))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

  # Invalid povline parameters
  req <- list(argsQuery = list(povline = "all"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)
  req <- list(argsQuery = list(povline = lkups$query_controls$povline$values[["max"]] + 1))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

  # Invalid popshare parameter
  req <- list(argsQuery = list(popshare = 2))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

  # Invalid fill_gaps parameter
  req <- list(argsQuery = list(fill_gaps = "TRUE"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

  # Invalid aggregate parameter
  req <- list(argsQuery = list(aggregate = "FALSE"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

  # Invalid group_by parameter
  req <- list(argsQuery = list(group_by = "pcn"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

  # Invalid welfare_type parameter
  req <- list(argsQuery = list(welfare_type = "INC"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

  # Invalid reporting_level parameter
  req <- list(argsQuery = list(reporting_level = "ALL"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_true(tmp)

  # Invalid ppp parameter
  req <- list(argsQuery = list(ppp = "NULL"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

  # Invalid format parameter
  req <- list(argsQuery = list(format = "tiff"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)
})

test_that("format_error() works as expected", {

  req <- list(argsQuery = list(country = "XXX", year = 2050, povline = 0))
  params <- names(req$argsQuery)
  tmp <- format_error(params, lkups$query_controls)
  expect_identical(names(tmp), c("error", "details"))
  expect_identical(tmp$error, "Invalid query arguments have been submitted.")
  expect_identical(names(tmp$details), c("country", "year", "povline"))
  expect_identical(names(tmp$details$country), c("msg", "valid"))
  expect_identical(names(tmp$details$year), c("msg", "valid"))
  expect_identical(names(tmp$details$povline), c("msg", "valid"))
  expect_identical(names(tmp$details$povline$valid), c("min", "max"))

  req <- list(argsQuery = list(table = "tmp"))
  params <- names(req$argsQuery)
  tmp <- format_error(params, lkups$query_controls)
  expect_identical(tmp$error, "Invalid query arguments have been submitted.")
  expect_identical(names(tmp$details), c("table"))
  expect_identical(names(tmp$details$table), c("msg", "valid"))
  expect_identical(tmp$details$table$valid, lkups$aux_tables)
})

test_that("assign_required_params works as expected for /pip endpoint", {

  req <- list()
  req$PATH_INFO <- "api/v1/pip"
  req <- assign_required_params(req)

  expect_identical(req$args$country, "all")
  expect_identical(req$args$year, "all")
  expect_identical(req$argsQuery$country, "all")
  expect_identical(req$argsQuery$year, "all")
})

test_that("assign_required_params works as expected for /pip-grp endpoint", {

  req <- list()
  req$PATH_INFO <- "api/v1/pip-grp"
  req <- assign_required_params(req)

  expect_identical(req$args$country, "all")
  expect_identical(req$args$year, "all")
  expect_identical(req$args$group_by, "none")
  expect_identical(req$argsQuery$country, "all")
  expect_identical(req$argsQuery$year, "all")
  expect_identical(req$argsQuery$group_by, "none")
})

test_that("extract_endpoint works as expected", {

  expect_identical(extract_endpoint("api/v1/pip"), "pip")
  expect_identical(extract_endpoint("/api//v1/pip"), "pip")
  expect_identical(extract_endpoint("/api//v1//pip"), "pip")
  expect_identical(extract_endpoint("api/v2/pip"), "pip")
  expect_identical(extract_endpoint("api/v1/pip-grp"), "pip-grp")
  expect_identical(extract_endpoint("api/v2/pip-grp"), "pip-grp")
  expect_identical(extract_endpoint("api/v1/aux"), "aux")
  expect_identical(extract_endpoint("api/v2/aux"), "aux")
})
