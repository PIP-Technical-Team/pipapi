lkups <- pipapi:::clean_api_data(
  data_folder_root = Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))

test_that("validate_query_parameters() works", {

  # Test that all pip() parameters are accepted
  req <- list(argsQuery = list(
    country = "AGO",
    year = "2018",
    povline = "1.9",
    popshare  = "NULL",
    fill_gaps = "FALSE",
    aggregate = "FALSE",
    group_by  = "none",
    welfare_type = "all",
    reporting_level = "all",
    ppp = "NULL"
  ))
  tmp <- validate_query_parameters(req)
  expect_identical(req$argsQuery, tmp)

  # Test that non-accepted parameter are removed
  req <- list(argsQuery = list(
    country = "AGO",
    test = "hello"))
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
    group_by  = "none",
    welfare_type = "all",
    reporting_level = "all",
    popshare = "0.5",
    ppp = "1"
  )
  tmp <- parse_parameters(params)
  expect_type(tmp$country, 'character')
  expect_type(tmp$year, 'character')
  expect_true(is.numeric(tmp$povline))
  expect_type(tmp$fill_gaps, 'logical')
  expect_type(tmp$aggregate, 'logical')
  expect_type(tmp$group_by, 'character')
  expect_type(tmp$welfare_type, 'character')
  expect_type(tmp$reporting_level, 'character')
  expect_true(is.numeric(tmp$popshare))
  expect_true(is.numeric(tmp$ppp))

})

test_that("check_parameters() works as expected", {

  # Test that all pip() parameters are accepted
  req <- list(argsQuery = list(
    country = "AGO",
    year = 2018,
    povline = 1.9,
    popshare  = NULL,
    fill_gaps = FALSE,
    aggregate = FALSE,
    group_by  = "none",
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
  req <- list(argsQuery = list(povline = 100))
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
  expect_false(tmp)

  # Invalid ppp parameter
  req <- list(argsQuery = list(ppp = "NULL"))
  tmp <- check_parameters(req, lkups$query_controls)
  expect_false(tmp)

})

test_that("format_error() works as expected", {
  tmp <- format_error("XYZ", c('AGO', 'BOL'))
  expect_identical(tmp$error,
                   "Invalid value for XYZ. Please use one of'AGO', 'BOL'.")
})
