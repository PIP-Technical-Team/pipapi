if (Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") != "") {
  lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
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

test_that("parse_parameter() correctly parses query parameter values", {
  # Single character
  out <- parse_parameter(param = "AGO", param_name = "country")
  expect_equal(out, "AGO")
  # Multiple character
  out <- parse_parameter(param = "SSA,LAC,WLD", param_name = "country")
  expect_equal(out, c("SSA", "LAC", "WLD"))
  # Single integer
  out <- parse_parameter(param = "2018", param_name = "year")
  expect_equal(out, 2018L)
  # Multiple integers
  out <- parse_parameter(param = "2018,2020", param_name = "year")
  expect_equal(out, c(2018L, 2020L))
  # Single numeric
  out <- parse_parameter(param = "1.9", param_name = "povline")
  expect_equal(out, 1.9)
  # Multiple numerics
  out <- parse_parameter(param = "1.9,3.2", param_name = "povline")
  expect_equal(out, c(1.9, 3.2))
  # Single logical
  out <- parse_parameter(param = "TRUE", param_name = "fill_gaps")
  expect_equal(out, TRUE)
  # Multiple logicals
  out <- parse_parameter(param = c("TRUE", "FALSE"), param_name = "fill_gaps")
  expect_equal(out, c(TRUE, FALSE))

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

x <- c("20220609_2011_02_02_PROD", "20220504_2017_01_02_PROD", "20211212_2011_01_01_PROD",
       "20200101_2011_01_01_PROD", "20220602_2017_01_02_INT", "20220504_2017_01_02_INT", "20211212_2011_02_01_PROD")

test_that("return_correct_version works as expected", {
  expect_equal(return_correct_version(ppp_version = 2017, versions_available = x), "20220504_2017_01_02_PROD")
  expect_equal(return_correct_version(ppp_version = 2017, identity = "INT", versions_available = x), "20220602_2017_01_02_INT")
  expect_equal(return_correct_version(release_version = 20220504, versions_available = x), "20220504_2017_01_02_PROD")
  expect_equal(return_correct_version(release_version = 20220504, ppp_version = 2017, versions_available = x), "20220504_2017_01_02_PROD")
  expect_equal(return_correct_version(version = "20220609_2011_02_02_PROD", versions_available = x), "20220609_2011_02_02_PROD")
  expect_equal(return_correct_version(release_version = 20220504, ppp_version = 2017, identity = "INT", versions_available = x), "20220504_2017_01_02_INT")
  expect_equal(return_correct_version(release_version = 20220504, identity = "INT", versions_available = x), "20220504_2017_01_02_INT")
  expect_equal(return_correct_version(release_version = 20211212, ppp_version = 2011, versions_available = x), "20211212_2011_02_01_PROD")
})


test_that("extract_release_date works as expected", {
  val <- extract_release_date(x)
  expect_equal(val, as.Date(c("2022-06-09", "2022-05-04", "2021-12-12", "2020-01-01", "2022-06-02", "2022-05-04", "2021-12-12")))
  expect_s3_class(val, "Date")
})

test_that("extract_ppp_date works as expected", {
  val <- extract_ppp_date(x)
  expect_equal(val, as.Date(c("2011-02-02","2017-01-02","2011-01-01","2011-01-01","2017-01-02","2017-01-02","2011-02-01")))
  expect_s3_class(val, "Date")
})

test_that("extract_identity works as expected", {
  val <- extract_identity(x)
  expect_equal(val, c("PROD", "PROD", "PROD", "PROD", "INT", "INT", "PROD"))
  expect_type(val, "character")
  expect_length(val, length(x))
})

test_that("version_dataframe works as expected", {
  out <- version_dataframe(x)
  expect_s3_class(out, "data.frame")
  expect_equal(dim(out), c(7, 4))
  expect_true(all(sapply(out, class) == "character"))
})


test_that("rpi_version works as expected", {
    expect_equal(rpi_version("20220602", "2017", "INT", x), "20220602_2017_01_02_INT")
    expect_equal(rpi_version("20220504", "2017", "PROD", x), "20220504_2017_01_02_PROD")
})

test_that("rp_version works as expected", {
  expect_equal(rp_version("20220504", "2017", x), c("20220504_2017_01_02_PROD", "20220504_2017_01_02_INT"))
  expect_equal(rp_version("20220609", "2011", x), "20220609_2011_02_02_PROD")
})

test_that("ri_version works as expected", {
  expect_equal(ri_version("20220602", "INT", x), "20220602_2017_01_02_INT")
  expect_equal(ri_version("20220504", "PROD", x), "20220504_2017_01_02_PROD")
})

test_that("pi_version works as expected", {
  expect_equal(pi_version("2017", "INT", x), c("20220602_2017_01_02_INT", "20220504_2017_01_02_INT"))
  expect_equal(pi_version("2011", "PROD", x), c("20220609_2011_02_02_PROD", "20211212_2011_01_01_PROD",
                                                "20200101_2011_01_01_PROD", "20211212_2011_02_01_PROD"))
})
