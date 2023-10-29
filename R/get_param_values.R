#' Get valid query parameter values
#' Get vector of accepted query parameter values for the API
#' @param lkup list: A list of lkup tables
#' @param version character: Data version. Defaults to most recent version.
#' @param endpoint character: the endpoint for which to return valid parameters
#' @export
get_param_values <- function(lkup,
                             version,
                             endpoint = c("all",
                                          "aux",
                                          "pip",
                                          "pip-grp",
                                          "pip-info",
                                          "valid-params")) {
  # endpoint <- endpoint[1] # Ensure it only passes one endpoint at a time
  endpoint <- match.arg(endpoint)
  # TO IMPROVE: Too much hard-coding here
  endpoint_map <- c("all",
                    "get_aux_table",
                    "pip",
                    "pip_grp",
                    "get_pip_version",
                    "get_param_values")
  names(endpoint_map) <- c("all",
                           "aux",
                           "pip",
                           "pip-grp",
                           "pip-info",
                           "valid-params")

  # Extract accepted data versions values
  data_version <- data.frame(
    param_names  = "version",
    param_values = lkup$versions,
    param_boundaries = NA_character_,
    param_types = "character",
    stringsAsFactors = FALSE
  )
  # Extract correct query_controls list
  lkup <- lkup$versions_paths[[version]]
  query_controls <- lkup$query_controls[names(lkup$query_controls) != "parameter"]

  # Turn list into a dataframe
  out <- vector(mode = "list", length(query_controls))
  names(out) <- names(query_controls)

  # Handles cases when the endpoint parameter is specified

  if (endpoint != "all") {
    # Only return the parameters for a specific endpoint
    endpoint <- endpoint_map[endpoint]
    out <- out[names(out) %in% methods::formalArgs(endpoint)]
    # Handle the exception for pip-grp (takes regional codes instead of country codes)
    # Bad API design here. Needs review
    if (endpoint == "pip_grp") {
      query_controls$country <- query_controls$region
    }
    query_controls <- query_controls[names(query_controls) %in% names(out)]
  }

  names_out <- names(out)
  for (i in seq_along(out)) {

    param_values <- query_controls[[i]]$values
    param_values_length <- length(param_values)
    # handle numeric query controls that don't have controlled vocabulary, but
    # only boundary conditions (min and max values)
    if (query_controls[[i]]$type == "numeric") {
      param_boundaries <- names(query_controls[[i]]$values)
    } else {
      param_boundaries <- rep(NA, param_values_length)
    }
    param_names <- rep(names_out[i], param_values_length)
    param_types  <- rep(query_controls[[i]]$type, param_values_length)

    out[[i]] <- data.frame(param_names      = param_names,
                           param_values     = param_values,
                           param_boundaries = param_boundaries,
                           param_types      = param_types,
                           stringsAsFactors = FALSE)

  }

  out <- data.table::rbindlist(out)
  out <- rbind(out, data_version)

  return(out)
}

test_that("all objects are correctly passed and used", {
  root <- rprojroot::is_r_package
  tmp <- lintr::lint(filename = root$find_file("R/get_param_values.R"),
                     linters = lintr::object_usage_linter())

  expect_equal(length(tmp), 0)
})
