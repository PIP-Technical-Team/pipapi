#' Check validity of query parameters
#'
#' @param req req: plumber request environment
#' @param query_controls list: List of valid values
#' @return logical
#' @noRd
check_parameters <- function(req, query_controls) {
  out <- lapply(seq_along(req$argsQuery), function(i) {
    param_name <- names(req$argsQuery)[i]
    check_parameter(
      values = req$argsQuery[[i]],
      valid_values = query_controls[[param_name]][["values"]],
      type = query_controls[[param_name]][["type"]]
    )
  })
  out <- unlist(out)
  return(out)
}

#' Check validity of a single query parameter
#' @param values vector: vector of values
#' @param valid_values vector: vector of valid values
#' @param type character: Type of value
#' @return logical
#' @noRd
check_parameter <- function(values, valid_values, type) {
  if (type == "character") {
    check_param_chr(values, valid_values)
  } else if (type == "numeric") {
    check_param_num(values, valid_values)
  } else if (type == "logical") {
    check_param_lgl(values)
  }
}

#' Check validity of a single query parameter
#' @param values character: query values
#' @param valid_values character: valid values
#' @return logical
#' @noRd
check_param_chr <- function(values, valid_values) {
  out <- all(tolower(values) %in% tolower(valid_values))

  return(out)
}

#' Check validity of a single query parameter
#' @param value numeric: query value
#' @param valid_values list: List of valid bounded values
#' @return logical
#' @noRd
check_param_num <- function(value, valid_values) {
  out <- all(value >= valid_values[["min"]], value <= valid_values[["max"]])
  return(out)
}

#' Check validity of a single query parameter
#' @param value logical: query values
#' @return logical
#' @noRd
check_param_lgl <- function(value) {
  out <- is.logical(value) & !is.na(value)

  return(out)
}

#' Format error
#' Format error if check_parameters() returns TRUE
#' @param params character: Vector with parsed parameters
#' @param query_controls list: Query controls
#' @return list
#' @noRd
format_error <- function(params, query_controls) {
  msg1 <- "You supplied an invalid value for %s. Please use one of the valid values."
  msg2 <- "Invalid query arguments have been submitted."
  # params <- names(params)
  out <- lapply(params, function(x) {
    list(msg = sprintf(msg1, x),
         valid = query_controls[[x]]$values)
  })
  names(out) <- params
  out <- list(error = msg2, details = out)
  return(out)
}

#' Validate query parameters
#' @param params character: Query parameter to be parsed
#' @param valid_params character: Accepted query parameters
#' @noRd
validate_query_parameters <-
  function(params, valid_params = c(
    "country",
    "year",
    "povline",
    "popshare",
    "fill_gaps",
    "aggregate",
    "group_by",
    "welfare_type",
    "reporting_level",
    "ppp",
    "version",
    "format",
    "table",
    "parameter",
    "endpoint",
    "long_format"
  )) {
    params$argsQuery <-
      params$argsQuery[names(params$argsQuery) %in% valid_params]

    return(params$argsQuery)
  }


#' Parse parameters
#' @param params character: Query parameter to be parsed
#' @return character
#' @noRd
parse_parameters <- function(params) {
  for (i in seq_along(params)) {
    params[[i]] <- parse_parameter(
      param = params[[i]],
      param_name = names(params)[i]
    )
  }
  return(params)
}

#' Parse parameters
#' @param param character: Query parameter to be parsed
#' @param param_name character: Parameter name
#' @return character
#' @noRd
parse_parameter <- function(param,
                            param_name) {
  param <- urltools::url_decode(param)
  param <- strsplit(param, ",")
  param <- unlist(param)

  # Make API case insensitive
  if (param_name %in% c("country", "fill_gaps", "version", "aggregate", "long_format")) {
    param <- toupper(param)
    if (param_name == "country") {
      if (length(param[param == "ALL"]) > 0) {
        param[param == "ALL"] <- "all"
      }
    }
  } else {
    param <- tolower(param)
  }

  param <- type.convert(param, as.is = TRUE)

  return(param)
}

#' Switch serializer
#' @return A plumber response
#' @noRd
serializer_switch <- function() {
  function(val, req, res, errorHandler) {
    tryCatch(
      {
        format <- attr(val, "serialize_format")
        if (is.null(format) || format == "json") {
          type <- "application/json"
          sfn <- function(x) jsonlite::toJSON(x, na = "null")
        } else if (format == "csv") {
          type <- "text/csv; charset=UTF-8"
          sfn <- function(x) readr::format_csv(x, na = "")
        } else if (format == "rds") {
          type <- "application/rds"
          sfn <- function(x) base::serialize(x, NULL)
        }
        val <- sfn(val)
        res$setHeader("Content-Type", type)
        res$body <- val
        res$toResponse()
      },
      error = function(err) {
        errorHandler(req, res, err)
      }
    )
  }
}

#' Assign PIP API required parameters if missing
#'
#' @param req list: plumber `req` object
#'
#' @return list
#' @noRd
assign_required_params <- function(req) {

  # Handle required names for /pip endpoint
  endpoint <- extract_endpoint(req$PATH_INFO)
  if (endpoint %in% c("pip", "pip-grp")) {
    if (is.null(req$args$country)) {
      req$args$country <- "ALL"
      req$argsQuery$country <- "ALL"
    }
    if (is.null(req$args$year)) {
      req$args$year <- "ALL"
      req$argsQuery$year <- "ALL"
    }
    if (endpoint == "pip-grp") {
      if (is.null(req$args$group_by)) {
        req$args$group_by <- "none"
        req$argsQuery$group_by <- "none"
      }
    }
  }
  return(req)
}

#' helper function to extract endpoint from req$PATH_INFO object
#'
#' @param path character: Information returned by req$PATH_INFO
#' @return character
#' @noRd
extract_endpoint <- function(path) {
  # stringr::str_extract(path, pattern = "([^/]+$)")
  sub(".*[/]", "", path)
}


#' Return the version of the data
#'
#' @param version Data version. Defaults to most recent version. See api/v1/versions
#' @param release_version date when the data was published in YYYYMMDD format
#' @param ppp_version ppp year to be used
#' @param identity One of "PROD" (production), "INT" (internal) and "TEST"
#' @param versions_available character vector of all the versions available
#'
#' @return character
#'
#' @export
#'
return_correct_version <- function(version = NULL,
                                   release_version = NULL,
                                   ppp_version = NULL,
                                   identity = 'PROD',
                                   versions_available) {
  # STEP 1 -If the full `version` ID is passed return it directly.
  if (!is.null(version)) return(version)
  # STEP 2 - If at least a partial version ID is passed, infer the full version ID
  # STEP 2.1 - All partial IDs are passed. Combined them into a full version ID
  if (!is.null(release_version) & !is.null(ppp_version) & !is.null(identity)) {
    selected_version <- rpi_version(release_version, ppp_version, identity, versions_available)
  } else if (!is.null(release_version) & !is.null(ppp_version)) {
  # STEP 2.2 - If identity is NULL, return closest matching version if it exists
    # This probably would never be executed since identity would never be NULL unless explicitly specified.
    selected_version <- rp_version(release_version, ppp_version, versions_available)
  # STEP 2.3 - If ppp_version is NULL, return closest matching version if it exists
  } else if (!is.null(release_version) & !is.null(identity)) {
    selected_version <- ri_version(release_version, identity, versions_available)
  # STEP 2.4 - If release_version is NULL, return closest matching version if it exists
  } else if (!is.null(ppp_version) & !is.null(identity)) {
    selected_version <- pi_version(ppp_version, identity, versions_available)
  }
  # STEP 3 - If no matching version is found return error
  if (length(selected_version) == 0)
    #Since the function returns character values
    return("404")
  # STEP 4 - If only 1 value matches return it
  else if (length(selected_version) == 1)
    return(selected_version)
  # STEP 5 - If multiple match, the most recent version (max version value)
  else return(max(selected_version))
}


#' Return the ppp date from the version of the data
#'
#' @param version character vector of data version
#'
#' @return Date of ppp
#'
extract_ppp_date <- function(version) {
  as.Date(sub('\\d+_(\\d{4}_\\d{2}_\\d{2})_[A-Z]+', '\\1', version), '%Y_%m_%d')
}

#' Return the release date from the version of the data
#'
#' @param version character vector of data version
#'
#' @return Date of release
#'
extract_release_date <- function(version) {
  as.Date(sub('(\\d+)_\\d{4}_\\d{2}_\\d{2}_[A-Z]+', '\\1', version), '%Y%m%d')
}

#' Return identity from the version of the data
#'
#' @param version character vector of data version
#'
#' @return character vector of identity
#'
#'
extract_identity <- function(version) {
  #Extract everything till last underscore
  sub('.*_', '', version)
}

#' Return versions of the data available.
#'
#' @param version character vector of data version
#'
#' @return Dataframe with 4 columns, versions, release_version, ppp_version and identity
#'
#' @export
#'
version_dataframe <- function(versions) {
  ppp_version <- format(extract_ppp_date(versions), '%Y')
  release_version <- format(extract_release_date(versions), "%Y%m%d")
  identity <- extract_identity(versions)
  out <- data.frame(version = versions,
                    release_version = release_version,
                    ppp_version = ppp_version,
                    identity = identity)
  return(out)
}



rpi_version <- function(release_version, ppp_version, identity, versions_available) {
  grep(sprintf('^%s_%s_\\d{2}_\\d{2}_%s$', release_version, ppp_version, identity), versions_available, value = TRUE)
}

rp_version <- function(release_version, ppp_version, versions_available) {
  grep(sprintf('^%s_%s_\\d{2}_\\d{2}_[A-Z]+$', release_version, ppp_version), versions_available, value = TRUE)
}

ri_version <- function(release_version, identity, versions_available) {
  grep(sprintf('^%s_\\d{4}_\\d{2}_\\d{2}_%s$', release_version, identity), versions_available, value = TRUE)
}

pi_version <- function(ppp_version, identity, versions_available) {
  grep(sprintf('\\d{6}_%s_\\d{2}_\\d{2}_%s$', ppp_version, identity), versions_available, value = TRUE)
}


#' Return citation from the version
#'
#' @param version character vector of data version
#'
#' @return character. Text containing citation for the version passed.
#'
#' @export
#'
citation_from_version <- function(version) {
  release_date <- extract_release_date(version)
  ppp_date <- extract_ppp_date(version)
  sprintf('Poverty and Inequality Platform, %s, %s PPPs.', release_date, format(ppp_date, '%Y'))
}
