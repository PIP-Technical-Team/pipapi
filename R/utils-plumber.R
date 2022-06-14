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
    "endpoint"
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
  params <- type.convert(params, as.is = TRUE)

  return(params)
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
      req$args$country <- "all"
      req$argsQuery$country <- "all"
    }
    if (is.null(req$args$year)) {
      req$args$year <- "all"
      req$argsQuery$year <- "all"
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
