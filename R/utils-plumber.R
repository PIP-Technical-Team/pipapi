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
  out <- all(values %in% valid_values)

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
  out <- is.logical(value)

  return(out)
}

#' Format error
#' Format error if check_parameters() returns TRUE
#' @param param character or numeric: Parsed parameters
#' @param valid_values character or numeric: Accepted values for this parameter
#' @return list
#' @noRd
format_error <- function(param, valid_values) {
  out <- list(
    error = paste0(
      "Invalid value for ", param, ". Please use one of",
      paste(
        "'", valid_values, "'",
        sep = "", collapse = ", "
      ), "."
    )
  )
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
    "format",
    "parameter"
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
parse_parameter <- function(param, param_name) {
  param <- urltools::url_decode(param)
  param <- strsplit(param, ",")
  param <- unlist(param)

  # CREATE GLOBALS TO AVOID HARD CODED VALUES HERE
  if (param_name %in% c("country", "year", "group_by", "welfare_type",
                        "reporting_level", "format", "parameter")) {
    param <- as.character(param)
  } else if (param_name %in% c("povline", "popshare", "ppp")) {
    param <- as.numeric(param)
  } else if (param_name %in% c("fill_gaps", "aggregate")) {
    param <- as.logical(param)
  }

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
          sfn <- jsonlite::toJSON
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
