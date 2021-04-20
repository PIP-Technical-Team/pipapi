#' parse_parameters_chr
#'
#' @param param character: Query parameter to be parsed
#'
#' @return character
#' @export
#'
parse_parameters_chr <- function(param) {

  if (!is.null(param)) {
    param <- urltools::url_decode(param)
    param <- strsplit(param, ",")
    param <- unlist(param)
  }

  return(param)
}

#' parse_parameters_int
#'
#' @param param character: Query parameter to be parsed
#'
#' @return integer
#' @export
#'
parse_parameters_int <- function(param) {

  if (!is.null(param)) {
    param <- parse_parameters_chr(param)
    param <- as.integer(param)
  }

  return(param)
}

#' parse_parameters_dbl
#'
#' @param param character: Query parameter to be parsed
#'
#' @return integer
#' @export
#'
parse_parameters_dbl <- function(param) {

  if (!is.null(param)) {
    param <- parse_parameters_chr(param)
    param <- as.numeric(param)
  }

  return(param)
}

#' parse_parameters_lgl
#'
#' @param param character: Query parameter to be parsed
#'
#' @return integer
#' @export
#'
parse_parameters_lgl <- function(param) {

  if (!is.null(param)) {
    param <- parse_parameters_chr(param)
    param <- as.logical(param)
  }

  return(param)
}

#' check_parameters
#' Check whether a parsed parameter is valid or not
#' @param req env: plumber request environment
#' @param res evn: plumber response environment
#' @param param character or numeric: Parsed parameters
#' @param valid_values character or numeric: Accepted values for this parameter
#'
#' @return logical
#' @export
#'
check_parameters <- function(req, res, param, valid_values) {
  if (!is.null(req$args[[param]]) && !any(req$args[[param]] %in% valid_values)) {
    res$status <- 400
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' format_error
#' Format error if check_parameters() returns TRUE
#' @param param character or numeric: Parsed parameters
#' @param valid_values character or numeric: Accepted values for this parameter
#'
#' @return list
#' @export
#'
format_error <- function(param, valid_values) {
  out <- list(
    error = paste(
      "Invalid value for ", param, ". Please use one of",
      paste(
        "'", valid_values, "'",
        sep = "", collapse = ", "
      )
    )
  )
}
