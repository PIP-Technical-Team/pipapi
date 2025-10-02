#' Check validity of query parameters
#'
#' @param req req: plumber request environment
#' @param query_controls list: List of valid values
#' @return logical
#' @noRd
check_parameters_values <- function(req, query_controls) {
  out <- lapply(seq_along(req$argsQuery),
                \(i) {
                    param_name <- names(req$argsQuery)[i]
                    check_parameter_values(
                      values = req$argsQuery[[i]],
                      valid_values = query_controls[[param_name]][["values"]],
                      type = query_controls[[param_name]][["type"]])
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
check_parameter_values <- function(values, valid_values, type) {
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
#' Format error if check_parameters_values() returns TRUE
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
    "long_format",
    "additional_ind",
    "cum_welfare",
    "cum_population",
    "requested_mean",
    "mean",
    "times_mean",
    "lorenz",
    "n_bins",
    "pass",
    "type",
    "exclude"
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
  if (param_name %in% c("country",
                        "fill_gaps",
                        "version",
                        "aggregate",
                        "long_format",
                        "additional_ind")) {

    param <- toupper(param)
    if (param_name == "country") {
      if (length(param[param == "ALL"]) > 0) {
        param[param == "ALL"] <- "all"
      }
    }
  } else {
    param <- tolower(param)
  }

  param <- utils::type.convert(param, as.is = TRUE)

  return(param)
}

#' Assign PIP API required parameters if missing
#'
#' @param req list: plumber `req` object
#' @param pl_lkup data.frame: Poverty lines lookup table
#'
#' @return list
#' @noRd
assign_required_params <- function(req, pl_lkup) {

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
      req$args$fill_gaps <- NULL
      req$argsQuery$fill_gaps <- NULL
      if (is.null(req$args$group_by)) {
        req$args$group_by <- "none"
        req$argsQuery$group_by <- "none"
      }
    }
  }

  # Turn all country codes to upper case
  if (!is.null(req$args$country)) {
  req$args$country <- toupper(req$args$country)
  req$argsQuery$country <- toupper(req$argsQuery$country)
  }
  # Turn all year codes to upper case
  if (!is.null(req$args$year)) {
  req$args$year <- toupper(req$args$year)
  req$argsQuery$year <- toupper(req$argsQuery$year)
  }

  # Handle default poverty line
  if (endpoint %in% c("pip",
                      "pip-grp",
                      "hp-stacked",
                      "pc-charts",
                      "pc-download",
                      "pc-regional-aggregates",
                      "cp-key-indicators",
                      "cp-charts",
                      "cp-download")) {
    if (is.null(req$args$povline)) {
      req$args$povline <- pl_lkup$poverty_line[pl_lkup$is_default == TRUE]
      req$argsQuery$povline <- pl_lkup$poverty_line[pl_lkup$is_default == TRUE]
    }
  }

  # Handle long_format argument for /aux endpoint
  # Behavior: long_format argument will be forced to FALSE is the selected
  # table is not suppported for long format
  # Long format of tables
  if (endpoint == "aux") {
    # If no table is defined
    if (is.null(req$argsQuery$table)) {
      req$argsQuery$long_format <- FALSE
    }

    # If long format is not selected
    if (is.null(req$argsQuery$long_format)) {

      # Check if belongs to list of tables available in long format
      if (req$argsQuery$table %in%
          pipapi::get_valid_aux_long_format_tables()) {
        req$argsQuery$long_format <- TRUE
      } else {
        req$argsQuery$long_format <- FALSE
      }
      # end of if NULL long_format
    } else {
      req$argsQuery$long_format <- as.logical(req$argsQuery$long_format)
    }
  } # end of aux endpoint

  if (endpoint == "ui_aux") {
    if (is.null(req$argsQuery$table)) {
      req$argsQuery$exclude <- FALSE
    }

    # manage exclude paramter
    if (is.null(req$argsQuery$exclude)) {
      req$argsQuery$exclude <- FALSE
    } else {
      req$argsQuery$exclude <- as.logical(req$argsQuery$exclude)
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
#' @param versions character: All available versions
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
  current_date <- Sys.Date()
  current_year <- format(current_date, '%Y')
  # release_date <- extract_release_date(version)
  # ppp_date <- extract_ppp_date(version)
  citation <- sprintf('World Bank (%s), Poverty and Inequality Platform (version %s) [data set]. pip.worldbank.org. Accessed on %s',
                      current_year,
                      version,
                      current_date)

  return(list(
    citation = citation,
    version_id  = version,
    date_accessed = current_date
  )
  )
}

#' create_etag_header
#'
#' helper function that creates a unique hash of code + data
#' this hash value will be used as the value of the etag header
#' to facilitate caching of PIP API responses
#'
#' @param req R6 object: Plumber API request
#' @param lkups list: pipapi master lkups
#'
#' @return character
#'
#' @export

create_etag_header <- function(req, lkups){
  lkup_hash   <- lkups$versions_paths[[req$argsQuery$version]]
  pipapi_hash <- utils::packageDescription("pipapi")$GithubSHA1
  wbpip_hash  <- utils::packageDescription("wbpip")$GithubSHA1

  etag_hash <- rlang::hash(c(lkup_hash, pipapi_hash, wbpip_hash))

  return(etag_hash)
}

#' Helper function to return correct serializer
#'
#' @param format characer: Response format. Options are "json", "csv", or "rds"
#'
#' @return serializer function
#' @export
#'

assign_serializer <- function(format) {
  # json as default format
  if (is.null(format)) {
    format <- "json"
  }
  # List of supported serializers
  serializers <- list(
    "json"    = plumber::serializer_json(na = "null"),
    "csv"     = plumber::serializer_csv(na = ""),
    "rds"     = plumber::serializer_rds(),
    "arrow"   = plumber::serializer_feather()
  )

  return(serializers[[format]])
}

#' Helper function to determine whether an API call is compute intensive
#' and should be forked to a parallel process to avoid blocking the main
#' R process
#'
#' @param country character: selected countries
#' @param year character: selected years
#' @param intensity_threshold numeric: Number of selected country/year above which
#' the request will be considered intensive
#' @param include_year logical: Whether year selection should be included to determine
#' the intensity of the request
#'
#' @return logical
#' @export
#'

is_forked <- function(country,
                      year,
                      intensity_threshold = 40,
                      include_year = TRUE) {

  is_country_intensive <- any(country %in% c("ALL", "WLD") |
                                length(country) > intensity_threshold)
  if (include_year) {
    is_year_intensive <- any(year %in% c("ALL") |
                               length(year) > intensity_threshold)
  } else {
    is_year_intensive <- TRUE
  }

  is_intensive <- is_country_intensive & is_year_intensive

  return(is_intensive)
}


#' Validate grouped-stats endpoint input values
#' @param welfare character: query values
#' @param population character: valid values
#' @param max_length integer: Max length of welfare vector
#' @return list of two vectors welfare and population
#' @export
validate_input_grouped_stats <- function(welfare, population, max_length = 100) {
  welfare    <- parse_parameter(welfare,"welfare")
  population <- parse_parameter(population,"population")
  lw         <- length(welfare)
  # Only allow vector of length 100 and ensure the length of two vectors is same
  correct <- lw > 0 && lw <= max_length && lw == length(population)
  if (correct) {
    return(list(welfare = welfare, population = population))
  } else {
    return(NULL)
  }
}

#' Return output format for regression-params endpoint
#' @param vals list: Regression result values
#' @return dataframe
#' @noRd
return_output_regression_params <- function(vals) {
  # Convert standard error values into a matrix with 3 columns, named for ease
  # of understanding
  se_val <- matrix(vals$reg_results$se,
                   ncol = 3,
                   dimnames = list(NULL, c("se_A", "se_B", "se_C")))
  # Transpose coefficient values to make each coefficient a row instead of a
  # column
  coef_val <- t(vals$reg_results$coef)
  # Remove coefficient and standard error elements from the results to avoid
  # redundancy
  vals$reg_results$coef <- vals$reg_results$se <- NULL
  # Combine coefficient values, other regression results, standard errors into
  # a single dataframe and add columns for validity and normality checks from
  # the 'validity' sublist
  cbind(coef_val,
        do.call(cbind.data.frame, vals$reg_results),
        se_val,
        validity = vals$validity$is_valid,
        normality = vals$validity$is_normal)
}


#' Change the list-output to dataframe
#'
#' @param out output from wbpip::gd_compute_pip_stats
#'
#' @return dataframe
#' @export
change_grouped_stats_to_csv <- function(out) {
  out[paste0("decile", seq_along(out$deciles))] <- out$deciles
  out$deciles <- NULL
  data.frame(out)
}





#' Wrap a Plumber endpoint with standardized error handling
#'
#' `safe_endpoint()` wraps an endpoint handler in a `tryCatch`, ensuring
#' consistent error handling across the API. On success, the original
#' handler's result is returned. On error, a structured JSON object is
#' returned with useful metadata (status, message, request ID, endpoint),
#' and optionally additional debug details.
#'
#' Debug mode can be enabled by either:
#' \itemize{
#'   \item Passing `debug = TRUE` explicitly, or
#'   \item Setting the environment variable `PIPAPI_DEBUG=TRUE`.
#' }
#' When debug mode is active, the error payload also includes the error
#' class, call, query parameters, and a truncated traceback.
#'
#' @param fun A function `(req, res)` containing the endpoint logic.
#'   This is where you parse request arguments and call the relevant
#'   internal functions.
#' @param endpoint Character string giving the endpoint path
#'   (e.g., `"/api/v1/pip"`). Used in error payloads so clients know
#'   which endpoint failed.
#' @param debug Logical; if `NULL` (default), inherits from the
#'   environment variable `PIPAPI_DEBUG`. When `TRUE`, include extended
#'   diagnostic details in the error response.
#'
#' @return A function `(req, res)` suitable for use in Plumber routes.
#'   On error, sets `res$status <- 500` and returns a JSON object with:
#'   \describe{
#'     \item{error}{A short description ("Error in /api/v1/...")}
#'     \item{message}{Either the actual error message (debug mode) or
#'       `"Internal Server Error"`}
#'     \item{request_id}{The Plumber request ID, if available}
#'     \item{endpoint}{The endpoint string supplied}
#'     \item{class}{Error class (debug mode only)}
#'     \item{call}{The call that generated the error (debug mode only)}
#'     \item{query}{The query parameters (debug mode only)}
#'     \item{trace}{Traceback captured by `rlang::trace_back()` (debug mode only)}
#'   }
#'
#' @examples
#' \dontrun{
#' # Example: wrap a handler for /api/v1/pip
#' #* @get /api/v1/pip
#' function(req, res) {
#'   safe_endpoint(function(req, res) {
#'     params <- req$argsQuery
#'     params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
#'     params$version <- NULL
#'     do.call(pipapi::ui_pip, params)
#'   }, endpoint = "/api/v1/pip")(req, res)
#' }
#' }
#'
#' @export
safe_endpoint <- function(fun, endpoint, debug = NULL) {
  if (is.null(debug)) {
    debug <- identical(Sys.getenv("PIPAPI_DEBUG"), "TRUE")
  }

  function(req, res) {
    tryCatch(
      {
        fun(req, res)
      },
      error = function(e) {
        res$status <- 500L
        out <- list(
          error      = paste("Error in", endpoint),
          message    = if (debug) conditionMessage(e) else "Internal Server Error",
          request_id = tryCatch(req$.id, error = \(.) NA),
          endpoint   = endpoint
        )
        if (debug) {
          out$class <- class(e)[[1]]
          out$call  <- as.character(conditionCall(e))
          out$query <- req$argsQuery
          out$trace <- utils::capture.output(
            rlang::trace_back(bottom = 10, simplify = "branch")
          )
        }
        out
      }
    )
  }
}

# ---- bounded execution helper -------------------------------------------
#' Evaluate an expression with a timeout
#'
#' Wraps [R.utils::withTimeout()] but returns a structured failure
#' object instead of stopping the whole process. This allows
#' `safe_endpoint()` to handle timeouts like normal errors without
#' killing the API process.
#'
#' @param expr Expression to evaluate.
#' @param secs Timeout in seconds (default: from env var `PLUMBER_REQ_TIMEOUT`,
#'   or 150 if unset).
#'
#' @return Result of `expr` if it finishes in time; otherwise a list
#'   with `ok = FALSE`, `error = "timeout"`, and `elapsed` seconds.
#' @export
with_req_timeout <- function(expr,
                             secs = as.numeric(Sys.getenv("PLUMBER_REQ_TIMEOUT", "150"))) {
  if (!is.finite(secs) || secs <= 0) return(force(expr))

  start <- proc.time()[["elapsed"]]
  tryCatch(
    {
      R.utils::withTimeout(
        expr     = force(expr),
        timeout  = secs,
        onTimeout = "error"
      )

    },
    TimeoutException = \(e) {
      elapsed <- proc.time()[["elapsed"]] - start
      list(
        ok     = FALSE,
        error  = sprintf("Request exceeded timeout of %s seconds", secs),
        elapsed = elapsed
      )
    }
  )
}


#' Normalize API query arguments for caching
#'
#' Ensures that:
#' - All keys are sorted alphabetically (so order of args never matters)
#' - NULL or empty arguments are dropped
#' - Numeric poverty lines are rounded consistently
#' - Logical arguments are coerced to TRUE/FALSE
#' - Character vectors are deduplicated and sorted (case preserved)
#' - Nested lists are normalized recursively
#'
#' @param params List of query parameters (typically req$argsQuery)
#' @param skip character vector of arguments to exclude
#' @param round_digits Integer, how many digits to round numeric poverty lines
#'
#' @return A cleaned and normalized list, safe for memoise/cache keys
normalize_args <- function(params, round_digits = 2L, skip = c("lkup")) {
  if (is.null(params) || length(params) == 0L) return(list())

  out <- as.list(params)

  # Drop NULL / empty
  out <- out[!vapply(out, function(x) is.null(x) || length(x) == 0L, logical(1))]

  # Round poverty line (if present)
  if (!is.null(out$povline)) {
    suppressWarnings({
      out$povline <- round(as.numeric(out$povline), round_digits)
    })
  }

  # Normalize each arg, but *not* the ones we skip (e.g. lkup)
  nms <- names(out)
  for (i in seq_along(out)) {
    nm <- nms[[i]]
    x  <- out[[i]]

    if (nm %in% skip) next

    # coerce common logical forms
    if (length(x) == 1L && is.character(x)) {
      lx <- tolower(x)
      if (lx %in% c("true","false")) { out[[i]] <- (lx == "true"); next }
    }
    if (length(x) == 1L && is.numeric(x) && x %in% c(0,1)) {
      out[[i]] <- as.logical(x)
      next
    }

    # character vectors: dedup + sort (preserve case)
    if (is.character(x) && length(x) > 1L) {
      out[[i]] <- unique(sort(x))
      next
    }

    # nested lists: normalize recursively (still skipping keys named in `skip`)
    if (is.list(x)) {
      out[[i]] <- normalize_args(x, round_digits = round_digits, skip = skip)
    }
  }

  # Deterministic key order
  out[sort(names(out))]
}

# ---- memoization wrapper (idempotent) ------------------------------------
#' momiose and normalize
#'
#' @param f function to normalize
#' @param cache object from [cachem::cache_disk]
#'
#' @return memoised function
memo_norm <- function(f, cache) {
  # If already memoised, leave it alone
  if (memoise::is.memoised(f)) return(f)

  memoise::memoise(
    function(...) {
      args <- normalize_args(list(...))
      do.call(f, args)
    },
    cache = cache,
    omit_args = "lkup"
  )
}
