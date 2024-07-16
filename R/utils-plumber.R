#' Check validity of query parameters
#'
#' @param req req: plumber request environment
#' @param query_controls list: List of valid values
#' @return logical
#' @noRd
check_parameters_values <- function(req, query_controls) {
  out <- lapply(seq_along(req$argsQuery), function(i) {
    param_name <- names(req$argsQuery)[i]
    check_parameter_values(
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
    "n_bins"
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
change_grouped_stats_to_csv <- function(out) {
  out[paste0("decile", seq_along(out$deciles))] <- out$deciles
  out$deciles <- NULL
  data.frame(out)
}
