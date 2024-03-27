#* @apiTitle PIP API
#* @apiDescription This API powers computations of statistics available at
#* pip.worldbank.org

library(pipapi)

# API filters -------------------------------------------------------------
## Validate version parameter ----

#* Ensure that version parameter is correct
#* @filter validate_version
function(req, res) {
# browser()
  ### STEP 1:
  # If no arguments are passed, use the latest version
  if (is.null(req$argsQuery$release_version) &
      is.null(req$argsQuery$ppp_version) &
      is.null(req$argsQuery$version) &
      is.null(req$argsQuery$identity)) {
      version <- lkups$latest_release
  } else {
  ### STEP 2:
  # If partial version information is passed, use selection algorithm
    if (is.null(req$argsQuery$identity)) {
      req$argsQuery$identity <- 'PROD'
    }

    version <-
      pipapi::return_correct_version(req$argsQuery$version,
                                     req$argsQuery$release_version,
                                     req$argsQuery$ppp_version,
                                     req$argsQuery$identity, lkups$versions)
  }
  ### STEP 3:
  # If the version is still not found (404) or it is not present in valid versions
    # vector return an error.
    if (!version %in% lkups$versions) {
        res$status <- 404
        out <- list(
          error = "Invalid query arguments have been submitted.",
          details = list(msg = "The selected value is not available.
                         Please select one of the valid values",
                         valid = pipapi::version_dataframe(lkups$versions)))
        return(out)
    } else {
      req$argsQuery$version <- version
    }

  plumber::forward()
}

## Filter out invalid parameters ----
#* Ensure that only valid parameters are being forwarded
#* @filter validate_query_parameters
function(req, res) {
  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    req$argsQuery <- pipapi:::validate_query_parameters(req)
  }
  plumber::forward()
}

## Parse query parameters ----
#* Parse query parameters of incoming request
#* @filter parse_parameters
function(req, res) {
  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    req$argsQuery <- pipapi:::parse_parameters(req$argsQuery)
  }
  plumber::forward()
}

## Validate parameter values ----
#* Protect against invalid arguments
#* @filter check_parameters_values
function(req, res) {
  lkups <- lkups$versions_paths[[req$argsQuery$version]]
  query_controls = lkups$query_controls

  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    #### STEP 1
    # Assign required parameters
    # Non-provided parameters are typically assigned the underlying function
    # arguments' default values. There are two exceptions to that however:
    # 1) The `country` & `year` parameters cannot be NULL in order to pass
    # the if condition that will decide whether or no the request should be
    # treated asynchronously.
    # 2) The introduction of PPP versioning implies having a dynamic default
    # poverty line

    req <- pipapi:::assign_required_params(req,
                                           pl_lkup = lkups$pl_lkup)

    ### STEP 2
    # Validate individual query parameters
    are_valid <- pipapi:::check_parameters_values(req, query_controls)
    if (any(are_valid == FALSE)) {
      res$status <- 404
      invalid_params <- names(req$argsQuery)[!are_valid]
      out <- pipapi:::format_error(invalid_params, query_controls)
      return(out)
    }
    ### STEP 3
    #Check for invalid combinations of query parameter values
    # Break if bad combination of country/region and grouping
    endpoint <- pipapi:::extract_endpoint(req$PATH_INFO)
    if (endpoint == "pip-grp") {
      group_condition   <- req$argsQuery$group_by != "none"
      country_condition <- !all(req$argsQuery$country %in% query_controls$region$values)
      if (group_condition & country_condition) {
        res$status <- 400
        invalid_params <- "region"
        out <- pipapi:::format_error("region", query_controls)
        out$error <- "You supplied an invalid value for country. Please use one of the valid values."
        return(out)
      }
    }

    # Break if bad combination of table and long_format
    if (endpoint == "aux") {
      if (isTRUE(req$argsQuery$long_format) &
          !is.null(req$argsQuery$table)) {

        if (!req$argsQuery$table %in% pipapi::get_valid_aux_long_format_tables()) {

          res$status <- 404
          out <- list(
            error = "Invalid query arguments have been submitted.",
            details = list(msg = "The selected table is not available in long format. Please select one of the valid values",
                         valid = pipapi::get_valid_aux_long_format_tables()))
          return(out)
        }
      }
    }

    ### STEP 4
    # Round poverty line
    # This is to prevent users to abuse the API by passing too many decimals
    if (!is.null(req$argsQuery$povline)) {
      req$argsQuery$povline <- round(req$argsQuery$povline, digits = 3)
    }
  }
  plumber::forward()
}

## Set response headers ----
#* Set required response headers
#* @filter response_headers
function(req, res) {
  res$setHeader("Strict-Transport-Security",
                "max-age=63072000; includeSubDomains; preload")

  res$setHeader("Content-Security-Policy",
                "default-src *  data: blob: filesystem: about: ws: wss: 'unsafe-inline' 'unsafe-eval' 'unsafe-dynamic'; script-src * data: blob: 'unsafe-inline' 'unsafe-eval'; connect-src * data: blob: 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src * data: blob: ; style-src * data: blob: 'unsafe-inline'; font-src * data: blob: 'unsafe-inline';")

  res$setHeader("X-Frame-Options",
                "SAMEORIGIN")

  res$setHeader("X-Content-Type-Options",
                "nosniff")

  res$setHeader("Referrer-Policy",
                "no-referrer")

  res$setHeader("Access-Control-Allow-Origin",
                "*")
  # Set max-age to 48hours (specified in seconds)
  # res$setHeader("Cache-Control",
  #               "max-age=172800")

  res$setHeader("ETag",
                pipapi::create_etag_header(req,
                                           lkups = lkups))

  plumber::forward()

  # Logging
  # end_filters <- tictoc::toc(quiet = TRUE)
  # logger::log_info('filters: {round(end_filters$toc - end_filters$tic, digits = getOption("digits", 6))}')

}

# Endpoints definition ----------------------------------------------------
## Endpoints: Core endpoints ----

#* Return PIP information
#* @get /api/v1/pip-info
function(req) {
  pipapi::get_pip_version(pip_packages = c("pipapi",
                                           "wbpip"),
                          data_versions = lkups$versions)
}

#* Return valid parameters
#* @get /api/v1/valid-params
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param format:[chr] Response format. Options are "json", "csv", "rds", or "arrow".
#* @param endpoint:[chr] Endpoint for which to return the valid parameters
function(req, res) {
  format <- req$argsQuery$format
  res$serializer <- pipapi::assign_serializer(format = format)
  version <- req$argsQuery$version
  endpoint <- req$argsQuery$endpoint

  out <- pipapi::get_param_values(
    lkup = lkups,
    version = version,
    endpoint = endpoint)
  out
}

#* Return main poverty and inequality statistics
#* @get /api/v1/pip
#* @param country:[chr] Country ISO3 code
#* @param year:[chr] Year
#* @param povline:[dbl] Poverty Line
#* @param popshare:[dbl] Share of the population living below the poverty Line.
#* @param fill_gaps:[bool] Fill gaps for years with no available surveys.
#* @param group_by:[chr] Triggers sub-groups aggregation.
#* @param welfare_type:[chr] Welfare Type. Options are "income" or "consumption"
#* @param reporting_level:[chr] Reporting level. Options are "national", "urban", "rural".
#* @param ppp:[dbl] Custom Purchase Power Parity (PPP) value.
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param identity:[chr] One of "PROD" (production) (default), "INT" (internal) and "TEST"
#* @param format:[chr] Response format. Options are "json", "csv", "rds", or "arrow".
#* @param additional_ind:[bool] Additional indicators based on standard PIP output.
#* Default is FALSE

function(req, res) {
  # Process request
  params         <- req$argsQuery
  params$lkup    <- lkups$versions_paths[[params$version]]
  res$serializer <- pipapi::assign_serializer(format = params$format)
  params$format  <- NULL
  params$version <- NULL

  # Parallel processing for slow requests
  if (is_forked(country = params$country, year = params$year)) {
    out <- promises::future_promise({
      tmp <- do.call(pipapi::pip, params)
      tmp
    }, seed = TRUE)
  } else {
    out <- do.call(pipapi::pip, params)
  }
  out
}

#* Return aggregated poverty and inequality statistics
#* @get /api/v1/pip-grp
#* @param country:[chr] Country ISO3 code
#* @param year:[chr] Year
#* @param povline:[dbl] Poverty Line
#* @param group_by:[chr] Select type of aggregation (simple weighted average or a pre-defined subgroup)
#* @param welfare_type:[chr] Welfare Type. Options are "income" or "consumption"
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param format:[chr] Response format. Options are "json", "csv", "rds", or "arrow".
#* for all available versions
#* @param additional_ind:[bool] Additional indicators based on standard PIP output.
#* Default is FALSE
function(req, res) {
  # Process request
  params         <- req$argsQuery
  params$lkup    <- lkups$versions_paths[[params$version]]
  res$serializer <- pipapi::assign_serializer(format = params$format)
  params$format  <- NULL
  params$version <- NULL

  # Parallel processing for slow requests
  if (is_forked(country = params$country, year = params$year)) {
    out <- promises::future_promise({
      tmp <- do.call(pipapi::pip_grp_logic, params)
      tmp
    }, seed = TRUE)
  } else {
    out <- do.call(pipapi::pip_grp_logic, params)
  }
  out
}

#* Return auxiliary data table
#* @get /api/v1/aux
#* @param table:[chr] Auxiliary data table to be returned
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param long_format:[bool] Data in long format
#* @param format:[chr] Response format. Options are "json", "csv", "rds", or "arrow".
function(req, res) {
  params <- req$argsQuery
  res$serializer <- pipapi::assign_serializer(format = params$format)

  if (is.null(req$args$table)) {
    # return all available tables if none selected
    list_of_tables <- lkups$versions_paths[[params$version]]$aux_tables
    out <- data.frame(tables = list_of_tables)
  } else {
    # Return only requested table
    params$data_dir <- lkups$versions_paths[[params$version]]$data_root
    params$format <- NULL
    params$version <- NULL
    out <- do.call(pipapi::get_aux_table, params)
  }
  out
}

#* Return available data versions
#* @get /api/v1/versions
function(req, res) {
  res$serializer <- pipapi::assign_serializer(format = req$argsQuery$format)
  out <- pipapi::version_dataframe(lkups$versions)
  out
}

#* Return information about a specific data version
#* @get /api/v1/version
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
function(req, res) {
  res$serializer <- pipapi::assign_serializer(format = req$argsQuery$format)
  out <- pipapi::version_dataframe(lkups$versions)
  out <- out[out$version == req$argsQuery$version, ]
  out
}

## Endpoints: Cache endpoints ----

#* Reset current cache directory
#* @get /api/v1/cache-reset
#* @serializer unboxedJSON
function() {
  pipapi:::clear_cache(cd)
}

#* Delete current cache directory
#* @get /api/v1/cache-delete
#* @serializer unboxedJSON
function() {
  unlink(cd$info()$dir, recursive = TRUE)
}

#* Get the cached value from a specified key
#* @get /api/v1/cache-get
#* @param key: [chr] key corresponding to a specific cached value
#* @serializer unboxedJSON
function(key) {
  cd$get(key)
}

#* Return all keys from the cache
#* @get /api/v1/cache-keys
#* @serializer unboxedJSON
function(key) {
  cd$keys()
}

#* Return cache info
#* @get /api/v1/cache-info
#* @serializer unboxedJSON
function() {
  if (!cd$is_destroyed()) {
    info <- cd$info()
    info$missing <- NULL
    c(n_items = cd$size(), info)
  }
}

# #* Return cache log
# #* @get /api/v1/cache-log
# #* @serializer print list(quote = FALSE)
# function(){
#   if (!cd$is_destroyed()) {
#     readLines(cd$info()$logfile)
#   }
# }

## Endpoints: Miscellaneous ----

#* Check status of API
#* @get /api/v1/health-check
function() {
  "PIP API is running"
}

#* Check timestamp for the data
#* @get /api/v1/data-timestamp
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer unboxedJSON
function(req) {
  dir <- lkups$versions_paths[[req$argsQuery$version]]$data_root
  readLines(sprintf("%s/data_update_timestamp.txt", dir))
}

#* Retrieve data signature hash
#* @get /api/v1/data-signature
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer unboxedJSON
function(req) {
  lkups$versions_paths[[req$argsQuery$version]]$cache_data_id
}

#* Get information on directory contents
#* @get /api/v1/dir-info
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
function(req) {
  dir <- lkups$versions_paths[[req$argsQuery$version]]$data_root
  x <- fs::dir_info(dir, recurse = TRUE, type = "file")
  x$file <- sub(".*/", "", x$path)
  x <- x[c("path", "file", "size", "birth_time", "modification_time", "change_time", "access_time")]
  list(
    aux_files = x[grepl("aux", x$path),],
    estimation_files = x[grepl("estimation", x$path),],
    survey_data = x[grepl("survey_data", x$path),]
  )
}

#* Check Github hash for the PIP packages
#* @get /api/v1/gh-hash
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer unboxedJSON
function(req) {
  list(pipapi = packageDescription("pipapi")$GithubSHA1,
       wbpip = packageDescription("wbpip")$GithubSHA1)
}

#* Return number of workers
#* @get /api/v1/n-workers
#* @serializer unboxedJSON
function() {
  list(
    n_cores = unname(future::availableCores()),
    n_workers = future::nbrOfWorkers(),
    n_free_workers = future::nbrOfFreeWorkers()
  )
}

# #* Return system info
# #* @get /api/v1/system-info
# function(){
#   Sys.info()
# }

# #* Return working directory
# #* @get /api/v1/get-root
# function() {
#   getwd()
# }

# #* Return available objects
# #* @get /api/v1/get-available-objects
# function() {
#   list(
#     search_list = search(),
#     global    = ls(pos = 1),
#     parent    = ls(pos = 2),
#     gd_parent = ls(pos = 3)
#   )
# }

# UI endpoints ----

## UI endpoints: Homepage --------------------------------------------------

#* Return poverty lines for home page display
#* @get /api/v1/poverty-lines
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
function(req) {
  pipapi::get_aux_table(data_dir = lkups$versions_paths[[req$argsQuery$version]]$data_root,
                        table = "poverty_lines")
}

#* Return indicators master table
#* @get /api/v1/indicators
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json list(na="null")
function(req) {
  pipapi::get_aux_table(data_dir = lkups$versions_paths[[req$argsQuery$version]]$data_root,
                        table = "indicators")
}

#* Return list of variables used for decomposition
#* @get /api/v1/decomposition-vars
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
function(req) {
  pipapi::get_aux_table(data_dir = lkups$versions_paths[[req$argsQuery$version]]$data_root,
                        table = "decomposition_master")
}

#* Return data for home page main chart
#* @get /api/v1/hp-stacked
#* @param povline:[dbl] Poverty Line
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  promises::future_promise({
    do.call(pipapi::ui_hp_stacked, params)
  }, seed = TRUE)
}

#* Return data for home page country charts
#* @get /api/v1/hp-countries
#* @param povline:[dbl] Poverty Line
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  do.call(pipapi::ui_hp_countries, params)
}


## UI Endpoints: Poverty calculator ----------------------------------------

#* Return data for Poverty Calculator main chart
#* @get /api/v1/pc-charts
#* @param country:[chr] Country ISO3 code
#* @param year:[chr] Year
#* @param povline:[dbl] Poverty Line
#* @param fill_gaps:[bool] Fill gaps for years with no available surveys
#* @param group_by:[chr] Triggers sub-groups aggregation
#* @param welfare_type:[chr] Welfare Type. Options are "income" or "consumption"
#* @param reporting_level:[chr] Reporting level. Options are "all", national", "urban", "rural".
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json list(na = "null")
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  if (is_forked(country = params$country, year = params$year)) {
    out <- promises::future_promise({
      do.call(pipapi::ui_pc_charts, params)
    }, seed = TRUE)
  } else {
    out <- do.call(pipapi::ui_pc_charts, params)
  }
  return(out)
}

#* Return data for Poverty Calculator download
#* @get /api/v1/pc-download
#* @param country:[chr] Country ISO3 code
#* @param year:[chr] Year
#* @param povline:[dbl] Poverty Line
#* @param fill_gaps:[bool] Fill gaps for years with no available surveys
#* @param group_by:[chr] Triggers sub-groups aggregation
#* @param welfare_type:[chr] Welfare Type. Options are "income" or "consumption"
#* @param reporting_level:[chr] Reporting level. Options are "all", national", "urban", "rural".
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer csv
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$pop_units <- 1
  params$version <- NULL
  if (is_forked(country = params$country, year = params$year)) {
    promises::future_promise({
      do.call(pipapi::ui_pc_charts, params)
    }, seed = TRUE)
  } else {
    do.call(pipapi::ui_pc_charts, params)
  }
}

#* Return regional aggregations for all years
#* @get /api/v1/pc-regional-aggregates
#* @param country:[chr] Region code
#* @param year:[chr] Year
#* @param povline:[dbl] Poverty Line
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  promises::future_promise({
    do.call(pipapi::ui_pc_regional, params)
  }, seed = TRUE)
}

## UI Endpoints: Country Profiles ----------------------------------------

#* Return Country Profile - Key Indicators
#* @get /api/v1/cp-key-indicators
#* @param country:[chr] Country ISO3 code
#* @param povline:[dbl] Poverty Line
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json list(na="null")
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  do.call(pipapi::ui_cp_key_indicators, params)
}


#* Return Country Profile - Charts
#* @get /api/v1/cp-charts
#* @param country:[chr] Country ISO3 code
#* @param povline:[dbl] Poverty Line
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]

  params$version <- NULL
    do.call(pipapi::ui_cp_charts, params)
}

#* Return Country Profile - Downloads
#* @get /api/v1/cp-download
#* @param country:[chr] Country ISO3 code
#* @param povline:[dbl] Poverty Line
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
function(req, res) {
  params <- req$argsQuery
  res$serializer <- pipapi::assign_serializer(format = params$format)
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  params$format  <- NULL

  if (is_forked(country = params$country, include_year = FALSE)) {
    out <- promises::future_promise({
      tmp <- do.call(pipapi::ui_cp_download, params)
      tmp
    }, seed = TRUE)
  } else {
    out <- do.call(pipapi::ui_cp_download, params)
  }
  out
}

## UI endpoints: Miscellaneous ----

#* Return auxiliary data table
#* @get /api/v1/ui_aux
#* @param table:[chr] Auxiliary data table to be returned
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param format:[chr] Response format. Options are "json", "csv", "rds", or "arrow".
function(req, res) {
  params <- req$argsQuery
  res$serializer <- pipapi::assign_serializer(format = params$format)
  if (is.null(req$args$table)) {
    # return all available tables if none selected
    list_of_tables <- lkups$versions_paths[[params$version]]$aux_tables
    out <- data.frame(tables = list_of_tables)
  } else {
    # Return only requested table
    params$data_dir <- lkups$versions_paths[[params$version]]$data_root
    params$format <- NULL
    params$version <- NULL
    out <- do.call(pipapi::get_aux_table_ui, params)
  }
  out
}

#* Return metadata for the Data Sources page
#* @get /api/v1/survey-metadata
#* @param country:[chr] Country ISO3 code
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json list(na="null")
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  out <- do.call(pipapi::ui_svy_meta, params)
  out
}

#* Return valid years
#* @get /api/v1/valid-years
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json list(na="null")
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[params$version]]
  out <- pipapi::valid_years(data_dir = params$lkup$data_root)
  out
}


#* Return citation
#* @get /api/v1/citation
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @serializer json
function(req, res) {
  # Remove browser cache of citation endpoint as it returns the current date
  res$headers["Cache-Control"] <- NULL
  params <- req$argsQuery
  out <- pipapi::citation_from_version(params$version)
  out
}
