#* @apiTitle PIP API
#* @apiDescription This API powers computations of statistics available at
#* pip.worldbank.org

library(pipapi)

# API filters -------------------------------------------------------------

#* Ensure that version parameter is correct
#* @filter validate_version
function(req, res) {

  # STEP 1 - If no arguments are passed, use the latest version
  if (is.null(req$argsQuery$release_version) & is.null(req$argsQuery$ppp_version) &
     is.null(req$argsQuery$version) & is.null(req$argsQuery$identity)) {
      version <- lkups$latest_release
  # STEP 2 - If partial version information is passed, use selection algorithm
  } else {
    if (is.null(req$argsQuery$identity)) req$argsQuery$identity <- 'PROD'
    version <- pipapi::return_correct_version(req$argsQuery$version,
                                              req$argsQuery$release_version,
                                              req$argsQuery$ppp_version,
                                              req$argsQuery$identity, lkups$versions)
  }
    # If the version is not found (404) or it is not present in valid versions vector return an error.
    if (!version %in% lkups$versions) {
        res$status <- 404
        out <- list(
          error = "Invalid query arguments have been submitted.",
          details = list(msg = "The selected value is not available. Please select one of the valid values",
                         valid = pipapi::version_dataframe(lkups$versions)))
        return(out)
    } else req$argsQuery$version <- version

  plumber::forward()
}

#* Ensure that only valid parameters are being forwarded
#* @filter validate_query_parameters
function(req, res) {
  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    req$argsQuery <- pipapi:::validate_query_parameters(req)
  }
  plumber::forward()
}

#* Parse query parameters of incoming request
#* @filter parse_parameters
function(req, res) {
  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    req$argsQuery <- pipapi:::parse_parameters(req$argsQuery)
  }
  plumber::forward()
}

#* Protect against invalid arguments
#* @filter check_parameters
function(req, res) {
  lkups <- lkups$versions_paths[[req$argsQuery$version]]
  query_controls = lkups$query_controls

  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    # STEP 1: Assign required parameters
    # Non-provided parameters are typically assigned the underlying function
    # arguments' default values. There are two exceptions to that however:
    # 1) The `country` & `year` parameters cannot be NULL in order for to pass
    # the if condition that will decide whether or no the request should be
    # treated asynchronously.
    # 2) The introduction of PPP versioning implies having a dynamic default
    # poverty line

    req <- pipapi:::assign_required_params(req,
                                           pl_lkup = lkups$pl_lkup)

    # STEP 2: Validate individual query parameters
    are_valid <- pipapi:::check_parameters(req, query_controls)
    if (any(are_valid == FALSE)) {
      res$status <- 404
      invalid_params <- names(req$argsQuery)[!are_valid]
      out <- pipapi:::format_error(invalid_params, query_controls)
      return(out)
    }
    # STEP 3: Check for invalid combinations of query parameter values
    # Break if bad request
    endpoint <- pipapi:::extract_endpoint(req$PATH_INFO)
    if (endpoint == "pip-grp") {
      group_condition   <- req$argsQuery$group_by != "none"
      country_condition <- !all(req$argsQuery$country %in% query_controls$region$values)
      if (group_condition & country_condition) {
        res$status <- 400
        invalid_params <- "region"
        out <- pipapi:::format_error("region", query_controls)
        out$error <- "You supplied an invalid value for country. Please use one of the valid values."

        # out <- list(
        #   error = "Invalid query arguments have been submitted.",
        #   details = list(msg = paste0("You cannot query individual countries when specifying a predefined sub-group. Please use  country=all")
        # )
        return(out)
      }
    }
    if (endpoint == "aux") {
      if(is.null(req$argsQuery$long_format)) req$argsQuery$long_format <- FALSE
        if (req$argsQuery$long_format && !req$argsQuery$table %in% c('cpi', 'ppp', 'gdp', 'pce', 'pop')) {
           res$status <- 404
           out <- list(error = "Only the tables cpi, ppp, gdp, pce and pop are available in long format")
           return(out)
        }
      }
  }
  plumber::forward()
}


# Set response headers ----------------------------------------------------

#* Set required response headers
#* @filter response_headers
function(res) {
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

  plumber::forward()

  # Logging
  # end_filters <- tictoc::toc(quiet = TRUE)
  # logger::log_info('filters: {round(end_filters$toc - end_filters$tic, digits = getOption("digits", 6))}')

}

# Register switch serializer
plumber::register_serializer("switch", pipapi:::serializer_switch)


# Endpoints definition ----------------------------------------------------

#* Check status of API
#* @get /api/v1/health-check
function() {
  "PIP API is running"
}

#* Return available data versions
#* @get /api/v1/versions
function(req) {
  out <- pipapi::version_dataframe(lkups$versions)
  out
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

#* Return cache info
#* @get /api/v1/cache-info
#* @serializer unboxedJSON
function() {
  info <- cd$info()
  info$missing <- NULL
  c(n_items = cd$size(), info)
}

#* Return cache log
#* @get /api/v1/cache-log
#* @serializer print list(quote = FALSE)
function(){
  readLines(cd$info()$logfile)
}

#* Reset current cache
#* @get /api/v1/cache-reset
#* @serializer unboxedJSON
function() {
  pipapi:::clear_cache(cd)
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

#* Return PIP information
#* @get /api/v1/pip-info
function(req) {
  pipapi::get_pip_version(lkup = lkups)
}

#* Return valid parameters
#* @get /api/v1/valid-params
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param format:[chr] Response format. Options are "json", "csv", or "rds".
#* @param endpoint:[chr] Endpoint for which to return the valid parameters
#* @serializer switch
function(req) {
  version <- req$argsQuery$version
  endpoint <- req$argsQuery$endpoint
  out <- pipapi::get_param_values(
    lkup = lkups,
    version = version,
    endpoint = endpoint)
  attr(out, "serialize_format") <- req$argsQuery$format
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
#* @param format:[chr] Response format. Options are "json", "csv", or "rds".
#* @serializer switch
function(req) {
  # Process request
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[params$version]]
  params$format <- NULL
  params$version <- NULL

  # Parallel processing for slow requests
  if (params$country == "all" && params$year == "all") {
    out <- promises::future_promise({
      tmp <- do.call(pipapi::pip, params)
      attr(tmp, "serialize_format") <- req$argsQuery$format
      tmp
    }, seed = TRUE)
  } else {
    out <- do.call(pipapi::pip, params)
    attr(out, "serialize_format") <- req$argsQuery$format
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
#* @param format:[chr] Response format. Options are "json", "csv", or "rds".
#* for all available versions
#* @serializer switch
function(req, res) {
  # Process request
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[params$version]]
  params$format <- NULL
  params$version <- NULL

  # Parallel processing for slow requests
  if (params$country %in% c("ALL", "WLD") && params$year == "ALL") {
    out <- promises::future_promise({
      tmp <- do.call(pipapi::pip_grp_logic, params)
      attr(tmp, "serialize_format") <- req$argsQuery$format
      tmp
    }, seed = TRUE)
  } else {
    out <- do.call(pipapi::pip_grp_logic, params)
    attr(out, "serialize_format") <- req$argsQuery$format
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
#* @param format:[chr] Response format. Options are "json", "csv", or "rds".
#* @serializer switch
function(req) {
  params <- req$argsQuery
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
  attr(out, "serialize_format") <- req$argsQuery$format
  out
}
# UI endpoints: Homepage --------------------------------------------------

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
    do.call(pipapi:::ui_hp_stacked, params)
  }, seed = TRUE)
}

#* Return data for home page country charts
#* @get /api/v1/hp-countries
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  do.call(pipapi:::ui_hp_countries, params)
}


# UI Endpoints: Poverty calculator ----------------------------------------

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
  if (params$country == "all" && params$year == "all") {
    promises::future_promise({
      do.call(pipapi::ui_pc_charts, params)
    }, seed = TRUE)
  } else {
    do.call(pipapi::ui_pc_charts, params)
  }
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
  if (params$country == "all" && params$year == "all") {
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

# UI Endpoints: Country Profiles ----------------------------------------

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
  if (params$country == "all") {
    promises::future_promise({
      do.call(pipapi::ui_cp_charts, params)
    }, seed = TRUE)
  } else {
    do.call(pipapi::ui_cp_charts, params)
  }
}

#* Return Country Profile - Downloads
#* @get /api/v1/cp-download
#* @param country:[chr] Country ISO3 code
#* @param povline:[dbl] Poverty Line
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer switch
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  params$format  <- NULL

  if (params$country == "all") {
    out <- promises::future_promise({
      tmp <- do.call(pipapi::ui_cp_download, params)
      attr(tmp, "serialize_format") <- req$argsQuery$format
      tmp
    }, seed = TRUE)
  } else {
    out <- do.call(pipapi::ui_cp_download, params)
    attr(out, "serialize_format") <- req$argsQuery$format
  }
  out
}

# UI Endpoints: Survey metadata  ------------------------------------------

#* Return data for the Data Sources page
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
  do.call(pipapi::ui_svy_meta, params)
}

#* Return valid years
#* @get /api/v1/valid-years
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer switch
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[params$version]]
  pipapi::valid_years(data_dir = params$lkup$data_root)
}


#* Return citation
#* @get /api/v1/citation
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param release_version:[chr] date when the data was published in YYYYMMDD format
#* @param ppp_version:[chr] ppp year to be used
#* @serializer switch
function(req) {
  params <- req$argsQuery
  pipapi::citation_from_version(params$version)
}