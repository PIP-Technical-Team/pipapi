#* @apiTitle PIP API
#* @apiDescription This API powers computations of statistics available at
#* pip.worldbank.org

library(pipapi)

# API filters -------------------------------------------------------------

#* Ensure that version parameter is correct
#* @filter validate_version
function(req, res) {
  # tictoc::tic("filters")
  # browser()
  if (!is.null(req$argsQuery$version) & !grepl("swagger", req$PATH_INFO)) {
    if (!is.null(req$argsQuery$version)) {
      if (!req$argsQuery$version %in% lkups$versions) {
        res$status <- 404
        out <- list(
          error = "Invalid query arguments have been submitted.",
          details = list(msg = "You supplied an invalid value for version. Please use one of the valid values.",
                         valid = lkups$versions))
        return(out)
        #return("Invalid version has been submitted. Please check valid versions with /versions")
      }
    }
  } else {
    req$argsQuery$version <- lkups$latest_release
  }
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
  # browser()
  lkups <- lkups$versions_paths[[req$argsQuery$version]]
  query_controls = lkups$query_controls

  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    # STEP 1: Assign required parameters
    # Non-provided parameters are typically assigned the underlying function
    # arguments' default values. There is an exception to that however:
    # The `country` & `year` parameters cannot be NULL in order for to pass
    # the if condition that will decide whether or no the request should be
    # treated asynchronously.
    req <- pipapi:::assign_required_params(req)

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
      if (req$argsQuery$group_by != "none" && req$argsQuery$country != "all") {
        res$status <- 400
        out <- list(
          error = "Invalid query arguments have been submitted.",
          details = list(msg = "You cannot query individual countries when specifying a predefined sub-group. Please use country=all")
        )
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
#* @param format:[chr] Response format. Options are "json", "csv", or "rds".
#* @serializer switch
function(req) {
  # browser()
  out <- lkups$versions
  # if (!is.null(req$argsQuery$format)) {
  #   if (req$argsQuery$format == "csv") out <- data.frame(versions = out)
  #   attr(out, "serialize_format") <- req$argsQuery$format
  # }
  out <- data.frame(versions = out)
  attr(out, "serialize_format") <- req$argsQuery$format
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
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer unboxedJSON
function(req) {
  dir <- lkups$versions_paths[[req$argsQuery$version]]$data_root
  readLines(sprintf("%s/data_update_timestamp.txt", dir))
}

#* Check Github hash for the PIP packages
#* @get /api/v1/gh-hash
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
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param format:[chr] Response format. Options are "json", "csv", or "rds".
#* @serializer switch
function(req) {
  # browser()
  version <- req$argsQuery$version
  out <- pipapi::get_param_values(
    lkup = lkups,
    version = version)
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
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions for all available versions
#* @param format:[chr] Response format. Options are "json", "csv", or "rds".
#* @serializer switch
function(req) {
  # Process request
  # browser()
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[params$version]]
  params$format <- NULL
  params$version <- NULL

  # Define default arguments
  # if (is.null(params$country))
  #   params$country <- "all"
  # if (is.null(params$year))
  #   params$year <- "all"

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
#* @param popshare:[dbl] Share of the population living below the poverty Line.
#* @param group_by:[chr] Select type of aggregation (simple weighted average or a pre-defined subgroup)
#* @param welfare_type:[chr] Welfare Type. Options are "income" or "consumption"
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param format:[chr] Response format. Options are "json", "csv", or "rds".
#* for all available versions
#* @serializer switch
function(req, res) {
  # Process request
  # browser()
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[params$version]]
  params$format <- NULL
  params$version <- NULL

  # Parallel processing for slow requests
  if (params$country == "all" && params$year == "all") {
    out <- promises::future_promise({
      tmp <- do.call(pipapi::pip_grp, params)
      attr(tmp, "serialize_format") <- req$argsQuery$format
      tmp
    }, seed = TRUE)
  } else {
    out <- do.call(pipapi::pip_grp, params)
    attr(out, "serialize_format") <- req$argsQuery$format
  }
  out
}

#* Return auxiliary data table
#* @get /api/v1/aux
#* @param table:[chr] Auxiliary data table to be returned
#* @param version:[chr] Data version. Defaults to latest versions. See api/v1/versions (add filter for version validation and default selection)
#* @param format:[chr] Response format. Options are "json", "csv", or "rds".
#* @serializer switch
function(req) {
  # browser()
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[params$version]]
  params$format <- NULL
  params$version <- NULL

  if (is.null(req$args$table)) {
    out <- data.frame(tables = params$lkup$aux_tables)
  } else {
    out <- pipapi::get_aux_table(
      data_dir = params$lkup$data_root,
      table = req$args$table)
  }
  attr(out, "serialize_format") <- req$argsQuery$format
  out
}
# UI endpoints: Homepage --------------------------------------------------

#* Return poverty lines for home page display
#* @get /api/v1/poverty-lines
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
function(req) {
  pipapi::get_aux_table(data_dir = lkups$versions_paths[[req$argsQuery$version]]$data_root,
                        table = "poverty_lines")
}

#* Return indicators master table
#* @get /api/v1/indicators
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json list(na="null")
function(req) {
  pipapi::get_aux_table(data_dir = lkups$versions_paths[[req$argsQuery$version]]$data_root,
                        table = "indicators")
}

#* Return list of variables used for decomposition
#* @get /api/v1/decomposition-vars
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
function(req) {
  pipapi::get_aux_table(data_dir = lkups$versions_paths[[req$argsQuery$version]]$data_root,
                        table = "decomposition_master")
}

#* Return data for home page main chart
#* @get /api/v1/hp-stacked
#* @param povline:[dbl] Poverty Line
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
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json
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
#* @param povline:[dbl] Poverty Line
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

# UI Endpoints: Survey metadata  ------------------------------------------

#* Return data for the Data Sources page
#* @get /api/v1/survey-metadata
#* @param country:[chr] Country ISO3 code
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @serializer json list(na="null")
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[req$argsQuery$version]]
  params$version <- NULL
  do.call(pipapi::ui_svy_meta, params)
}
