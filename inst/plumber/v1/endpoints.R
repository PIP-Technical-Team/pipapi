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
        return("Invalid version has been submitted. Please check valid versions with /versions")
      }
    }
  } else {
    req$argsQuery$version <- "latest_release"
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

#* Protect against invalid country code and year
#* @filter check_parameters
function(req, res) {
  # validate version
  # browser()
  lkups <- lkups$versions_paths[[req$argsQuery$version]]
  query_controls = lkups$query_controls

  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    are_valid <- pipapi:::check_parameters(req, query_controls)
    if (any(are_valid == FALSE)) {
      return("Invalid query parameters have been submitted")
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

#* Check status of API
#* @get /api/v1/versions
function() {
  lkups$versions
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

#* Return PIP information
#* @get /api/v1/pip-info
function(req) {
  pipapi::get_pip_version(lkup = lkups)
}

#* Return valid parameters
#* @get /api/v1/valid-params
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param format:[chr] Response format. Options are of "json", "csv", or "rds".
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
#* @param version:[chr] Data version. Defaults to most recent version. See api/v1/versions
#* @param format:[chr] Response format. Options are of "json", "csv", or "rds".
#* for all available versions
#* @serializer switch
function(req) {
  # Process request
  # browser()
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[params$version]]
  params$format <- NULL
  params$version <- NULL
  out <- do.call(pipapi::pip, params)
  attr(out, "serialize_format") <- req$argsQuery$format
  out
}

#* Return auxiliary data table
#* @get /api/v1/aux
#* @param table:[chr] Auxiliary data table to be returned
#* @param version:[chr] Data version. Defaults to latest versions. See api/v1/versions (add filter for version validation and default selection)
#* @param format:[chr] Response format. Options are of "json", "csv", or "rds".
#* @serializer switch
function(req) {
  # browser()
  params <- req$argsQuery
  params$lkup <- lkups$versions_paths[[params$version]]
  params$format <- NULL
  params$version <- NULL

  if (is.null(req$args$table)) {
    out <- params$lkup$aux_tables
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
  do.call(pipapi:::ui_hp_stacked, params)
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
  do.call(pipapi::ui_pc_charts, params)
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
  do.call(pipapi::ui_pc_charts, params)
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
  do.call(pipapi::ui_pc_regional, params)
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
  do.call(pipapi::ui_cp_charts, params)
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

# # Update UI
# #* @plumber
# function(pr) {
#   pr %>%
#     plumber::pr_set_api_spec(yaml::read_yaml("openapi.yaml"))
# }
