#* @apiTitle PIP API
#* @apiVersion 0.0.1
#* @apiDescription This API powers computations of statistics available at
#* pip.worldbank.org

library(pipapi)


# API filters -------------------------------------------------------------

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
function(req, res, query_controls = lkups$query_controls) {
  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    are_valid <- pipapi:::check_parameters(req, query_controls)
    if (any(are_valid == FALSE)) {
      return("Invalid query parameters have been submitted")
    }
  }
  plumber::forward()
}


# Endpoints definition ----------------------------------------------------

#* Check status of API
#* @get /api/v1/health-check
function() {
  "PIP API is running"
}

#* @get /api/v1/system-info
function(){
  Sys.info()
}

#* Return PIP information
#* @get /api/v1/get-root
function() {
  getwd()
}

#* Return PIP information
#* @get /api/v1/get-available-objects
function() {
  list(
    search_list = search(),
    global    = ls(pos = 1),
    parent    = ls(pos = 2),
    gd_parent = ls(pos = 3)
  )
}

#* Return PIP information
#* @get /api/v1/info
function() {
  pipapi::get_pip_version(data_folder_root = lkups$data_root,
                          valid_params     = lkups$query_controls)
}

#* Return CPI table
#* @get /api/v1/cpi
#* @serializer json
function() {
  pipapi::get_aux_table(data_dir = lkups$data_root,
                        table = "cpi")
}

#* Return poverty lines for home page display
#* @get /api/v1/poverty-lines
#* @serializer json
function() {
  pipapi::get_aux_table(data_dir = lkups$data_root,
                        table = "poverty_lines")
}

#* Return indicators master table
#* @get /api/v1/indicators
#* @serializer json
function() {
  pipapi::get_aux_table(data_dir = lkups$data_root,
                        table = "indicators_master")
}

#* Return list of countries
#* @get /api/v1/countries
#* @serializer json
function() {
  pipapi::get_aux_table(data_dir = lkups$data_root,
                        table = "countries")
}

# #* Return custom plot
# #* @get /api/v1/get-plot
# #* @serializer htmlwidget
# function() {
#   pipapi:::custom_plot(lkups)
# }

#* @get /api/v1/pip
#* @param country:[chr] country iso3 code
#* @param year:[chr] year
#* @param povline:[dbl] Poverty Line
#* @param popshare:[dbl] numeric Share of the population living below the poverty Line
#* @param fill_gaps:[bool] Fill gaps for years with no available surveys
#* @param aggregate:[bool] Whether to aggregate results or not
#* @param group_by:[chr] Triggers sub-groups aggregation
#* @param welfare_type:[chr] Welfare Type. Options are "income" or "consumption"
#* @param svy_coverage:[chr] Survey coverage. Options are "national", "urban", "rural".
#* @param ppp:[dbl] Custom Purchase Power Parity (PPP) value
#* @param
#* @serializer json
function(req) {
  # Process request
  # browser()
  params <- req$argsQuery
  params$lkup <- lkups

  do.call(pipapi::pip, params)
}

#* @get /api/v1/hp-stacked
#* @param povline:[dbl] Poverty Line
#* @param
#* @serializer json
function(req) {
  # Process request
  # browser()
  params <- req$argsQuery
  params$lkup <- lkups

  do.call(pipapi:::ui_hp_stacked, params)
}

#* Return data for home page country charts
#* @get /api/v1/hp-countries
#* @serializer json
function(req) {
  params <- req$argsQuery
  params$lkup <- lkups

  do.call(pipapi:::ui_hp_countries, params)
}


# # Update UI
# #* @plumber
# function(pr) {
#   pr %>%
#     plumber::pr_set_api_spec(yaml::read_yaml("openapi.yaml"))
# }
