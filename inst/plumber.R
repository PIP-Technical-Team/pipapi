#* @apiTitle PIP API
#* @apiVersion 0.0.1
#* @apiDescription This API powers computations of statistics available at
#* pip.worldbank.org

library(pipapi)

source("../TEMP/TEMP_clean_data.R")

#* Check status of API
#* @get /health-check
function() {
  "PIP API is running"
}

#* Parse query parameters of incoming request
#* @filter parse_parameters
function(req, res) {
  if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
    req$args$country       <- pipapi:::parse_parameters_chr(req$args$country)
    req$args$year          <- pipapi:::parse_parameters_chr(req$args$year)
    req$args$povline       <- pipapi:::parse_parameters_dbl(req$args$povline)
    req$args$popshare      <- pipapi:::parse_parameters_dbl(req$args$popshare)
    req$args$fill_gaps     <- pipapi:::parse_parameters_lgl(req$args$fill_gaps)
    req$args$aggregate     <- pipapi:::parse_parameters_lgl(req$args$aggregate)
    req$args$group_by      <- pipapi:::parse_parameters_chr(req$args$group_by)
    req$args$welfare_type  <- pipapi:::parse_parameters_chr(req$args$welfare_type)
    req$args$svy_coverage  <- pipapi:::parse_parameters_chr(req$args$svy_coverage)
    req$args$ppp           <- pipapi:::parse_parameters_dbl(req$args$ppp)
  }

  plumber::forward()
}

# #* Protect against invalid country code and year
# #* @filter check_parameters
# function(req, res, valid_countries, valid_years) {
#   if (req$QUERY_STRING != "" & !grepl("swagger", req$PATH_INFO)) {
#     if (
#       pipapi::check_parameters(req, res, param = "country",
#                                    valid_values = pipapi:::valid_countries)
#     ) {
#       return(
#         pipapi::format_error(param = "country",
#                                  valid_values = pipapi:::valid_countries)
#       )
#     } else if (
#       pipapi::check_parameters(req, res, param = "year",
#                                    valid_values = pipapi:::valid_years)
#     ) {
#       return(
#         pipapi::format_error(param = "year",
#                                  valid_values = pipapi:::valid_years)
#       )
#     } else {
#       plumber::forward()
#     }
#   } else {
#     plumber::forward()
#   }
# }



#* @get /pip
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
  #browser()
  pipapi::pip(country      = req$args$country,
              year         = req$args$year,
              povline      = req$args$povline,
              popshare     = req$args$popshare,
              fill_gaps    = req$args$fill_gaps,
              aggregate    = req$args$aggregate,
              group_by     = req$args$group_by,
              welfare_type = req$args$welfare_type,
              svy_coverage = req$args$svy_coverage,
              ppp          = req$args$ppp,
              lkup         = lkups,
              paths        = paths)
}
