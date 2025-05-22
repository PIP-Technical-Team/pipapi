library(plumber)
library(logger)

endpoints_path <- system.file("plumber/v1/endpoints.R", package = "pipapi")
api_spec_path <-  system.file("plumber/v1/openapi.yaml", package = "pipapi")
# convert_empty <- pipapi:::convert_empty

log_appender(appender_console())
log_layout(layout_glue_colors)
log_threshold(INFO)

plumber::pr(endpoints_path) |>
  plumber::pr_set_error(function(req, res, err) {
    log_error("Error at {req$REQUEST_METHOD} {req$PATH_INFO}: {err$message}")
    res$status <- 500
    list(error = "Internal server error")
  }) |>

  # Optional: global hooks (these are more general than pr_hook stages)
  plumber::pr_register_hooks(list(
    preroute = function(req) {
      log_info("Incoming request: {req$REQUEST_METHOD} {req$PATH_INFO}")
    }
  )) |>
  # pre-route log
  plumber::pr_hook("preroute", function() {
    # log_separator()
    # tictoc::tic("route") # Start timer for log info
  }) |>
  # post-route log
  plumber::pr_hook("postroute", function(req, res) {
    # end_route <- tictoc::toc(quiet = TRUE)
    # log_info('route: {convert_empty(req$REMOTE_ADDR)} {convert_empty(req$REQUEST_METHOD)} {convert_empty(req$PATH_INFO)} {convert_empty(req$QUERY_STRING)}  {convert_empty(res$status)} {round(end_route$toc - end_route$tic, digits = getOption("digits", 6))}')
  }) |>
  # pre-serialization log
  plumber::pr_hook("preserialize", function() {
    # tictoc::tic("serialize")
  }) |>
  # post-serialization log
  plumber::pr_hook("postserialize", function(req) {
    # end_serial <- tictoc::toc(quiet = TRUE)
    # log_info('serialize: {convert_empty(req$PATH_INFO)} {round(end_serial$toc - end_serial$tic, digits = getOption("digits", 6))}')
    # log_separator()
  }) |>
  plumber::pr_hook("exit", function() {
    # log_info('Bye bye: {proc.time()[["elapsed"]]}')
  }) |>
  # Set API spec
  plumber::pr_set_api_spec(api = function(spec) {
    spec$info$version <- utils::packageVersion("pipapi") |>
      as.character()
    spec
  }) |>
  plumber::pr_set_api_spec(
    yaml::read_yaml(api_spec_path))
