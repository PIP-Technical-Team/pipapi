library(plumber)
library(logger)
library(glue)

endpoints_path <- system.file("plumber/v1/endpoints.R", package = "pipapi")
api_spec_path <-  system.file("plumber/v1/openapi.yaml", package = "pipapi")
convert_empty <- pipapi:::convert_empty

# Enable / Disable logging
if (Sys.getenv("PIPAPI_LOGGING") == "TRUE") {
  log_dir <- rappdirs::user_log_dir("pipapi")
  if (!fs::dir_exists(log_dir)) {fs::dir_create(log_dir)}
  if (!fs::file_exists(paste0(log_dir, "/pipapi_logs"))) {
    fs::file_create(paste0(log_dir, "/pipapi_logs"))
  }
  log_file <- paste0(log_dir, "/pipapi_logs")
  logger::log_appender(logger::appender_file(log_file))
}

plumber::pr(endpoints_path) |>
  # pre-route log
  plumber::pr_hook("preroute", function(req) {
    if (req$PATH_INFO != "/api/v1/logs") {
    logger::log_info("{convert_empty(req$PATH_INFO)} {convert_empty(req$QUERY_STRING)}")
    }
  }) |>
  # post-serialization log
  plumber::pr_hook("postserialize", function(res) {
    if (res$status != 200) {
      tmp <- substr(res$body, start = 1, stop = 30)
      logger::log_info("{convert_empty(res$status)} {convert_empty(tmp)}...")
    } else {
      logger::log_info("{convert_empty(res$status)}")
    }
  }) |>
  # Set API spec
  plumber::pr_set_api_spec(api = function(spec) {
    spec$info$version <- utils::packageVersion("pipapi") |> as.character()
    spec
  }) |>
  plumber::pr_set_api_spec(
    yaml::read_yaml(api_spec_path))
