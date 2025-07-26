library(plumber)
endpoints_path <- system.file("plumber/v1/endpoints.R", package = "pipapi")
api_spec_path <-  system.file("plumber/v1/openapi.yaml", package = "pipapi")

plumber::pr(endpoints_path) |>
  # pre-route log
  plumber::pr_hook("preroute", function(req) {
  }) |>
  # post-route log
  plumber::pr_hook("postroute", function(req, res) {
  }) |>
  # pre-serialization log
  plumber::pr_hook("preserialize", function() {
  }) |>
  # post-serialization log
  plumber::pr_hook("postserialize", function(req) {
  }) |>
  plumber::pr_hook("exit", function() {
  }) |>
  plumber::pr_set_error(function(req, res, err) {
    # In case of error, make sure you log the endpoint for #432
    method <- req$REQUEST_METHOD
    path <- req$PATH_INFO
    cat(sprintf("ERROR at %s %s: %s\n", method, path, err$message))
  }) |>
  # Set API spec
  plumber::pr_set_api_spec(api = function(spec) {
    spec$info$version <- utils::packageVersion("pipapi") |>
      as.character()
    spec
  }) |>
  plumber::pr_set_api_spec(
    yaml::read_yaml(api_spec_path))
