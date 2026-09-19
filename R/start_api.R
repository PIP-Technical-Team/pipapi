#' Main function to launch the API
#'
#' @param api_version character: API version to launch
#' @param port integer: Port
#' @param host character: Host
#' @param lkups Optional versioned lookup list.
#'
#' @return plumber API
#' @export
#'
start_api <- function(api_version = "v1",
                      port = 80,
                      host = "0.0.0.0",
                      lkups = NULL) {
  if (!is.null(lkups)) options(pipapi.lkups = lkups)
  if (cache_v2_enabled() && is.null(getOption("pipapi.cache_v2_config"))) {
    root <- Sys.getenv("PIPAPI_CACHE_V2_ROOT", unset = "")
    data_root <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL", unset = "")
    if (!nzchar(root) || !nzchar(data_root)) {
      stop("Cache-v2 API startup requires PIPAPI_CACHE_V2_ROOT and PIPAPI_DATA_ROOT_FOLDER_LOCAL.")
    }
    cache_v2_configure_from_disk(root, data_root, intermediate_mode = "read_only")
  }
  version_path <- sprintf(
    "plumber/%s/plumber.R",
    api_version
  )
  api_path <- system.file(version_path, package = "pipapi")
  api_env <- new.env(parent = environment())
  if (!is.null(lkups)) api_env$lkups <- lkups
  api <- sys.source(api_path, envir = api_env)
  plumber::pr_run(api$value, host = host, port = port)

}
