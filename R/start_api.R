#' Main function to launch the API
#'
#' @param api_version character: API version to launch
#' @param port integer: Port
#' @param host character: Host
#'
#' @return plumber API
#' @export
#'
start_api <- function(api_version = "v1",
                      port = 80,
                      host = "0.0.0.0") {
  version_path <- sprintf(
    "plumber/%s/plumber.R",
    api_version
  )
  api_path <- system.file(version_path, package = "pipapi")
  api <- source(api_path)
  plumber::pr_run(api$value, host = host, port = port)

}
