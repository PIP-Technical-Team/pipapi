#' main
#'
#' Main function to launch the API
#' @return
#' @export
#'
main <- function() {
  api_path <- system.file("plumber/v1/plumber.R", package = "pipapi")
  api <- plumber::plumb(api_path)
  api$run(port = 80, host = "0.0.0.0")
}
