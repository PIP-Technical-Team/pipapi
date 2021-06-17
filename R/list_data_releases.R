#' Return the list of PIP data releases
#'
#' @param data_dir character: Path to root data folder
#'
#' @return character
#' @export
#'

list_data_releases <- function(data_dir) {
  out <- dir(data_dir)
  out <- sort(out, decreasing = TRUE)

  return(out)
}
