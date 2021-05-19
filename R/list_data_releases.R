#' Return the list of PIP data releases
#'
#' @param root character: Path to root data folder
#'
#' @return character
#' @export
#'

list_data_releases <- function(root) {
  out <- dir(root)

  return(out)
}
