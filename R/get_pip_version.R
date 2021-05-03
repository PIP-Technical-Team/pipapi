#' Return the versions of the pip packages used for computations
#'
#'
#' @return list
#' @export
#'
get_pip_version <- function() {

  pip_packages <- c("pipapi", "wbpip")
  resp <- lapply(pip_packages, retrieve_version)
  names(resp) <- pip_packages

  return(resp)
}

retrieve_version <- function(package) {
  desc <- read.dcf(system.file("DESCRIPTION", package = package))
  resp <- list(
      version = unname(desc[1,"Version"])#,
      # built = unname(desc[1,"Built"])
    )
  return(resp)
}
