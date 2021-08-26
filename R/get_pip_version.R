#' Return the versions of the pip packages used for computations
#'
#' @param pip_packages character: Custom packages powering the API
#' @inheritParams pip
#' @return list
#' @export
get_pip_version <- function(pip_packages = c("pipapi", "wbpip"),
                            lkup) {

  # Package versions
  pkg <- lapply(pip_packages, retrieve_pkg_version)
  names(pkg) <- pip_packages

  return(
    list(
      valid_query_parameters = lkup$query_controls,
      packages_version = pkg,
      data_versions = lkups$query_controls$version
    )
  )
}

retrieve_pkg_version <- function(package) {
  desc <- read.dcf(system.file("DESCRIPTION", package = package))
  pkg <- list(
    version = unname(desc[1, "Version"]) # ,
    # built = unname(desc[1,"Built"])
  )

  return(pkg)
}
