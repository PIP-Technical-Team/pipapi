#' Return the versions of the pip packages used for computations
#'
#' @param pip_packages character: Custom packages powering the API
#' @param data_folder_root character: Root path of data folder
#' @param valid_params list: Valid API query parameters
#'
#' @return list
#' @export
#'
get_pip_version <- function(pip_packages = c("pipapi", "wbpip"),
                            data_folder_root,
                            valid_params) {

  # Package versions
  pkg <- lapply(pip_packages, retrieve_pkg_version)
  names(pkg) <- pip_packages

  # Data version
  data <- fs::dir_ls(
    path = data_folder_root,
    type = "directory",
    recurse = TRUE
  )

  return(
    list(
      valid_query_parameters = valid_params,
      packages_version = pkg,
      data_versions = data
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
