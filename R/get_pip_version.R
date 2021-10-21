#' Return the versions of the pip packages used for computations
#'
#' @param pip_packages character: Custom packages powering the API
#' @inheritParams pip
#' @return list
#' @export
get_pip_version <- function(pip_package = c("pipapi"),
                            lkup) {

  # Package versions
  pipapi_version <- retrieve_pkg_version(pip_package)
  pipapi_package <- paste(pip_package, pipapi_version, sep = "_")

  # System
  pip_system <- sessionInfo()
  loaded_packages <- pip_system$loadedOnly
  loaded_packages_names <- names(loaded_packages)
  loaded_packages_versions <- unlist(lapply(loaded_packages, function(x) {return(x[["Version"]])}))
  loaded_packages <- paste(loaded_packages_names, loaded_packages_versions, sep = "_")
  loaded_packages <- c(loaded_packages, pipapi_package)
  loaded_packages <- sort(loaded_packages)


  return(
    list(
      available_data_versions = lkup$versions,
      package_versions        = loaded_packages,
      r_version               = pip_system$R.version$version.string,
      server_os               = pip_system$platform,
      server_time             = Sys.time()
    )
  )
}

retrieve_pkg_version <- function(package) {
  desc <- read.dcf(system.file("DESCRIPTION", package = package))
  # pkg <- list(
  #   version = unname(desc[1, "Version"]) # ,
    # built = unname(desc[1,"Built"])
    pkg_version <- unname(desc[1, "Version"])
  # )

  return(pkg_version)
}
