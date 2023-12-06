#' Return the versions of the pip packages used for computations
#'
#' @param pip_packages character: Custom packages powering the API
#' @param data_versions character: Available data_versions
#' @return list
#' @export
get_pip_version <- function(pip_packages = c("pipapi",
                                             "wbpip"),
                            data_versions) {

  # PIP package versions
  core_packages <- lapply(pip_packages, function(x){
    pkg_version <- retrieve_pkg_version(x)
    pkg_hash    <- packageDescription(x)$GithubSHA1

    return(
      list(
        pkg_version = pkg_version,
        pkg_hash    = pkg_hash
      )
    )
  })

  names(core_packages) <- pip_packages


  # System info
  pip_system <- sessionInfo()

  # Other packages
  loaded_packages          <- pip_system$loadedOnly
  loaded_packages          <- loaded_packages[!names(loaded_packages) %in% pip_packages]
  loaded_packages_names    <- names(loaded_packages)
  loaded_packages_versions <- unlist(lapply(loaded_packages, function(x) {return(x[["Version"]])}))
  loaded_packages          <- paste(loaded_packages_names, loaded_packages_versions, sep = "_")
  loaded_packages          <- sort(loaded_packages)


  return(
    list(
      available_data_versions = data_versions,
      pip_packages            = core_packages,
      other_packages          = loaded_packages,
      r_version               = pip_system$R.version$version.string,
      server_os               = pip_system$running,
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
