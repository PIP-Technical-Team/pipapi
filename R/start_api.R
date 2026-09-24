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
.load_api_router <- function(api_path, api_env) {
  router <- source(api_path, local = api_env)$value
  if (!inherits(router, "Plumber")) {
    stop("The API script did not create a Plumber router.")
  }
  router
}

.cache_v2_prepare_lkups <- function(lkups) {
  configured <- names(.cache_v2_state$config$manifest$versions)
  available <- names(lkups$versions_paths)
  advertised <- as.character(lkups$versions)
  if (is.null(available) || anyNA(available) || anyDuplicated(available) ||
      !identical(advertised, available)) {
    stop("API lookup versions and version paths must match exactly.")
  }
  missing <- setdiff(configured, available)
  if (length(missing)) {
    stop("API lookups are missing configured versions: ", paste(missing, collapse = ", "))
  }
  if (!lkups$latest_release %in% configured) {
    stop("The API latest release must be a configured cache-v2 version.")
  }
  lkups$versions_paths <- Map(
    cache_v2_attach_if_managed,
    lkups$versions_paths,
    names(lkups$versions_paths)
  )
  lkups
}

.cache_v2_verify_api_lkups <- function(lkups) {
  for (version in names(.cache_v2_state$config$manifest$versions)) {
    cache_v2_validate_intermediate(lkups$versions_paths[[version]], require_rows = TRUE)
  }
  invisible(lkups)
}

.resolve_api_lkups <- function(lkups = NULL) {
  if (!is.null(lkups)) return(lkups)
  configured <- getOption("pipapi.lkups")
  if (!is.null(configured)) return(configured)
  get0("lkups", envir = .GlobalEnv, inherits = FALSE)
}

start_api <- function(api_version = "v1",
                      port = 80,
                      host = "0.0.0.0",
                      lkups = NULL) {
  lkups <- .resolve_api_lkups(lkups)
  if (!is.null(lkups)) options(pipapi.lkups = lkups)
  if (cache_v2_enabled() && is.null(.cache_v2_state$config)) {
    root <- Sys.getenv("PIPAPI_CACHE_V2_ROOT", unset = "")
    data_root <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL", unset = "")
    if (!nzchar(root) || !nzchar(data_root)) {
      stop("Cache-v2 API startup requires PIPAPI_CACHE_V2_ROOT and PIPAPI_DATA_ROOT_FOLDER_LOCAL.")
    }
    cache_v2_configure_from_disk(root, data_root, intermediate_mode = "read_only")
  }
  if (!is.null(lkups) && cache_v2_enabled() && !is.null(.cache_v2_state$config)) {
    lkups <- .cache_v2_prepare_lkups(lkups)
    .cache_v2_verify_api_lkups(lkups)
    options(pipapi.lkups = lkups)
  }
  version_path <- sprintf(
    "plumber/%s/plumber.R",
    api_version
  )
  api_path <- system.file(version_path, package = "pipapi")
  api_env <- new.env(parent = environment())
  if (is.null(lkups)) stop("API startup requires versioned lookups.")
  api_env$lkups <- lkups
  api <- .load_api_router(api_path, api_env)
  plumber::pr_run(api, host = host, port = port)

}
