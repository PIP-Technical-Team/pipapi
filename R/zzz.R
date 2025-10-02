pipapi_default_options <- list(
  pipapi.query_live_data = FALSE,
  pipapi.verbose = FALSE
)


.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("PIPAPI_APPLY_CACHING") == "TRUE") {
    d <- rappdirs::user_cache_dir("pipapi")
    # log <- sprintf("%s/cache.log", d)
    cd <- cachem::cache_disk(d,
                             read_fn = qs::qread,
                             write_fn = qs::qsave,
                             extension = ".qs",
                             evict = "lru",
                             logfile = NULL,
                             max_size = as.numeric(Sys.getenv("PIPAPI_CACHE_MAX_SIZE")),
                             prune_rate = 50)

    # keep originals under another name
    pip_raw                  <- pip
    pip_agg_raw              <- pip_agg
    pip_grp_raw              <- pip_grp
    pip_grp_logic_raw        <- pip_grp_logic
    pip_grp_new_raw          <- pip_grp_new
    ui_cp_charts_raw         <- ui_cp_charts
    ui_cp_download_raw       <- ui_cp_download
    ui_cp_key_indicators_raw <- ui_cp_key_indicators

    # then memoise the memoised versions for external use
    pip                  <<- memo_norm(pip_raw, cache = cd)
    pip_agg              <<- memo_norm(pip_agg_raw, cache = cd)
    pip_grp              <<- memo_norm(pip_grp_raw, cache = cd)
    pip_grp_logic        <<- memo_norm(pip_grp_logic_raw, cache = cd)
    pip_grp_new          <<- memo_norm(pip_grp_new_raw, cache = cd)
    ui_cp_charts         <<- memo_norm(ui_cp_charts_raw, cache = cd)
    ui_cp_download       <<- memo_norm(ui_cp_download_raw, cache = cd)
    ui_cp_key_indicators <<- memo_norm(ui_cp_key_indicators_raw, cache = cd)

    # pos = 1L
    # assign("cd", cd, envir = as.environment(pos))
    assign("cd", cd, envir = .GlobalEnv)
    packageStartupMessage("Info: Disk based caching is enabled.")
  }

  op <- options()
  toset <- !(names(pipapi_default_options) %in% names(op))
  if (any(toset)) options(pipapi_default_options[toset])


  # set multi threats
  # available_cores <- parallel::detectCores() - 1
  #
  # cores_to_use <- max(available_cores, 1) |>
  #   min(8)
  # set_in_pipapienv("cores_to_use", cores_to_use)


  # pov lines to store
  pl <- c(seq(from = 0.01, to = 5, by = 0.01),
          seq(from = 5.1, to = 20, by = 0.1),
          seq(from = 21, to = 100, by = 1),
          seq(from = 105, to = 900, by = 5))
  set_in_pipapienv("pl_to_store", pl)

  invisible()

}

