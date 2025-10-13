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

    # --- preserve raw versions
    # assign("pip_raw",                  pip, envir                  = parent.env(environment()))
    # assign("pip_agg_raw",              pip_agg, envir              = parent.env(environment()))
    # assign("ui_cp_charts_raw",         ui_cp_charts, envir         = parent.env(environment()))
    # assign("ui_cp_download_raw",       ui_cp_download, envir       = parent.env(environment()))
    # assign("ui_cp_key_indicators_raw", ui_cp_key_indicators, envir = parent.env(environment()))

    # then memoise the memoised versions for external use
    # pip                  <<- memo_norm(pip_raw, cache = cd)
    # pip_agg              <<- memo_norm(pip_agg_raw, cache = cd)
    # ui_cp_charts         <<- memo_norm(ui_cp_charts_raw, cache = cd)
    # ui_cp_download       <<- memo_norm(ui_cp_download_raw, cache = cd)
    # ui_cp_key_indicators <<- memo_norm(ui_cp_key_indicators_raw, cache = cd)



    pip            <<- memoise::memoise(pip, cache = cd, omit_args = "lkup")
    pip_agg        <<- memoise::memoise(pip_agg, cache = cd, omit_args = "lkup")
    ui_cp_charts   <<- memoise::memoise(ui_cp_charts, cache = cd, omit_args = "lkup")
    ui_cp_download <<- memoise::memoise(ui_cp_download, cache = cd, omit_args = "lkup")
    ui_cp_key_indicators <<- memoise::memoise(ui_cp_key_indicators, cache = cd, omit_args = "lkup")
    # ui_hp_stacked  <<- memoise::memoise(ui_hp_stacked, cache = cd, omit_args = "lkup")
    # pip_grp_new    <<- memoise::memoise(pip_grp_new, cache = cd, omit_args = "lkup")
    # pip_grp_logic  <<- memoise::memoise(pip_grp_logic, cache = cd, omit_args = "lkup")
    # pip_grp        <<- memoise::memoise(pip_grp, cache = cd, omit_args = "lkup")



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
  pl <- c(
    seq(from = 0.01, to = 4, by = 0.01),
    seq(from = 4.05, to = 20, by = 0.05),
    seq(from = 21, to = 100, by = 1),
    seq(from = 110, to = 900, by = 10)) |>
    round(2) |>
    unique()
  set_in_pipapienv("pl_to_store", pl)

  invisible()

}

