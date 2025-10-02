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

    # Small wrapper around memoised functions:
    memo_norm <- \(f, cache) {
      memoise::memoise(\(...) {
        args <- normalize_args(list(...))
        key  <- digest::digest(args, algo = "xxhash64")

        if (cache$exists(key)) {
          cli::cli_alert_info("CACHE HIT [{key}] for {substitute(f)}")
        } else {
          cli::cli_alert_warning("CACHE MISS [{key}] for {substitute(f)}")
        }

        do.call(f, args)
      },
      cache = cache,
      omit_args = "lkup"   # <- important, we don’t want to memoise on version lookup table
      )
    }

    # Memoise your core functions with normalization
    pip                  <<- memo_norm(pip, cache = cd)
    ui_hp_stacked        <<- memo_norm(ui_hp_stacked, cache = cd)
    pip_agg              <<- memo_norm(pip_agg, cache = cd)
    pip_grp_new          <<- memo_norm(pip_grp_new, cache = cd)
    pip_grp_logic        <<- memo_norm(pip_grp_logic, cache = cd)
    pip_grp              <<- memo_norm(pip_grp, cache = cd)
    ui_cp_charts         <<- memo_norm(ui_cp_charts, cache = cd)
    ui_cp_download       <<- memo_norm(ui_cp_download, cache = cd)
    ui_cp_key_indicators <<- memo_norm(ui_cp_key_indicators, cache = cd)

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

