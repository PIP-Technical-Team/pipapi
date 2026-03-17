pipapi_default_options <- list(
  pipapi.query_live_data = FALSE,
  pipapi.verbose = FALSE
)


.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("PIPAPI_APPLY_CACHING") == "TRUE") {
    d <- rappdirs::user_cache_dir("pipapi")
    # log <- sprintf("%s/cache.log", d)
    cd <- cachem::cache_disk(
      d,
      read_fn = qs2::qs_read,
      write_fn = qs2::qs_save,
      extension = ".qs",
      evict = "lru",
      logfile = NULL,
      max_size = as.numeric(Sys.getenv("PIPAPI_CACHE_MAX_SIZE")),
      prune_rate = 50
    )

    pip <<- memoise::memoise(pip, cache = cd, omit_args = "lkup")
    pip_agg <<- memoise::memoise(pip_agg, cache = cd, omit_args = "lkup")
    ui_cp_charts <<- memoise::memoise(
      ui_cp_charts,
      cache = cd,
      omit_args = "lkup"
    )
    ui_cp_download <<- memoise::memoise(
      ui_cp_download,
      cache = cd,
      omit_args = "lkup"
    )
    ui_cp_key_indicators <<- memoise::memoise(
      ui_cp_key_indicators,
      cache = cd,
      omit_args = "lkup"
    )

    assign("cd", cd, envir = .GlobalEnv)
    packageStartupMessage("Info: Disk based caching is enabled.")
  }

  op <- options()
  toset <- !(names(pipapi_default_options) %in% names(op))
  if (any(toset)) {
    options(pipapi_default_options[toset])
  }

  # pov lines to store
  pl <- c(
    seq(from = 0.01, to = 4, by = 0.01),
    seq(from = 4.05, to = 20, by = 0.05),
    seq(from = 21, to = 100, by = 1),
    seq(from = 110, to = 900, by = 10)
  ) |>
    round(2) |>
    unique()
  set_in_pipapienv("pl_to_store", pl)

  invisible()
}
