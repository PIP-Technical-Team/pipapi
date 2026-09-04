pipapi_default_options <- list(
  pipapi.query_live_data = FALSE,
  pipapi.verbose = FALSE
)


.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("PIPAPI_APPLY_CACHING") == "TRUE") {
    d <- rappdirs::user_cache_dir("pipapi")
    cache_default_max_size <- 1024^3
    cache_max_size <- suppressWarnings(
      as.numeric(Sys.getenv("PIPAPI_CACHE_MAX_SIZE"))
    )

    if (is.na(cache_max_size) || cache_max_size <= 0) {
      if (nzchar(Sys.getenv("PIPAPI_CACHE_MAX_SIZE"))) {
        packageStartupMessage(
          sprintf(
            "Warning: Invalid PIPAPI_CACHE_MAX_SIZE=%s. Using default %s bytes.",
            dQuote(Sys.getenv("PIPAPI_CACHE_MAX_SIZE")),
            format(cache_default_max_size, scientific = FALSE)
          )
        )
      } else {
        packageStartupMessage(
          sprintf(
            "Info: PIPAPI_CACHE_MAX_SIZE is empty. Using default %s bytes.",
            format(cache_default_max_size, scientific = FALSE)
          )
        )
      }

      cache_max_size <- cache_default_max_size
    }

    if (dir.exists(d)) {
      cache_files <- suppressWarnings(
        list.files(d, pattern = "\\.qs$", full.names = TRUE, recursive = TRUE)
      )

      if (length(cache_files) > 0) {
        cache_file_size <- suppressWarnings(file.size(cache_files))
        bad_files <- cache_files[is.na(cache_file_size) | cache_file_size == 0]

        if (length(bad_files) > 0) {
          suppressWarnings(unlink(bad_files, recursive = TRUE, force = TRUE))
          packageStartupMessage(
            sprintf(
              "Info: Removed %s zero-size cache file(s) from %s.",
              length(bad_files),
              d
            )
          )
        }
      }
    }

    # log <- sprintf("%s/cache.log", d)
    cd <- tryCatch(
      cachem::cache_disk(
        d,
        read_fn = qs2::qs_read,
        write_fn = qs2::qs_save,
        extension = ".qs",
        evict = "lru",
        logfile = NULL,
        max_size = cache_max_size,
        prune_rate = 50
      ),
      error = function(err) {
        packageStartupMessage(
          sprintf(
            "Warning: Disk cache init failed (%s). Caching is disabled.",
            conditionMessage(err)
          )
        )
        NULL
      }
    )

    if (!is.null(cd)) {
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
