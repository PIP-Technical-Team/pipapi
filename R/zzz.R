.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("PIPAPI_APPLY_CACHING") == "TRUE") {
    d <- rappdirs::user_cache_dir("pipapi")
    log <- sprintf("%s/cache.log", d)
    cd <- cachem::cache_disk(d, evict = "lru", logfile = log, max_size = as.numeric(Sys.getenv("PIPAPI_CACHE_MAX_SIZE")))
    pip <<- memoise::memoise(pip, cache = cd)
    pip_grp <<- memoise::memoise(pip_grp, cache = cd)
    assign("cd", cd, envir = .GlobalEnv)
    message("Info: Disk based caching is enabled.")
  }
}
