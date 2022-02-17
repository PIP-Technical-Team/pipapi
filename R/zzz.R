.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("PIPAPI_APPLY_CACHING") == "TRUE") {
    d <- rappdirs::user_cache_dir("pipapi")
    log <- sprintf("%s/cache.log", d)
    cd <- cachem::cache_disk(d, evict = "lru", logfile = log, max_size = 5 * 1024 * 1024^2) # 5GB
    pip <<- memoise::memoise(pip, cache = cd)
    assign("cd", cd, envir = .GlobalEnv)
    message("Info: Disk based caching is enabled.")
  }
}
