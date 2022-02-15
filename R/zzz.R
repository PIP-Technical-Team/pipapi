.onLoad <- function(libname, pkgname) {
  d <- rappdirs::user_cache_dir("pipapi")
  log <- sprintf("%s/cache.log", d)
  cd <- cachem::cache_disk(d, evict = "lru", logfile = log, max_size = 5 * 1024 * 1024^2) # 5GB
  pip <<- memoise::memoise(pip, cache = cd)
  assign("cd", cd, envir = .GlobalEnv)
}
