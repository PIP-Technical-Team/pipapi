.onLoad <- function(libname, pkgname) {
  if (Sys.getenv("PIPAPI_APPLY_CACHING") == "TRUE") {
    d <- rappdirs::user_cache_dir("pipapi")
    # log <- sprintf("%s/cache.log", d)
    cd <- cachem::cache_disk(d,
                             evict = "lru",
                             logfile = NULL,
                             max_size = as.numeric(Sys.getenv("PIPAPI_CACHE_MAX_SIZE")),
                             prune_rate = 50)
    pip <<- memoise::memoise(pip, cache = cd, omit_args = "lkup")
    pip_grp <<- memoise::memoise(pip_grp, cache = cd, omit_args = "lkup")
    pip_grp_logic <<- memoise::memoise(pip_grp_logic, cache = cd, omit_args = "lkup")
    ui_cp_charts <<- memoise::memoise(ui_cp_charts, cache = cd, omit_args = "lkup")
    ui_cp_download <<- memoise::memoise(ui_cp_download, cache = cd, omit_args = "lkup")
    ui_cp_key_indicators <<- memoise::memoise(ui_cp_key_indicators, cache = cd, omit_args = "lkup")
    get_aux_table <<- memoise::memoise(get_aux_table, cache = cd)

    assign("cd", cd, envir = .GlobalEnv)
    packageStartupMessage("Info: Disk based caching is enabled.")
  }
}


