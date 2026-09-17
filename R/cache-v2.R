# Cache v2 deliberately does not use memoise hashes or legacy lookup signatures.
.cache_v2_state <- new.env(parent = emptyenv())
.cache_v2_state$config <- NULL
.cache_v2_state$locks <- new.env(parent = emptyenv())
.cache_v2_state$originals <- new.env(parent = emptyenv())
.cache_v2_state$stats <- list()

.cache_v2_count <- function(event, operation, seconds = NULL) {
  name <- paste(event, operation, sep = ".")
  old <- .cache_v2_state$stats[[name]]
  if (is.null(old)) old <- list(count = 0L, seconds = 0)
  old$count <- old$count + 1L
  if (!is.null(seconds)) old$seconds <- old$seconds + seconds
  .cache_v2_state$stats[[name]] <- old
  invisible(NULL)
}

cache_v2_stats <- function(reset = FALSE) {
  out <- .cache_v2_state$stats
  if (isTRUE(reset)) .cache_v2_state$stats <- list()
  out
}

cache_v2_enabled <- function() {
  identical(Sys.getenv("PIPAPI_CACHE_V2"), "TRUE") &&
    identical(Sys.getenv("PIPAPI_APPLY_CACHING"), "TRUE")
}

.cache_v2_sort <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      if (any(!nzchar(names(x))) || anyDuplicated(names(x))) {
        stop("Cache v2 requires unique, nonempty field names.")
      }
      x <- x[order(enc2utf8(names(x)), method = "radix")]
    }
    return(lapply(x, .cache_v2_sort))
  }
  if (is.character(x)) x <- enc2utf8(x)
  x
}

.cache_v2_json <- function(x) {
  enc2utf8(as.character(jsonlite::toJSON(.cache_v2_sort(x),
    auto_unbox = TRUE, null = "null", na = "null", digits = NA,
    pretty = FALSE, force = TRUE
  )))
}

.cache_v2_sha <- function(x) {
  digest::digest(charToRaw(enc2utf8(x)), algo = "sha256", serialize = FALSE)
}

.cache_v2_file_sha <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

.cache_v2_component <- function(x) {
  if (length(x) != 1L || is.na(x) || !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", x) ||
      x %in% c(".", "..")) stop("Invalid cache v2 namespace component.")
  x
}

.cache_v2_version_root <- function(data_dir, version) {
  .cache_v2_component(version)
  if (dir.exists(file.path(data_dir, version))) return(file.path(data_dir, version))
  if (dir.exists(file.path(data_dir, "_aux"))) return(data_dir)
  stop("Source directory not found for version: ", version)
}

.cache_v2_inputs <- function(root) {
  dirs <- c("_aux", "estimations", "survey_data", "lineup_data")
  if (!all(dir.exists(file.path(root, dirs)))) {
    stop("Incomplete cache v2 source tree: ", root)
  }
  ids <- unlist(lapply(dirs, function(d) {
    file.path(d, list.files(file.path(root, d), recursive = TRUE,
      all.files = TRUE, no.. = TRUE))
  }), use.names = FALSE)
  ids <- gsub("\\\\", "/", ids)
  ids <- ids[!grepl("(^|/)(caches?|logs?|tmp|temp)(/|$)|(^|/)cache\\.duckdb(\\.wal)?$|\\.(tmp|lock|log)$", ids,
    ignore.case = TRUE)]
  revision <- "data_update_timestamp.txt"
  if (file.exists(file.path(root, revision))) ids <- c(ids, revision)
  sort(unique(ids), method = "radix")
}

cache_v2_manifest <- function(data_dir, versions, previous = NULL) {
  if (!length(versions) || anyDuplicated(versions)) stop("Supply unique full versions.")
  data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  if (length(versions) > 1L && dir.exists(file.path(data_dir, "_aux"))) {
    stop("Multiple versions require a parent source directory.")
  }
  entries <- lapply(versions, function(version) {
    root <- .cache_v2_version_root(data_dir, version)
    ids <- .cache_v2_inputs(root)
    revision_path <- file.path(root, "data_update_timestamp.txt")
    release <- if (file.exists(revision_path)) {
      paste(readLines(revision_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    } else NULL
    old <- previous$versions[[version]]
    files <- lapply(ids, function(id) {
      path <- file.path(root, id)
      before <- file.info(path)
      sha <- .cache_v2_file_sha(path)
      after <- file.info(path)
      if (!identical(before$size, after$size) || !identical(before$mtime, after$mtime)) {
        stop("Source changed while hashing: ", id)
      }
      timestamp <- release
      if (is.null(timestamp) && identical(old$files[[id]]$sha256, sha)) {
        timestamp <- old$files[[id]]$source_timestamp
      }
      list(id = id, sha256 = sha, size = unname(after$size),
        source_timestamp = timestamp, scan_mtime = as.numeric(after$mtime))
    })
    names(files) <- ids
    identity <- lapply(files, function(f) { f$scan_mtime <- NULL; f })
    list(fingerprint = .cache_v2_sha(.cache_v2_json(list(version = version,
      files = identity, release_revision = release))), files = files,
      release_revision = release)
  })
  names(entries) <- versions
  list(data_root = data_dir, versions = entries,
    fingerprint = .cache_v2_sha(.cache_v2_json(lapply(entries, `[[`, "fingerprint"))))
}

cache_v2_assert_inputs <- function(manifest, data_dir = manifest$data_root, full = TRUE) {
  fail <- function(message, versions = names(manifest$versions)) {
    cfg <- .cache_v2_state$config
    if (!is.null(cfg) && identical(cfg$manifest$fingerprint, manifest$fingerprint)) {
      cache_v2_taint(message, versions)
    }
    stop(message)
  }
  if (isTRUE(full)) {
    current <- cache_v2_manifest(data_dir, names(manifest$versions), previous = manifest)
    if (!identical(current$fingerprint, manifest$fingerprint)) {
      changed <- names(manifest$versions)[!vapply(names(manifest$versions), function(v) {
        identical(current$versions[[v]]$fingerprint, manifest$versions[[v]]$fingerprint)
      }, logical(1))]
      fail("Cache v2 source inputs changed; restart the build.", changed)
    }
  } else {
    for (version in names(manifest$versions)) {
      root <- .cache_v2_version_root(data_dir, version)
      files <- manifest$versions[[version]]$files
      if (!identical(.cache_v2_inputs(root), names(files))) fail("Source input set changed.", version)
      stat <- file.info(file.path(root, names(files)))
      if (!identical(unname(stat$size), unname(vapply(files, `[[`, numeric(1), "size")))) {
        fail("Source input stats changed; run a full content check.", version)
      }
      changed <- which(as.numeric(stat$mtime) != vapply(files, `[[`, numeric(1), "scan_mtime"))
      for (i in changed) {
        if (!identical(.cache_v2_file_sha(file.path(root, names(files)[i])), files[[i]]$sha256)) {
          fail("Source input content changed; restart the build.", version)
        }
        # Mtime is a scan hint only. A verified copy-time change is harmless.
        cfg <- .cache_v2_state$config
        if (!is.null(cfg) && identical(cfg$manifest$fingerprint, manifest$fingerprint) &&
            identical(cfg$manifest$data_root, data_dir)) {
          .cache_v2_state$config$manifest$versions[[version]]$files[[i]]$scan_mtime <- as.numeric(stat$mtime[i])
        }
      }
    }
  }
  invisible(TRUE)
}

.cache_v2_constant <- function(x) {
  # Reference-mutable tables are covered by packaged data assets, not live state.
  if (inherits(x, "data.table")) stop("Not a static constant.")
  if (!(is.atomic(x) || is.list(x) || is.null(x))) stop("Not a static constant.")
  attrs <- attributes(x)
  attributes(x) <- NULL
  if (is.list(x)) x <- lapply(x, .cache_v2_constant)
  if (is.character(x)) x <- enc2utf8(x)
  if (length(attrs)) attributes(x) <- lapply(attrs, .cache_v2_constant)
  x
}

.cache_v2_code <- function(ns) {
  names <- sort(ls(ns, all.names = TRUE), method = "radix")
  names <- names[!startsWith(names, ".__")]
  out <- lapply(names, function(name) {
    x <- get(name, envir = ns, inherits = FALSE)
    if (!is.function(x)) {
      return(tryCatch(list(name = name, constant_digest = digest::digest(
        .cache_v2_constant(x), algo = "sha256", serializeVersion = 3L
      )), error = function(e) NULL))
    }
    original <- attr(x, "cache_v2_original", exact = TRUE)
    if (!is.null(original)) x <- original
    list(name = name,
      formals = paste(deparse(formals(x), width.cutoff = 500L), collapse = "\n"),
      body = paste(deparse(body(x), width.cutoff = 500L), collapse = "\n"))
  })
  .cache_v2_sha(.cache_v2_json(Filter(Negate(is.null), out)))
}

cache_v2_build <- function() {
  # Loaded formals/bodies exclude bytecode addresses, environments and wrapper state.
  # This detects edited development code even when DESCRIPTION/RemoteSha are stale.
  deps <- tools::package_dependencies("pipapi", db = utils::installed.packages(), recursive = TRUE)[["pipapi"]]
  packages <- sort(unique(c("pipapi", "wbpip", "digest", "filelock", "ps", deps)),
    method = "radix")
  entries <- lapply(packages, function(package) {
    desc <- utils::packageDescription(package)
    if (is.na(desc[1L])) stop("Cannot establish build for package: ", package)
    root <- system.file(package = package)
    ids <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    # Installed runtime assets and native code, not help indexes or install paths.
    ids <- ids[!grepl("^(Meta|help|html|doc|R|include|tests|examples)/|^(DESCRIPTION|INDEX|NAMESPACE|MD5)$", ids)]
    if (!package %in% c("pipapi", "wbpip", "plumber", "jsonlite", "qs2")) {
      ids <- ids[grepl("^(libs|data)/", ids)]
    }
    assets <- lapply(ids, function(id) list(id = id,
      sha256 = .cache_v2_file_sha(file.path(root, id))))
    data_root <- system.file("data", package = package)
    if (nzchar(data_root) && !dir.exists(file.path(root, "data"))) {
      data_ids <- list.files(data_root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
      assets <- c(assets, lapply(data_ids, function(id) list(id = paste0("data/", id),
        sha256 = .cache_v2_file_sha(file.path(data_root, id)))))
    }
    code <- if (package %in% c("pipapi", "wbpip", "plumber", "jsonlite", "qs2")) {
      .cache_v2_code(asNamespace(package))
    } else NULL
    list(version = desc$Version, remote_sha = desc$RemoteSha,
      code_digest = code, assets = assets)
  })
  names(entries) <- packages
  identity <- list(schema = 2L, r_version = paste(R.version$major, R.version$minor, sep = "."),
    packages = entries, serializer = "qs2/default;json/exact-utf8-bytes")
  c(list(fingerprint = .cache_v2_sha(.cache_v2_json(identity)),
    method = "loaded-function-formals-and-bodies, static namespace constants and runtime assets; no transient environments"),
    identity)
}

.cache_v2_assert_wrappers <- function() {
  for (operation in c("pip", "pip_agg", "ui_cp_charts", "ui_cp_download", "ui_cp_key_indicators")) {
    fun <- get(operation, envir = environment(.cache_v2_assert_wrappers))
    if (is.null(attr(fun, "cache_v2_original", exact = TRUE)) ||
        !identical(attr(fun, "cache_v2_mode", exact = TRUE), 2L)) {
      stop("Cache v2 wrappers are not installed; set both cache flags before loading pipapi in a fresh process.")
    }
  }
  invisible(TRUE)
}

cache_v2_configure <- function(root, manifest, build, planned_keys = character(),
                              intermediate_mode = "write", required = TRUE,
                              runtime_max_size = 1024^3) {
  if (!cache_v2_enabled()) stop("Cache v2 requires PIPAPI_CACHE_V2=TRUE and PIPAPI_APPLY_CACHING=TRUE.")
  .cache_v2_assert_wrappers()
  intermediate_mode <- match.arg(intermediate_mode, c("write", "read_only", "read", "readonly", "off", "none"))
  if (!identical(cache_v2_build()$fingerprint, build$fingerprint)) {
    stop("Cache v2 actual installed build does not match the required build.")
  }
  if (!length(manifest$versions) || !nzchar(manifest$fingerprint)) stop("Invalid source manifest.")
  if (length(runtime_max_size) != 1L || !is.finite(runtime_max_size) || runtime_max_size <= 0) {
    stop("runtime_max_size must be positive.")
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  .cache_v2_state$config <- list(root = root, manifest = manifest, build = build,
    planned_keys = unique(planned_keys), intermediate_mode = intermediate_mode,
    required = required, runtime_max_size = runtime_max_size)
  .cache_v2_disk_space(root, 0)
  invisible(.cache_v2_state$config)
}

.cache_v2_taint_path <- function(fingerprint) {
  file.path(.cache_v2_state$config$root, "v2", ".tainted", fingerprint)
}

cache_v2_taint <- function(reason = "Source verification failed", versions = NULL) {
  cfg <- .cache_v2_state$config
  if (is.null(cfg)) stop("Cache v2 is not configured.")
  if (is.null(versions)) versions <- names(cfg$manifest$versions)
  for (entry in cfg$manifest$versions[versions]) {
    path <- .cache_v2_taint_path(entry$fingerprint)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    if (!file.exists(path) && !file.create(path)) stop("Cannot record tainted cache provenance.")
  }
  invisible(reason)
}

.cache_v2_guard <- function(identity, inputs = FALSE) {
  descriptor <- identity$descriptor
  cfg <- .cache_v2_state$config
  if (!is.null(descriptor$version) &&
      (!identical(descriptor$dependency_fingerprint, cfg$manifest$versions[[descriptor$version]]$fingerprint) ||
       !identical(descriptor$build_fingerprint, cfg$build$fingerprint))) {
    stop("Cache v2 identity does not match configured provenance.")
  }
  if (file.exists(.cache_v2_taint_path(identity$descriptor$dependency_fingerprint))) {
    stop("Cache v2 provenance is tainted; use a clean cache root after source verification.")
  }
  if (inputs) cache_v2_assert_inputs(.cache_v2_state$config$manifest, full = FALSE)
}

cache_v2_attach <- function(lkup, version) {
  cfg <- .cache_v2_state$config
  if (is.null(cfg)) stop("Cache v2 is not configured.")
  source <- cfg$manifest$versions[[version]]
  if (is.null(source)) stop("Full version is absent from the source manifest: ", version)
  lkup$cache_v2 <- list(version = version, dependency_fingerprint = source$fingerprint,
    build_fingerprint = cfg$build$fingerprint, lookup_variant = "full")
  lkup
}

cache_v2_cp_lookup <- function(lkup) {
  lkup$svy_lkup <- lkup$svy_lkup[lkup$svy_lkup$display_cp == 1, ]
  if (!is.null(lkup$cache_v2)) lkup$cache_v2$lookup_variant <- "cp"
  lkup
}

cache_v2_context <- function(lkup = NULL) {
  if (is.null(lkup)) return(getOption("pipapi.cache_v2_context"))
  cfg <- .cache_v2_state$config
  stamp <- lkup$cache_v2
  if (is.null(cfg) || is.null(stamp)) return(NULL)
  source <- cfg$manifest$versions[[stamp$version]]
  if (!identical(stamp$build_fingerprint, cfg$build$fingerprint) ||
      !identical(stamp$dependency_fingerprint, source$fingerprint)) {
    stop("Lookup provenance does not match cache v2 configuration.")
  }
  c(list(root = cfg$root, intermediate_mode = cfg$intermediate_mode,
    revision = .cache_v2_sha(.cache_v2_json(stamp[c("version", "dependency_fingerprint", "build_fingerprint")]))), stamp)
}

cache_v2_effective_args <- function(operation, args, lkup) {
  if (!is.list(args) || (length(args) && (is.null(names(args)) || anyDuplicated(names(args))))) {
    stop("Effective arguments must be a named list.")
  }
  compute <- operation %in% c("pip", "pip_agg", "ui_cp_charts", "ui_cp_download",
    "ui_cp_key_indicators", "ui_hp_stacked", "ui_pc_charts", "ui_pc_regional")
  if (compute) {
    fun <- if (exists(operation, .cache_v2_state$originals, inherits = FALSE)) {
      get(operation, .cache_v2_state$originals)
    } else get(operation, envir = environment(cache_v2_effective_args))
    formal <- formals(fun)
    if (length(setdiff(names(args), names(formal)))) stop("Unknown computation argument.")
    scope <- list2env(c(list(lkup = lkup), args[setdiff(names(args), "lkup")]), parent = environment(fun))
    for (name in setdiff(names(formal), c("lkup", "lkup_hash", "..."))) {
      if (!name %in% names(args)) {
        args[name] <- list(eval(formal[[name]], scope))
        assign(name, args[[name]], scope)
      }
    }
  }
  args[c("lkup", "lkup_hash")] <- NULL
  if (operation %in% c("pip", "pip_agg", "ui_pc_charts", "ui_pc_regional")) {
    args$country <- toupper(args$country)
    if (is.character(args$year)) args$year <- toupper(args$year)
    for (name in intersect(c("welfare_type", "reporting_level", "group_by"), names(formal))) {
      choices <- if (name == "group_by" && operation == "pip_agg") NULL else eval(formal[[name]], scope)
      if (!is.null(choices) && !is.null(args[[name]])) args[[name]] <- match.arg(args[[name]], choices)
    }
  }
  if (operation == "ui_cp_charts") args$country <- args$country[1L]
  if (!is.null(args$year)) {
    year <- suppressWarnings(as.numeric(args$year))
    if (length(year) && all(is.finite(year))) args$year <- year
  }
  for (name in c("povline", "popshare", "ppp", "pop_units")) {
    if (is.numeric(args[[name]])) args[[name]] <- as.numeric(args[[name]])
  }
  args
}

.cache_v2_path <- function(identity) {
  cfg <- .cache_v2_state$config
  root <- cfg$root
  if (!identity$key %in% cfg$planned_keys) root <- file.path(root, "runtime")
  d <- identity$descriptor
  file.path(root, "v2", d$version, d$kind, d$operation,
    substr(identity$key, 1L, 2L), paste0(identity$key, if (d$kind == "response") ".json" else ".qs"))
}

cache_v2_identity <- function(operation, args, lkup, representation = NULL) {
  context <- cache_v2_context(lkup)
  if (is.null(context)) stop("Cache v2 needs configured lookup provenance.")
  .cache_v2_component(operation)
  parameters <- cache_v2_effective_args(operation, args, lkup)
  povline <- parameters$povline
  cacheable <- is.null(povline) || (is.numeric(povline) && all(is.finite(povline)) &&
    all(abs(povline * 100 - round(povline * 100)) < 1e-8))
  if (cacheable && !is.null(povline)) {
    parameters$povline <- NULL
    parameters$povline_cents <- round(povline * 100)
  }
  # JSON types are explicit; vectors retain their original order, including names.
  typed <- lapply(parameters, function(x) {
    if (is.null(x)) return(list(type = "null", value = NULL))
    if (!is.atomic(x) || is.object(x) || anyNA(x)) stop("Unsupported effective argument type.")
    list(type = if (is.numeric(x)) "number" else typeof(x),
      value = unname(as.list(x)), names = names(x))
  })
  version_parts <- strsplit(context$version, "_", fixed = TRUE)[[1L]]
  descriptor <- list(schema = 2L, operation = operation,
    kind = if (is.null(representation)) "compute" else "response",
    version = context$version, ppp_version = version_parts[2L],
    parameters = typed, lookup_variant = context$lookup_variant,
    dependency_fingerprint = context$dependency_fingerprint,
    build_fingerprint = context$build_fingerprint, representation = representation)
  canonical <- .cache_v2_json(descriptor)
  out <- list(key = .cache_v2_sha(canonical), descriptor = descriptor,
    canonical = canonical, cacheable = cacheable, effective_args = cache_v2_effective_args(operation, args, lkup))
  out$path <- .cache_v2_path(out)
  out
}

.cache_v2_disk_space <- function(path, bytes) {
  probe <- getOption("pipapi.cache_v2_disk_usage", ps::ps_disk_usage)
  free <- probe(path)$available
  if (!length(free) || anyNA(free) || any(free < bytes + 1024^2)) {
    stop("Insufficient disk space for cache v2 publication.")
  }
  invisible(TRUE)
}

cache_v2_with_lock <- function(identity, expr) {
  path <- .cache_v2_path(identity)
  lockpath <- paste0(path, ".lock")
  if (exists(lockpath, .cache_v2_state$locks, inherits = FALSE)) return(eval.parent(substitute(expr)))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock <- filelock::lock(lockpath, timeout = getOption("pipapi.cache_v2_lock_timeout", 60000))
  if (is.null(lock)) stop("Timed out waiting for cache v2 lock: ", identity$key)
  assign(lockpath, TRUE, .cache_v2_state$locks)
  on.exit({ rm(list = lockpath, envir = .cache_v2_state$locks); filelock::unlock(lock) }, add = TRUE)
  eval.parent(substitute(expr))
}

cache_v2_get <- function(identity) {
  miss <- function(state) list(hit = FALSE, state = state, value = NULL, metadata = NULL)
  if (isTRUE(getOption("pipapi.query_live_data")) || identical(identity$cacheable, FALSE)) return(miss("missing"))
  .cache_v2_guard(identity)
  path <- .cache_v2_path(identity)
  meta_path <- paste0(path, ".meta.json")
  if (!file.exists(path) && !file.exists(meta_path)) return(miss("missing"))
  if (!file.exists(path) || !file.exists(meta_path)) return(miss("corrupt"))
  tryCatch({
    meta <- jsonlite::fromJSON(meta_path, simplifyVector = FALSE)
    if (!identical(meta$state, "complete") || !identical(meta$key, identity$key) ||
        !identical(meta$canonical, identity$canonical) ||
        !identical(.cache_v2_json(meta$descriptor), identity$canonical) ||
        !identical(as.numeric(meta$size), unname(file.size(path))) ||
        !identical(meta$sha256, .cache_v2_file_sha(path))) return(miss("corrupt"))
    if (identity$descriptor$kind == "response" &&
        (!is.character(meta$content_type) || length(meta$content_type) != 1L ||
         is.na(meta$content_type) || !nzchar(meta$content_type))) return(miss("corrupt"))
    value <- if (identity$descriptor$kind == "response") {
      con <- file(path, "rb"); on.exit(close(con), add = TRUE)
      readBin(con, "raw", n = file.size(path))
    } else qs2::qs_read(path)
    .cache_v2_count("hit", identity$descriptor$operation)
    list(hit = TRUE, state = "complete", value = data.table::copy(value), metadata = meta)
  }, error = function(e) miss("corrupt"))
}

.cache_v2_rename <- function(from, to) {
  rename <- getOption("pipapi.cache_v2_rename", file.rename)
  if (!isTRUE(rename(from, to))) stop("Cache v2 atomic rename failed: ", basename(to))
}

.cache_v2_runtime_room <- function(bytes, keep) {
  cfg <- .cache_v2_state$config
  if (bytes > cfg$runtime_max_size) stop("Artifact exceeds the runtime cache limit.")
  root <- file.path(cfg$root, "runtime")
  paths <- list.files(root, pattern = "\\.(qs|json)$", recursive = TRUE, full.names = TRUE)
  paths <- paths[!grepl("\\.meta\\.json$", paths) & paths != keep]
  info <- file.info(paths)
  metadata_sizes <- file.size(paste0(paths, ".meta.json"))
  metadata_sizes[is.na(metadata_sizes)] <- 0
  info$size <- info$size + metadata_sizes
  used <- sum(info$size, na.rm = TRUE)
  for (i in order(info$mtime)) {
    if (used + bytes <= cfg$runtime_max_size) break
    lockpath <- paste0(paths[i], ".lock")
    if (exists(lockpath, .cache_v2_state$locks, inherits = FALSE)) next
    lock <- filelock::lock(lockpath, timeout = 0)
    if (is.null(lock)) next
    tryCatch({
      if (unlink(c(paths[i], paste0(paths[i], ".meta.json"))) != 0L) stop("Runtime cache eviction failed.")
      used <- used - info$size[i]
    }, finally = filelock::unlock(lock))
  }
  if (used + bytes > cfg$runtime_max_size) stop("Runtime cache is full; entries are in use.")
}

cache_v2_put <- function(identity, value, content_type = NULL) {
  if (isTRUE(getOption("pipapi.query_live_data")) || identical(identity$cacheable, FALSE)) return(invisible(FALSE))
  if (inherits(value, c("condition", "try-error")) ||
      (is.list(value) && (identical(value$ok, FALSE) || !is.null(value$error)))) {
    stop("Cache v2 cannot publish an error result.")
  }
  cache_v2_with_lock(identity, {
    existing <- cache_v2_get(identity)
    if (existing$hit) return(invisible(existing$metadata))
    .cache_v2_guard(identity, inputs = TRUE)
    path <- .cache_v2_path(identity)
    .cache_v2_disk_space(dirname(path), as.numeric(utils::object.size(value)))
    temp <- tempfile(".cache-v2-", tmpdir = dirname(path))
    meta_temp <- paste0(temp, ".meta")
    on.exit(unlink(c(temp, meta_temp)), add = TRUE)
    if (identity$descriptor$kind == "response") {
      if (!is.character(content_type) || length(content_type) != 1L ||
          is.na(content_type) || !nzchar(content_type)) stop("Response cache requires a content type.")
      if (is.character(value) && length(value) == 1L && !is.na(value)) value <- charToRaw(enc2utf8(value))
      if (!is.raw(value)) stop("Response cache requires exact JSON bytes.")
      if (!jsonlite::validate(rawToChar(value))) stop("Response cache requires valid JSON.")
      con <- file(temp, "wb")
      tryCatch(writeBin(value, con), finally = close(con))
    } else {
      qs2::qs_save(value, temp)
      qs2::qs_read(temp)
    }
    size <- unname(file.size(temp))
    .cache_v2_disk_space(dirname(path), size)
    metadata <- list(state = "complete", key = identity$key, canonical = identity$canonical,
      descriptor = identity$descriptor, sha256 = .cache_v2_file_sha(temp), size = size,
      content_type = content_type)
    con <- file(meta_temp, "wb")
    tryCatch(writeBin(charToRaw(.cache_v2_json(metadata)), con), finally = close(con))
    runtime_lock <- NULL
    if (!identity$key %in% .cache_v2_state$config$planned_keys) {
      runtime_lock <- filelock::lock(file.path(.cache_v2_state$config$root, "runtime.lock"),
        timeout = getOption("pipapi.cache_v2_lock_timeout", 60000))
      if (is.null(runtime_lock)) stop("Timed out waiting for runtime cache maintenance.")
      on.exit(filelock::unlock(runtime_lock), add = TRUE)
      .cache_v2_runtime_room(size + file.size(meta_temp), path)
    }
    .cache_v2_guard(identity, inputs = TRUE)
    # Only corrupt/incomplete targets reach here. Move them aside before repair;
    # a complete target is never removed or replaced, including on Windows.
    for (target in c(paste0(path, ".meta.json"), path)) {
      if (file.exists(target)) {
        quarantine <- tempfile(".corrupt-", tmpdir = dirname(path))
        .cache_v2_rename(target, quarantine)
        unlink(quarantine)
      }
    }
    .cache_v2_rename(temp, path)
    .cache_v2_rename(meta_temp, paste0(path, ".meta.json"))
    .cache_v2_count("write", identity$descriptor$operation)
    invisible(metadata)
  })
}

.cache_v2_wrap <- function(operation, original) {
  assign(operation, original, .cache_v2_state$originals)
  wrapper <- function() NULL
  formals(wrapper) <- formals(original)
  environment(wrapper) <- environment()
  body(wrapper) <- quote({
    frame <- environment()
    arg_names <- setdiff(names(formals(original)), c("lkup_hash", "..."))
    args <- stats::setNames(lapply(arg_names, get, envir = frame, inherits = FALSE), arg_names)
    lkup <- args$lkup
    context <- cache_v2_context(lkup)
    if (is.null(context)) stop("Cache v2 computation requires configured lookup provenance.")
    effective <- cache_v2_effective_args(operation, args, lkup)
    context$parameters <- effective
    context$ppp <- effective$ppp
    context$popshare <- effective$popshare
    old <- options(pipapi.cache_v2_context = context)
    on.exit(options(old), add = TRUE)
    if (isTRUE(getOption("pipapi.query_live_data"))) return(do.call(original, args))
    identity <- cache_v2_identity(operation, args, lkup)
    if (!identity$cacheable) return(do.call(original, args))
    .cache_v2_guard(identity, inputs = TRUE)
    hit <- cache_v2_get(identity)
    if (hit$hit) return(hit$value)
    cache_v2_with_lock(identity, {
      hit <- cache_v2_get(identity)
      if (hit$hit) return(hit$value)
      started <- proc.time()[["elapsed"]]
      value <- do.call(original, args)
      .cache_v2_count("compute", operation, proc.time()[["elapsed"]] - started)
      cache_v2_put(identity, value)
      data.table::copy(value)
    })
  })
  attr(wrapper, "cache_v2_original") <- original
  attr(wrapper, "cache_v2_mode") <- 2L
  wrapper
}
