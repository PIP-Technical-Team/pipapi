# Cache v2 deliberately does not use memoise hashes or legacy lookup signatures.
.cache_v2_state <- new.env(parent = emptyenv())
.cache_v2_state$config <- NULL
.cache_v2_state$locks <- new.env(parent = emptyenv())
.cache_v2_state$bootstrap_locks <- new.env(parent = emptyenv())
.cache_v2_state$originals <- new.env(parent = emptyenv())
.cache_v2_state$endpoint_fingerprints <- new.env(parent = emptyenv())
.cache_v2_state$build_identity <- NULL
.cache_v2_state$stats <- list()
.cache_v2_state$verified_inputs <- list(fingerprint = NULL, time = as.POSIXct(NA))

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

.cache_v2_canonical_path <- function(lkup) {
  if (!is.list(lkup) || !is.character(lkup$data_root) ||
      length(lkup$data_root) != 1L || is.na(lkup$data_root) ||
      !nzchar(lkup$data_root)) {
    stop("A lookup with a non-empty data_root is required.")
  }
  file.path(lkup$data_root, "cache.duckdb")
}

.cache_v2_compute_enabled <- function() {
  is.null(.cache_v2_state$config$compute_cache) || isTRUE(.cache_v2_state$config$compute_cache)
}

.cache_v2_inputs <- function(root) {
  dirs <- c("_aux", "estimations", "survey_data", "lineup_data")
  if (!all(dir.exists(file.path(root, dirs)))) {
    stop("Incomplete cache v2 source tree: ", root)
  }
  ids <- unlist(lapply(dirs, function(d) file.path(
    d, list.files(file.path(root, d), recursive = TRUE,
                  all.files = TRUE, no.. = TRUE)
  )), use.names = FALSE)
  ids <- gsub("\\\\", "/", ids)
  estimation_tables <- paste0(
    "(prod_svy_estimation|prod_ref_estimation|dist_stats|prod_refy_estimation|",
    "lineup_years|lineup_dist_stats)\\.fst$"
  )
  ids <- ids[
    grepl("^_aux/[^/]+\\.fst$", ids, ignore.case = TRUE) |
    grepl("^_aux/(country_profiles|censored|survey_metadata)\\.rds$", ids,
          ignore.case = TRUE) |
    grepl(paste0("^estimations/", estimation_tables), ids, ignore.case = TRUE) |
    grepl("^(survey_data|lineup_data)/[^/]+\\.fst$", ids, ignore.case = TRUE)
  ]
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
    # A release timestamp is provenance, not an input to precached statistics.
    # Neither a copied file's mtime nor a timestamp edit changes the data key.
    identity <- lapply(files, function(f) {
      f$scan_mtime <- NULL
      f$source_timestamp <- NULL
      f
    })
    list(fingerprint = .cache_v2_sha(.cache_v2_json(list(version = version,
      files = identity))), files = files,
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

.cache_v2_function_descriptor <- function(package, name) {
  ns <- asNamespace(package)
  fun <- get(name, envir = ns, inherits = FALSE)
  original <- attr(fun, "cache_v2_original", exact = TRUE)
  if (!is.null(original)) fun <- original
  if (!is.function(fun)) stop("Cache v2 dependency is not a function: ", package, "::", name)
  list(
    package = package,
    name = name,
    formals = paste(deparse(formals(fun), width.cutoff = 500L), collapse = "\n"),
    body = paste(deparse(body(fun), width.cutoff = 500L), collapse = "\n")
  )
}

.cache_v2_binding_fingerprint <- function(value, id) {
  normalize <- function(x) {
    if (is.function(x)) {
      return(list(formals = paste(deparse(formals(x), width.cutoff = 500L), collapse = "\n"),
                  body = paste(deparse(body(x), width.cutoff = 500L), collapse = "\n")))
    }
    if (inherits(x, "data.table")) x <- as.data.frame(x)
    if (is.list(x)) {
      out <- lapply(x, normalize)
      attributes(out) <- lapply(attributes(x), normalize)
      return(out)
    }
    .cache_v2_constant(x)
  }
  static <- tryCatch(normalize(value), error = function(e) {
    stop("Unsupported cache dependency binding: ", id, " (", conditionMessage(e), ")")
  })
  digest::digest(static, algo = "sha256", serializeVersion = 3L)
}

.cache_v2_namespace_calls <- function(expr) {
  out <- list()
  visit <- function(node) {
    if (!is.call(node) && !is.expression(node) && !is.pairlist(node)) return()
    if (is.call(node) && length(node) == 3L &&
        (identical(node[[1L]], as.name("::")) ||
         identical(node[[1L]], as.name(":::")))) {
      if (is.symbol(node[[2L]]) && is.symbol(node[[3L]])) {
        out[[length(out) + 1L]] <<- c(as.character(node[[2L]]), as.character(node[[3L]]))
      }
    }
    for (i in seq_along(node)) {
      if (!rlang::is_missing(node[[i]])) visit(node[[i]])
    }
  }
  visit(expr)
  out
}

.cache_v2_dynamic_audit <- list(
  # get() selects source-data columns/tables, not R functions; those inputs are
  # covered by the source manifest and the effective request arguments.
  "pipapi::get_aux_table" = "get",
  "pipapi::subset_ctry_years" = "get",
  # These expressions select columns and values from the versioned lookups.
  "pipapi::get_ctr_alt_agg" = c("eval", "parse"),
  "pipapi::get_impl_ctrs" = c("eval", "parse"),
  "pipapi::pip_grp_logic" = c("eval", "parse"),
  # The target function itself is also a statically reachable namespace call.
  "pipapi::create_vintage_pattern_call" = "do.call",
  "pipapi::get_caller_names" = "sys.calls",
  # Operation targets are the endpoint's explicit UI root; formals are hashed.
  "pipapi::cache_v2_effective_args" = c("get", "eval")
)

# External wbpip function bodies are excluded; its immutable Version is part
# of the release identity instead.
.cache_v2_output_dependency <- function(package, name) {
  if (!identical(package, "pipapi")) return(FALSE)
  if (name %in% c("cache_v2_effective_args", "cache_v2_cp_lookup")) return(TRUE)
  if (grepl("^\\.?cache_v2_|^intermediate_cache_", name)) return(FALSE)
  !name %in% c("return_if_exists", "update_master_file", "safe_update_master_file",
               "load_inter_cache", "with_intermediate_db", "create_duckdb_file",
               "delete_cache", "reset_cache", "connect_with_retry")
}

.cache_v2_function_fingerprint <- function(dependencies, schema = 4L) {
  if (!is.list(dependencies) || !length(dependencies)) {
    stop("Cache v2 endpoint dependencies must not be empty.")
  }
  pending <- lapply(dependencies, function(dependency) {
    if (!is.character(dependency) || length(dependency) != 2L ||
        anyNA(dependency) || any(!nzchar(dependency))) {
      stop("Cache v2 endpoint dependencies must contain package and function names.")
    }
    dependency
  })
  functions <- list()
  constants <- list()
  queue_embedded <- function(value, package) {
    if (is.function(value)) {
      if (!identical(environment(value), asNamespace(package))) {
        stop("Dynamic cache dependency has a captured non-package environment: ", package)
      }
      refs <- codetools::findGlobals(value, merge = FALSE)$functions
      for (symbol in refs) {
        if (exists(symbol, envir = asNamespace(package), inherits = FALSE) &&
            is.function(get(symbol, envir = asNamespace(package), inherits = FALSE))) {
          pending[[length(pending) + 1L]] <<- c(package, symbol)
        }
      }
      for (target in .cache_v2_namespace_calls(body(value))) {
        if (target[[1L]] %in% c("pipapi", "wbpip")) {
          pending[[length(pending) + 1L]] <<- target
        }
      }
    } else if (is.list(value)) {
      for (item in value) queue_embedded(item, package)
    }
  }
  while (length(pending)) {
    dependency <- pending[[1L]]
    pending <- pending[-1L]
    package <- dependency[[1L]]
    name <- dependency[[2L]]
    id <- paste(package, name, sep = "::")
    if (!.cache_v2_output_dependency(package, name)) next
    if (!is.null(functions[[id]])) next
    ns <- asNamespace(package)
    fun <- if (exists(name, envir = ns, inherits = FALSE)) {
      get(name, envir = ns, inherits = FALSE)
    } else {
      # Packaged data exports are available through :: but not as namespace
      # bindings (for example, pipapi::empty_response).
      tryCatch(getExportedValue(package, name), error = function(e) {
        stop("Missing cache dependency: ", id)
      })
    }
    if (!is.function(fun)) {
      constants[[id]] <- .cache_v2_binding_fingerprint(fun, id)
      queue_embedded(fun, package)
      next
    }
    original <- attr(fun, "cache_v2_original", exact = TRUE)
    if (is.function(original)) fun <- original
    if (!is.function(fun)) stop("Cache dependency is not a function: ", id)
    functions[[id]] <- .cache_v2_function_descriptor(package, name)
    if (!package %in% c("pipapi", "wbpip")) next
    # Discover direct calls and global constants from the installed namespace.
    # Only traverse package-owned output functions. External implementations
    # are not used as package-wide invalidation signals.
    globals <- codetools::findGlobals(fun, merge = FALSE)
    dynamic <- intersect(c("get", "get0", "do.call", "eval", "parse", "UseMethod",
                           "getExportedValue", "sys.calls"), globals$functions)
    expected_dynamic <- .cache_v2_dynamic_audit[[id]]
    if (!setequal(dynamic, if (is.null(expected_dynamic)) character() else expected_dynamic)) {
      stop("Dynamic cache dependency audit is incomplete for: ", id)
    }
    for (symbol in unique(c(globals$functions, globals$variables))) {
      # Request-local cache state is verified by the runtime cache guard; it
      # must not be serialized into a portable computation fingerprint.
      if (package == "pipapi" && symbol == ".cache_v2_state") next
      if (!exists(symbol, envir = ns, inherits = FALSE)) next
      binding <- get(symbol, envir = ns, inherits = FALSE)
      if (package == "pipapi" && symbol == ".pipapienv") {
        constants[["pipapi::pl_to_store"]] <- .cache_v2_binding_fingerprint(
          get0("pl_to_store", envir = binding, inherits = FALSE), "pipapi::pl_to_store")
        next
      }
      if (is.function(binding)) {
        pending[[length(pending) + 1L]] <- c(package, symbol)
      } else {
        binding_id <- paste(package, symbol, sep = "::")
        constants[[binding_id]] <- .cache_v2_binding_fingerprint(binding, binding_id)
        queue_embedded(binding, package)
      }
    }
    # Explicit namespace calls are not reported as ordinary global symbols.
    for (target in c(.cache_v2_namespace_calls(formals(fun)),
                     .cache_v2_namespace_calls(body(fun)))) {
      if (!target[[1L]] %in% c("pipapi", "wbpip")) next
      pending[[length(pending) + 1L]] <- target
    }
  }
  descriptors <- list(
    functions = functions[sort(names(functions), method = "radix")],
    constants = constants[sort(names(constants), method = "radix")]
  )
  list(schema = as.integer(schema),
       fingerprint = .cache_v2_sha(.cache_v2_json(descriptors)),
       members = names(descriptors$functions),
       constants = names(descriptors$constants))
}

#' Fingerprint the installed computation used by the canonical cache
#'
#' Only reachable pipapi calculation functions and the immutable wbpip package
#' Version enter this identity. The pipapi release and Git commits are audit data.
#' @param refresh Recompute the identity in this process when `TRUE`.
#' @return A portable behavior fingerprint and its dependency contract.
#' @export
cache_v2_build <- function(refresh = FALSE) {
  if (!refresh && !is.null(.cache_v2_state$build_identity)) {
    return(.cache_v2_state$build_identity)
  }
  # Only the installed functions that create canonical intermediate estimates
  # (and the lookups supplied to them) belong in this build identity. A
  # pipapi release number or an unrelated UI function is not an input.
  compute <- .cache_v2_function_fingerprint(list(
    c("pipapi", "pip"), c("pipapi", "create_versioned_lkups")
  ))
  identity <- list(schema = 4L, compute = compute$fingerprint,
                   wbpip_version = as.character(utils::packageVersion("wbpip")))
  result <- c(list(fingerprint = .cache_v2_sha(.cache_v2_json(identity)),
    method = "reachable pipapi output functions and immutable wbpip version"),
    identity)
  .cache_v2_state$build_identity <- result
  result
}

#' Create the portable cache release contract
#'
#' This is the portable behavior contract shared by Windows builders and Linux
#' servers. The pipapi package version and Git commits are audit metadata, not
#' cache keys. The wbpip package Version is a code-change input.
#' @return A release contract and its fingerprint.
#' @export
cache_v2_release_contract <- function() {
  build <- cache_v2_build()
  endpoints <- c("hp-stacked", "pc-charts", "pc-regional-aggregates",
                 "cp-charts", "cp-key-indicators")
  response <- setNames(lapply(endpoints, function(endpoint) {
    cache_v2_response_spec(endpoint)$endpoint_fingerprint
  }), endpoints)
  identity <- list(schema = 4L, build = build$fingerprint, response = response)
  c(list(fingerprint = .cache_v2_sha(.cache_v2_json(identity))), identity)
}

.cache_v2_release_fields <- function(contract) {
  c("contract schema" = as.character(contract$schema),
    "pipapi functions and wbpip version" = as.character(contract$build),
    setNames(vapply(contract$response, as.character, character(1)),
             paste0("response ", names(contract$response))))
}

.cache_v2_validate_release <- function(contract) {
  if (!is.list(contract) || !identical(as.integer(contract$schema), 4L) ||
      !is.list(contract$response) ||
      !identical(names(contract$response), c("hp-stacked", "pc-charts",
        "pc-regional-aggregates", "cp-charts", "cp-key-indicators"))) {
    stop("Cache v2 release contract is incomplete or invalid.")
  }
  fields <- .cache_v2_release_fields(contract)
  if (anyNA(fields) || any(!nzchar(fields[-1L])) ||
      any(!grepl("^[0-9a-f]{64}$", fields[-1L]))) {
    stop("Cache v2 release contract is incomplete or invalid.")
  }
  identity <- contract[c("schema", "build", "response")]
  expected <- .cache_v2_sha(.cache_v2_json(identity))
  if (!is.character(contract$fingerprint) || length(contract$fingerprint) != 1L ||
      !identical(contract$fingerprint, expected)) {
    stop("Cache v2 release contract fingerprint is invalid.")
  }
  invisible(TRUE)
}

.cache_v2_assert_release <- function(expected, actual = cache_v2_release_contract()) {
  .cache_v2_validate_release(expected)
  .cache_v2_validate_release(actual)
  expected_fields <- .cache_v2_release_fields(expected)
  actual_fields <- .cache_v2_release_fields(actual)
  fields <- union(names(expected_fields), names(actual_fields))
  different <- fields[vapply(fields, function(field) {
    !identical(unname(expected_fields[[field]]), unname(actual_fields[[field]]))
  }, logical(1))]
  if (length(different)) {
    value <- function(values, field) {
      out <- values[[field]]
      if (is.null(out) || !length(out) || is.na(out) || !nzchar(out)) "<missing>" else out
    }
    detail <- vapply(different, function(field) sprintf(
      "%s: cache=%s, server=%s", field,
      value(expected_fields, field), value(actual_fields, field)
    ), character(1))
    stop("Cache v2 release does not match this server:\n- ",
         paste(detail, collapse = "\n- "),
         "\nRebuild the cache or deploy the package revisions recorded in the cache.")
  }
  invisible(TRUE)
}

.cache_v2_assert_wrappers <- function() {
  for (operation in c("pip", "pip_agg", "ui_hp_stacked", "ui_pc_charts", "ui_pc_regional",
                      "ui_cp_charts", "ui_cp_download", "ui_cp_key_indicators")) {
    fun <- get(operation, envir = asNamespace("pipapi"))
    if (is.null(attr(fun, "cache_v2_original", exact = TRUE)) ||
        !identical(attr(fun, "cache_v2_mode", exact = TRUE), 2L)) {
      stop("Cache v2 wrappers are not installed; set both cache flags before loading pipapi in a fresh process.")
    }
  }
  invisible(TRUE)
}

#' Configure cache v2
#'
#' Normal API and response-cache access is read-only for intermediate DuckDB
#' files. Use [cache_v2_bootstrap_intermediate()] for the explicit write path.
#'
#' @param root Response cache root.
#' @param manifest Source manifest returned by [cache_v2_manifest()].
#' @param build Build fingerprint returned by [cache_v2_build()].
#' @param planned_keys Complete response keys that must not be evicted.
#' @param intermediate_mode Intermediate DuckDB mode. Defaults to `"read_only"`.
#' @param compute_cache Whether computation results may be stored. Defaults to
#'   `FALSE`; response artifacts are independent of this setting.
#' @param required Whether cache provenance failures are fatal.
#' @param runtime_max_size Maximum size for non-planned runtime artifacts.
#' @return The active cache v2 configuration, invisibly.
#' @export
cache_v2_configure <- function(root, manifest, build, planned_keys = character(),
                              intermediate_mode = "read_only", compute_cache = FALSE,
                              required = TRUE, runtime_max_size = 1024^3) {
  if (!cache_v2_enabled()) stop("Cache v2 requires PIPAPI_CACHE_V2=TRUE and PIPAPI_APPLY_CACHING=TRUE.")
  .cache_v2_assert_wrappers()
  intermediate_mode <- match.arg(intermediate_mode, c("write", "read_only", "read", "readonly", "off", "none"))
  intermediate_mode <- if (intermediate_mode %in% c("read", "readonly")) "read_only" else intermediate_mode
  if (intermediate_mode %in% c("off", "none")) intermediate_mode <- "read_only"
  if (length(compute_cache) != 1L || is.na(compute_cache)) {
    stop("compute_cache must be a single TRUE or FALSE value.")
  }
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
    compute_cache = isTRUE(compute_cache), required = required,
    runtime_max_size = runtime_max_size)
  .cache_v2_state$verified_inputs <- list(fingerprint = NULL, time = as.POSIXct(NA))
  options(pipapi.cache_v2_config = .cache_v2_state$config)
  .cache_v2_disk_space(root, 0)
  invisible(.cache_v2_state$config)
}

#' Validate one canonical intermediate database
#'
#' @param lkup A lookup attached to cache-v2 provenance.
#' @param path Optional canonical database path.
#' @param require_rows Require both result tables to contain rows.
#' @param poverty_lines Optional poverty lines that must exist in both tables.
#' @return A validation report, invisibly.
#' @export
cache_v2_validate_intermediate <- function(lkup, path = NULL, require_rows = FALSE,
                                           poverty_lines = NULL) {
  if (!is.list(lkup) || is.null(lkup$cache_v2)) stop("A cache-v2 lookup is required.")
  if (is.null(path)) path <- intermediate_cache_path(lkup)
  expected <- .cache_v2_canonical_path(lkup)
  if (!identical(fs::path_norm(path), fs::path_norm(expected))) {
    stop("Canonical intermediate path does not match lkup$data_root.")
  }
  context <- attr(path, "cache_v2_context", exact = TRUE)
  if (is.null(context)) {
    context <- cache_v2_context(lkup)
    attr(path, "cache_v2_context") <- context
  }
  if (!file.exists(path)) return(invisible(list(valid = FALSE, missing = TRUE, path = path)))
  context <- attr(path, "cache_v2_context", exact = TRUE)
  if (is.null(context)) context <- cache_v2_context(lkup)
  if (is.null(context)) stop("Valid provenance is required for intermediate cache v2.")
  context$source_root <- lkup$data_root
  attr(path, "cache_v2_context") <- context
  context <- intermediate_cache_context(path)
  report <- with_intermediate_db(path, write = FALSE, function(con) {
    tables <- c("rg_master_file", "fg_master_file")
    present <- vapply(tables, DBI::dbExistsTable, logical(1), conn = con)
    counts <- setNames(vapply(tables, function(table) {
      if (!present[[table]]) return(0)
      DBI::dbGetQuery(con, paste("SELECT COUNT(*) AS n FROM", table))$n[[1L]]
    }, numeric(1)), tables)
    missing_lines <- setNames(lapply(tables, function(table) {
      if (!present[[table]] || is.null(poverty_lines)) return(numeric())
      cached <- DBI::dbGetQuery(con, paste("SELECT DISTINCT poverty_line FROM", table))$poverty_line
      setdiff(unique(round(as.numeric(poverty_lines), 2)), round(as.numeric(cached), 2))
    }), tables)
    coverage_ok <- all(vapply(missing_lines, length, integer(1)) == 0L)
    list(valid = all(present) && (!require_rows || all(counts > 0)) && coverage_ok,
           missing = FALSE, path = path, tables = present, rows = counts,
           missing_poverty_lines = missing_lines,
           context = context)
  }, missing = list(valid = FALSE, missing = TRUE, path = path))
  if (!isTRUE(report$valid)) {
    missing <- unique(unlist(report$missing_poverty_lines, use.names = FALSE))
    detail <- if (length(missing)) {
      paste0("; missing poverty lines: ", paste(sort(missing), collapse = ", "))
    } else ""
    stop("Intermediate database is missing required tables or coverage: ", path, detail)
  }
  invisible(report)
}

#' Bootstrap canonical intermediate DuckDB files
#'
#' This is the only cache-v2 operation that creates or rebuilds canonical
#' intermediate databases. Normal API and pre-cache requests never call it.
#'
#' @param lkups Versioned lookups returned by [create_versioned_lkups()].
#' @param povlines Optional poverty-line vector. If omitted, use each version's
#'   `poverty_lines` auxiliary table.
#' @param recreate Rebuild existing files when `TRUE`.
#' @param country Country selection for bootstrap calculations.
#' @param year Year selection for bootstrap calculations.
#' @return A per-version status data frame.
#' @export
cache_v2_bootstrap_intermediate <- function(lkups, povlines = NULL,
                                            recreate = FALSE, country = "ALL",
                                            year = "ALL") {
  if (!cache_v2_enabled()) stop("Cache v2 requires PIPAPI_CACHE_V2=TRUE and PIPAPI_APPLY_CACHING=TRUE.")
  if (!is.list(lkups)) stop("lkups must be a versioned lookup list.")
  configured <- names(.cache_v2_state$config$manifest$versions)
  versions <- configured
  if (is.null(versions) || !length(versions) || anyNA(versions) || anyDuplicated(versions)) {
    stop("Cache v2 must contain uniquely named full versions.")
  }
  version_paths <- if (!is.null(lkups$versions_paths)) lkups$versions_paths else {
    candidates <- lkups[versions]
    if (all(vapply(candidates, is.list, logical(1)))) candidates else NULL
  }
  if (!is.list(version_paths) || is.null(names(version_paths)) ||
      !all(versions %in% names(version_paths))) {
    stop("lkups are missing configured cache-v2 versions.")
  }
  if (length(recreate) != 1L || is.na(recreate)) stop("recreate must be TRUE or FALSE.")
  old_config <- .cache_v2_state$config
  on.exit(.cache_v2_state$config <- old_config, add = TRUE)
  if (is.null(old_config)) stop("Configure cache v2 before bootstrapping intermediate files.")
  manifest <- old_config$manifest
  build <- old_config$build
  results <- vector("list", length(versions))
  names(results) <- versions
  for (version in versions) {
    lkup <- version_paths[[version]]
    if (!is.list(lkup) || !is.character(lkup$data_root) || length(lkup$data_root) != 1L) {
      stop("Lookup for version ", version, " has no data_root.")
    }
    lkup <- cache_v2_attach(lkup, version)
    bootstrap_context <- cache_v2_context(lkup)
    if (is.null(bootstrap_context)) stop("Valid provenance is required for bootstrap.")
    bootstrap_context$source_root <- lkup$data_root
    bootstrap_context$parameters <- list(ppp = NULL, popshare = NULL)
    bootstrap_context$custom <- list(ppp = NULL, popshare = NULL)
    old_context <- getOption("pipapi.cache_v2_context")
    options(pipapi.cache_v2_context = bootstrap_context)
    on.exit(options(pipapi.cache_v2_context = old_context), add = TRUE)
    path <- .cache_v2_canonical_path(lkup)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    lock <- intermediate_cache_lock(path)
    lock_name <- paste0(path, ".lock")
    assign(lock_name, TRUE, .cache_v2_state$bootstrap_locks)
    on.exit({
      if (exists(lock_name, .cache_v2_state$bootstrap_locks, inherits = FALSE)) {
        rm(list = lock_name, envir = .cache_v2_state$bootstrap_locks)
        filelock::unlock(lock)
      }
    }, add = TRUE)
    if (file.exists(path) && !isTRUE(recreate)) {
      validation <- tryCatch(cache_v2_validate_intermediate(lkup, path), error = identity)
      if (inherits(validation, "error")) {
        filelock::unlock(lock)
        rm(list = lock_name, envir = .cache_v2_state$bootstrap_locks)
        stop(conditionMessage(validation), ". Use recreate = TRUE for an explicit rebuild.")
      }
      filelock::unlock(lock)
      rm(list = lock_name, envir = .cache_v2_state$bootstrap_locks)
      results[[version]] <- data.frame(version = version, path = path,
        status = "reused", stringsAsFactors = FALSE)
      next
    }
    pl <- povlines
    if (is.list(pl)) {
      if (!is.null(names(pl)) && version %in% names(pl)) {
        pl <- pl[[version]]
      } else if (length(pl) == length(versions)) {
        pl <- pl[[match(version, versions)]]
      } else {
        stop("Poverty lines must be named by full version when supplied as a list.")
      }
    }
    if (is.null(pl)) {
      pl <- tryCatch(get_aux_table(lkup$data_root, "poverty_lines"), error = function(e) NULL)
      if (is.data.frame(pl)) {
        column <- intersect(c("poverty_line", "povline", "value"), names(pl))
        if (!length(column)) stop("poverty_lines has no poverty-line column.")
        pl <- pl[[column[[1L]]]]
      }
    }
    if (is.null(pl)) pl <- getOption("pipapi.pl_to_store", numeric())
    pl <- as.numeric(pl)
    if (!length(pl) || any(!is.finite(pl))) stop("Bootstrap poverty lines must be finite.")
    backup <- NULL
    backup_meta <- NULL
    original_exists <- file.exists(path)
    restore_backup <- function() {
      if (!is.null(backup) && file.exists(backup)) {
        unlink(c(path, paste0(path, ".wal")), force = TRUE)
        if (!file.rename(backup, path)) stop("Cannot restore the existing canonical database: ", path)
      }
      if (!is.null(backup_meta) && file.exists(backup_meta)) {
        meta_path <- paste0(path, ".meta.json")
        unlink(meta_path, force = TRUE)
        if (!file.rename(backup_meta, meta_path)) {
          stop("Cannot restore the existing database metadata: ", meta_path)
        }
      }
      invisible(TRUE)
    }
    if (file.exists(path) && isTRUE(recreate)) {
      backup <- tempfile("cache-v2-backup-", tmpdir = dirname(path))
      if (!file.rename(path, backup)) stop("Cannot stage the existing canonical database for rebuild: ", path)
      meta_path <- paste0(path, ".meta.json")
      if (file.exists(meta_path)) {
        backup_meta <- tempfile("cache-v2-backup-meta-", tmpdir = dirname(path))
        if (!file.rename(meta_path, backup_meta)) {
          restore_backup()
          stop("Cannot stage the existing database metadata: ", meta_path)
        }
      }
      unlink(paste0(path, ".wal"), force = TRUE)
    }
    bootstrap_config <- old_config
    bootstrap_config$intermediate_mode <- "write"
    bootstrap_config$compute_cache <- FALSE
    bootstrap_config$planned_keys <- character()
    .cache_v2_state$config <- bootstrap_config
    lkup <- cache_v2_attach(lkup, version)
    write_context <- bootstrap_context
    write_context$intermediate_mode <- "write"
    options(pipapi.cache_v2_context = write_context)
    path <- intermediate_cache_path(lkup)
    committed <- FALSE
    tryCatch({
       pip(country = country, year = year, povline = pl, fill_gaps = FALSE, lkup = lkup)
      pip(country = country, year = year, povline = pl, fill_gaps = TRUE, lkup = lkup)
      .cache_v2_state$config <- bootstrap_config
      .cache_v2_state$config <- old_config
      validation_context <- bootstrap_context
      validation_context$intermediate_mode <- "read_only"
      attr(path, "cache_v2_context") <- validation_context
      options(pipapi.cache_v2_context = validation_context)
      cache_v2_validate_intermediate(lkup, path, require_rows = TRUE)
      if (file.exists(paste0(path, ".wal"))) stop("Bootstrap left a DuckDB WAL: ", path)
      committed <- TRUE
      if (!is.null(backup)) unlink(backup, force = TRUE)
      if (!is.null(backup_meta)) unlink(backup_meta, force = TRUE)
      options(pipapi.cache_v2_context = old_context)
      filelock::unlock(lock)
      rm(list = lock_name, envir = .cache_v2_state$bootstrap_locks)
      results[[version]] <- data.frame(version = version, path = path,
        status = if (isTRUE(recreate)) "rebuilt" else "created", stringsAsFactors = FALSE)
    }, error = function(e) {
      .cache_v2_state$config <- old_config
      if (!committed && is.null(backup) && !original_exists && file.exists(path)) {
        unlink(c(path, paste0(path, ".wal"), paste0(path, ".meta.json")), force = TRUE)
      }
      if (!committed) restore_backup()
      options(pipapi.cache_v2_context = old_context)
      stop("Bootstrap failed for ", version, ": ", conditionMessage(e))
    })
  }
  .cache_v2_state$config <- old_config
  do.call(rbind, results)
}

#' Read and validate a cache-v2 builder configuration from disk
#'
#' The builder writes `v2/cache-config.qs` together with its manifest. This
#' helper validates that configuration against the installed build and the
#' local source tree, then configures normal read-only access. Missing
#' canonical DuckDB files are warnings, not configuration failures.
#'
#' @param cache_root Response cache root.
#' @param data_root Parent data root containing the selected full versions.
#' @param config_path Optional path to `cache-config.qs`.
#' @param intermediate_mode Required intermediate mode. Production mode is
#'   `"read_only"`.
#' @param compute_cache Whether computation artifacts may be written.
#' @param required Whether cache provenance failures are fatal.
#' @param runtime_max_size Maximum size for non-planned runtime artifacts.
#' @return The active cache v2 configuration, invisibly.
#' @export
cache_v2_configure_from_disk <- function(cache_root, data_root,
                                         config_path = NULL,
                                         intermediate_mode = "read_only",
                                         compute_cache = FALSE,
                                         required = TRUE,
                                         runtime_max_size = 1024^3) {
  if (!cache_v2_enabled()) {
    stop("Cache v2 requires PIPAPI_CACHE_V2=TRUE and PIPAPI_APPLY_CACHING=TRUE.")
  }
  if (is.null(config_path)) config_path <- file.path(cache_root, "v2", "cache-config.qs")
  if (!file.exists(config_path)) stop("Cache v2 configuration was not found: ", config_path)
  index <- tryCatch(qs2::qs_read(config_path), error = function(e) {
    stop("Cannot read cache v2 configuration: ", conditionMessage(e))
  })
  meta_path <- paste0(config_path, ".meta.json")
  if (!file.exists(meta_path)) stop("Cache v2 configuration metadata is missing: ", meta_path)
  meta <- tryCatch(jsonlite::fromJSON(meta_path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(meta) || !identical(meta$complete, TRUE) ||
      !identical(as.numeric(meta$size), as.numeric(file.size(config_path))) ||
      !identical(meta$sha256, .cache_v2_file_sha(config_path))) {
    stop("Cache v2 configuration metadata is incomplete or corrupt.")
  }
  required_fields <- c("cache_root", "data_root", "versions", "build", "release_contract",
                       "manifest_fingerprint", "planned_keys")
  if (!is.list(index) || any(!required_fields %in% names(index))) {
    stop("Cache v2 configuration is missing required fields.")
  }
  if (!identical(as.integer(index$schema), 4L)) {
    stop("Cache configuration schema is incompatible; build the behavior-keyed cache once.")
  }
  cache_root <- normalizePath(cache_root, winslash = "/", mustWork = TRUE)
  data_root <- normalizePath(data_root, winslash = "/", mustWork = TRUE)
  validate_index_root <- function(value, runtime, label) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop("Cache v2 configuration has an invalid ", label, ".")
    }
    if (fs::is_absolute_path(value) &&
        !identical(fs::path_norm(normalizePath(value, winslash = "/", mustWork = FALSE)),
                   fs::path_norm(runtime))) {
      stop("Cache v2 configuration ", label, " does not match the runtime root.")
    }
  }
  validate_index_root(index$cache_root, cache_root, "cache_root")
  validate_index_root(index$data_root, data_root, "data_root")
  versions <- as.character(index$versions)
  if (!length(versions) || anyNA(versions) || anyDuplicated(versions)) {
    stop("Cache v2 configuration has invalid full data versions.")
  }
  if (!is.list(index$build) || !is.character(index$build$fingerprint) ||
      length(index$build$fingerprint) != 1L) stop("Cache v2 configuration has an invalid build fingerprint.")
  if (!is.list(index$release_contract) ||
      !is.character(index$release_contract$fingerprint) ||
      length(index$release_contract$fingerprint) != 1L) {
    stop("Cache v2 configuration has an invalid release contract.")
  }
  .cache_v2_assert_release(index$release_contract)
  has_manifest_file <- "manifest_file" %in% names(index)
  manifest_file <- if (has_manifest_file) {
    if (!is.character(index$manifest_file) || length(index$manifest_file) != 1L ||
        is.na(index$manifest_file) || !nzchar(index$manifest_file)) {
      stop("Cache v2 configuration has an invalid manifest_file.")
    }
    candidate <- index$manifest_file
    if (!fs::is_absolute_path(candidate)) candidate <- file.path(cache_root, candidate)
    candidate
  } else file.path(cache_root, "v2", "manifests", paste0(index$manifest_fingerprint, ".qs"))
  cache_prefix <- paste0(fs::path_norm(cache_root), "/")
  manifest_norm <- fs::path_norm(manifest_file)
  if (!fs::is_absolute_path(manifest_file) ||
      !startsWith(paste0(manifest_norm, "/"), cache_prefix) ||
      !grepl("/v2/manifests/[^/]+\\.qs$", manifest_norm)) {
    stop("Cache v2 manifest path escapes cache_root.")
  }
  if (!file.exists(manifest_file)) stop("Cache v2 manifest is missing: ", manifest_file)
  manifest_meta_path <- paste0(manifest_file, ".meta.json")
  if (!file.exists(manifest_meta_path)) stop("Cache v2 manifest metadata is missing: ", manifest_meta_path)
  manifest_meta <- tryCatch(jsonlite::fromJSON(manifest_meta_path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(manifest_meta) || !identical(manifest_meta$complete, TRUE) ||
      !identical(as.numeric(manifest_meta$size), as.numeric(file.size(manifest_file))) ||
      !identical(manifest_meta$sha256, .cache_v2_file_sha(manifest_file))) {
    stop("Cache v2 manifest metadata is incomplete or corrupt.")
  }
  manifest <- tryCatch(qs2::qs_read(manifest_file), error = function(e) {
    stop("Cannot read cache v2 manifest: ", conditionMessage(e))
  })
  if (!is.list(manifest) || is.null(manifest$versions) || is.null(manifest$fingerprint)) {
    stop("Cache v2 manifest is incomplete or corrupt.")
  }
  manifest$data_root <- data_root
  if (!identical(manifest$fingerprint, index$manifest_fingerprint)) {
    stop("Cache v2 source manifest does not match the builder configuration.")
  }
  current_manifest <- cache_v2_manifest(data_root, versions, previous = manifest)
  if (!identical(current_manifest$fingerprint, manifest$fingerprint)) {
    stop("Local source data does not match the builder manifest.")
  }
  if (!is.null(index$planned_keys) && length(index$planned_keys)) {
    if (any(!grepl("^[0-9a-f]{64}$", as.character(index$planned_keys)))) {
      stop("Cache v2 planned_keys contains invalid response keys.")
    }
  }
  if (!identical(intermediate_mode, "read_only")) {
    stop("Disk response caches require read_only intermediate mode.")
  }
  runtime_build <- cache_v2_build()
  old_config <- .cache_v2_state$config
  old_option <- getOption("pipapi.cache_v2_config")
  verified <- FALSE
  on.exit({
    if (!verified) {
      .cache_v2_state$config <- old_config
      options(pipapi.cache_v2_config = old_option)
    }
  }, add = TRUE)
  cache_v2_configure(cache_root, manifest, runtime_build,
    planned_keys = index$planned_keys, intermediate_mode = "read_only",
    compute_cache = compute_cache, required = required,
    runtime_max_size = runtime_max_size)
  verified <- TRUE
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

.cache_v2_guard <- function(identity, inputs = FALSE, force_inputs = FALSE) {
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
  if (inputs) {
    interval <- getOption("pipapi.cache_v2_input_check_interval", 60)
    if (!is.numeric(interval) || length(interval) != 1L || !is.finite(interval) || interval < 0) {
      stop("Cache v2 input check interval must be a finite non-negative number of seconds.")
    }
    verified <- .cache_v2_state$verified_inputs
    elapsed <- as.numeric(difftime(Sys.time(), verified$time, units = "secs"))
    due <- isTRUE(force_inputs) ||
      !identical(verified$fingerprint, cfg$manifest$fingerprint) ||
      !is.finite(elapsed) || elapsed >= interval
    if (due) {
      cache_v2_assert_inputs(cfg$manifest, full = FALSE)
      .cache_v2_state$verified_inputs <- list(
        fingerprint = cfg$manifest$fingerprint,
        time = Sys.time()
      )
    }
  }
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

cache_v2_attach_if_managed <- function(lkup, version) {
  cfg <- .cache_v2_state$config
  if (is.null(cfg)) stop("Cache v2 is not configured.")
  if (version %in% names(cfg$manifest$versions)) {
    return(cache_v2_attach(lkup, version))
  }
  if (!is.null(lkup[["cache_v2", exact = TRUE]])) {
    stop("Lookup provenance references an unmanaged version: ", version)
  }
  lkup
}

.cache_v2_path_identity <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }
  if (file.exists(path) || dir.exists(path)) {
    identity <- normalizePath(path, winslash = "/", mustWork = TRUE)
  } else if (dir.exists(dirname(path))) {
    identity <- file.path(
      normalizePath(dirname(path), winslash = "/", mustWork = TRUE),
      basename(path)
    )
  } else {
    identity <- normalizePath(path, winslash = "/", mustWork = FALSE)
  }
  identity <- fs::path_norm(identity)
  if (.Platform$OS.type == "windows") identity <- tolower(identity)
  identity
}

.cache_v2_same_file <- function(left, right) {
  if (file.exists(left) && file.exists(right)) {
    left_info <- fs::file_info(left)
    right_info <- fs::file_info(right)
    same_inode <- !is.na(left_info$inode) && !is.na(right_info$inode) &&
      identical(left_info$device_id, right_info$device_id) &&
      identical(left_info$inode, right_info$inode)
    if (same_inode) return(TRUE)
  }
  identical(.cache_v2_path_identity(left), .cache_v2_path_identity(right))
}

.cache_v2_managed_source_root <- function(path) {
  cfg <- .cache_v2_state$config
  if (is.null(cfg) || !is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(path)) return(FALSE)
  roots <- vapply(names(cfg$manifest$versions), function(version) {
    .cache_v2_version_root(cfg$manifest$data_root, version)
  }, character(1))
  normalized <- .cache_v2_path_identity(path)
  any(vapply(roots, function(root) {
    identical(normalized, .cache_v2_path_identity(root))
  }, logical(1)))
}

.cache_v2_managed_intermediate_path <- function(path) {
  cfg <- .cache_v2_state$config
  if (is.null(cfg) || !is.character(path) || length(path) != 1L ||
      is.na(path) || !nzchar(path)) return(FALSE)
  roots <- vapply(names(cfg$manifest$versions), function(version) {
    .cache_v2_version_root(cfg$manifest$data_root, version)
  }, character(1))
  expected <- file.path(roots, "cache.duckdb")
  any(vapply(expected, function(candidate) {
    .cache_v2_same_file(path, candidate)
  }, logical(1)))
}

cache_v2_cp_lookup <- function(lkup) {
  lkup$svy_lkup <- lkup$svy_lkup[lkup$svy_lkup$display_cp == 1, ]
  if (!is.null(lkup[["cache_v2", exact = TRUE]])) lkup$cache_v2$lookup_variant <- "cp"
  lkup
}

cache_v2_context <- function(lkup = NULL) {
  if (is.null(lkup)) return(getOption("pipapi.cache_v2_context"))
  cfg <- .cache_v2_state$config
  stamp <- lkup[["cache_v2", exact = TRUE]]
  if (is.null(cfg)) return(NULL)
  if (is.null(stamp)) {
    if (.cache_v2_managed_source_root(lkup$data_root)) {
      stop("Managed cache-v2 version requires provenance.")
    }
    return(NULL)
  }
  source <- cfg$manifest$versions[[stamp$version]]
  if (is.null(source)) stop("Lookup version is absent from cache v2 configuration.")
  if (!identical(stamp$build_fingerprint, cfg$build$fingerprint) ||
      !identical(stamp$dependency_fingerprint, source$fingerprint)) {
    stop("Lookup provenance does not match cache v2 configuration.")
  }
  if (is.character(lkup$data_root) && length(lkup$data_root) == 1L &&
      nzchar(lkup$data_root)) {
    expected <- .cache_v2_version_root(cfg$manifest$data_root, stamp$version)
    if (!identical(.cache_v2_path_identity(lkup$data_root),
                   .cache_v2_path_identity(expected))) {
      stop("Lookup data_root does not match its full data version.")
    }
  }
  c(list(root = cfg$root, intermediate_mode = cfg$intermediate_mode,
    source_root = lkup$data_root,
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
    formal <- if (compute) formal else list()
    scope <- if (compute) scope else list2env(list(), parent = emptyenv())
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
  if (d$kind == "response") {
    return(file.path(root, "v2", d$version, "response", paste0(identity$key, ".qs")))
  }
  file.path(root, "v2", d$version, d$kind, d$operation,
    substr(identity$key, 1L, 2L), paste0(identity$key, ".qs"))
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
  descriptor <- list(schema = 4L, operation = operation,
    kind = if (is.null(representation)) "compute" else "response",
    version = context$version, ppp_version = version_parts[2L],
    parameters = typed, lookup_variant = context$lookup_variant,
    # A runtime blocklist change must never serve an older cached response.
    blocked_aux_tables = sort(unique(blocked_aux_tables()), method = "radix"),
    dependency_fingerprint = context$dependency_fingerprint,
    # Responses also depend on lookup construction and canonical computation.
    build_fingerprint = context$build_fingerprint,
    endpoint_fingerprint = if (is.null(representation)) NULL else representation$endpoint_fingerprint,
    representation = representation)
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
      if (!is.raw(value)) stop("Response cache requires exact response bytes.")
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
    .cache_v2_guard(identity, inputs = TRUE, force_inputs = TRUE)
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
    supplied <- vapply(arg_names, function(name) {
      !isTRUE(eval(call("missing", as.name(name)), envir = frame))
    }, logical(1))
    args <- stats::setNames(lapply(arg_names[supplied], get, envir = frame, inherits = FALSE),
                            arg_names[supplied])
    if (!.cache_v2_compute_enabled()) return(do.call(original, args))
    lkup <- args$lkup
    context <- cache_v2_context(lkup)
    if (is.null(context)) return(do.call(original, args))
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
