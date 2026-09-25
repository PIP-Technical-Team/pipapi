#' Return the rows of the table if they exist in master file
#'
#' @inheritParams subset_lkup
#'
#' @return list with 3 elements data_present_in_master, modified `lkup` value and `povline`
#' @export
return_if_exists <- function(
  slkup,
  povline,
  cache_file_path,
  fill_gaps,
  verbose = getOption("pipapi.verbose")
) {
  # none selected
  if (fnrow(slkup) == 0) {
    return(list(data_present_in_master = NULL, lkup = slkup, povline = povline))
  }

  # Explicit live-data requests bypass pipapi's intermediate cache.
  if (isTRUE(getOption("pipapi.query_live_data"))) {
    return(list(data_present_in_master = NULL, lkup = slkup, povline = povline))
  }

  context <- intermediate_cache_context(cache_file_path)
  master_file <- tryCatch(
    load_inter_cache(cache_file_path = cache_file_path, fill_gaps = fill_gaps,
                     poverty_lines = povline),
    error = function(e) {
      if (!is.null(context) && identical(context$intermediate_mode, "read_only")) stop(e)
      cli::cli_warn("Failed to load intermediate cache: {e$message}")
      slkup[0]
    }
  )
  # if no cached files, return selected lkup
  if (fnrow(master_file) == 0) {
    return(list(data_present_in_master = NULL, lkup = slkup, povline = povline))
  }

  source_lkup <- data.table::copy(slkup)
  if (fill_gaps) {
    key_vars <- c("interpolation_id")
    # convert survey_comparability to NA
    # NOTE: This should not be necessary. for the new lineup distribution
    # metadata should come without this variable.

    # ZP comment: if using refy_lkup, this should be removed because
    #             it does not include survey_comparability
    slkup[, survey_comparability := NA_real_]
  } else {
    key_vars <- c("cache_id", "reporting_level")
  }

  # This is probably unnecesary
  # ZP comment: in my quick checks this has no impact, meaning
  #             slkup is already unique
  # ZP Question: is this to get rid of duplicates from df_refy???
  lkup_kvars <- slkup |>
    copy() |>
    funique() # this is not big.

  # get all vars
  slkup_vars <- setdiff(names(slkup), key_vars)
  # transform to NA when necessary
  lkup_kvars[
    is_interpolated == TRUE,
    (slkup_vars) := lapply(.SD, \(x) {
      if (fnunique(x) == 1) {
        x
      } else {
        NA
      }
    }),
    by = key_vars,
    .SDcols = slkup_vars
  ]

  lkup_kvars <- unique(lkup_kvars, by = key_vars)

  # Find all (key_vars, poverty_line) combinations present in master_file
  key_vars_pl <- c(key_vars, "poverty_line")

  # Suppose lkup_kvars is a data.table and povline is a vector
  # lkup_kvars_pov <- lkup_kvars[, .(poverty_line = povline),
  #                              by = eval(names(lkup_kvars))]
  lkup_kvars_pov <- lkup_kvars[rep(
    seq_len(nrow(lkup_kvars)),
    each = length(povline)
  )] # ZP: add povline
  lkup_kvars_pov[, poverty_line := rep(povline, times = nrow(lkup_kvars))]

  # Find which (key_vars, poverty_line) are present in master_file
  lk_not_ms <- join(
    x = lkup_kvars_pov,
    y = master_file, # ZP: remember, master_file is full cache file
    on = key_vars_pl,
    how = "anti", # rows in lkup not in master_file to know what new to do
    # validate = "1:1",
    overid = 2,
    verbose = 0,
    multiple = TRUE
  )

  # Mixing incomplete cached pairs with freshly calculated rows can omit or
  # duplicate results. An incomplete request is calculated wholly from source.
  if (fnrow(lk_not_ms) > 0) {
    return(list(data_present_in_master = NULL, lkup = source_lkup, povline = povline))
  }
  data_present_in_master <- join(
    x = lkup_kvars_pov, y = master_file, on = key_vars_pl,
    how = "inner", overid = 2, verbose = 0, multiple = TRUE
  )
  if (fnrow(data_present_in_master) == 0) {
    return(list(data_present_in_master = NULL, lkup = source_lkup, povline = povline))
  }
  if (verbose) message("Returning data from cache.")
  list(data_present_in_master = data_present_in_master, lkup = slkup[0], povline = povline)
}

#' Update master file with the contents of the dataframe
#' @inheritParams pip
#' @param dat Dataframe to be appended
#' @param cache_file_path path where cache file is saved
#'
#' @return a number i.e no. of rows updated
#' @export
#'
update_master_file <- function(
  dat,
  cache_file_path,
  fill_gaps,
  verbose = getOption("pipapi.verbose"),
  decimal = 2
) {
  if (isTRUE(getOption("pipapi.query_live_data"))) return(invisible(FALSE))
  context <- intermediate_cache_context(cache_file_path)
  if (!is.null(context) && context$intermediate_mode == "read_only") {
    return(invisible(FALSE))
  }
  if (nrow(dat) == 0L) return(invisible(FALSE))
  # select the right lines
  pl <- get_from_pipapienv("pl_to_store")

  # Keep only rows with <= 2 decimal places
  to_keep <- get_vars(dat, "poverty_line") |>
    reg_elem() |> # extract vectos
    as.character() |>
    sub("^[^.]*\\.?", "", x = _) |> # get only the decimal part
    (\(x) which(nchar(x) <= decimal))()

  dat <- dat[to_keep]

  povline <- dat[, poverty_line] |>
    unique()

  # Keep only those that belong to the list
  wpl <- povline[povline %in% round(pl, decimal)]

  if (length(wpl) == 0) {
    return(invisible(FALSE))
  }

  dat <- dat[poverty_line %in% wpl]

  if (nrow(dat) == 0) {
    return(invisible(FALSE))
  }

  if (fill_gaps) {
    target_file <- "fg_master_file"
    unique_keys <- c("interpolation_id", "poverty_line")
    keep_vars <- c(
      "interpolation_id",
      "poverty_line",
      "headcount",
      "poverty_gap",
      "poverty_severity",
      "watts"
    )
  } else {
    target_file <- "rg_master_file"
    unique_keys <- c("cache_id", "reporting_level", "poverty_line")
    keep_vars <- c(
      "cache_id",
      "reporting_level",
      "poverty_line",
      "headcount",
      "poverty_gap",
      "poverty_severity",
      "watts"
    )
  }

  nr <- with_intermediate_db(cache_file_path, write = TRUE, function(write_con) {
    DBI::dbWithTransaction(write_con, {
      intermediate_cache_schema(write_con, context)
      col_names <- DBI::dbListFields(write_con, target_file)
      if (all(c("mean", "median") %in% col_names)) {
        keep_vars <- c(keep_vars, "mean", "median")
      }
      append_data <- data.table::copy(dat[, ..keep_vars])
      append_data <- unique(append_data, by = unique_keys)
      if (anyNA(append_data[, ..unique_keys])) {
        stop("Intermediate cache row keys must not be missing.")
      }
      duckdb::duckdb_register(write_con, "append_data", append_data)
      columns <- paste(names(append_data), collapse = ", ")
      matches <- paste(paste0("t.", unique_keys, " = a.", unique_keys), collapse = " AND ")
      inserted <- DBI::dbExecute(write_con, paste0(
        "INSERT INTO ", target_file, " (", columns, ") SELECT ", columns,
        " FROM append_data AS a WHERE NOT EXISTS (SELECT 1 FROM ",
        target_file, " AS t WHERE ", matches, ")"
      ))
      if (!is.null(context)) .cache_v2_guard(list(descriptor = context), inputs = TRUE)
      inserted
    })
  })

  if (!is.null(context) && nr > 0) {
    .cache_v2_count("intermediate_write", if (fill_gaps) "fg" else "rg")
  }
  if (nr > 0 && verbose) {
    message(glue("{target_file} is updated."))
  }

  return(nr)
}

connect_with_retry <- function(
  db_path = NULL,
  max_attempts = 5,
  delay_sec = 1,
  read_only = TRUE,
  lkup = NULL,
  verbose = getOption("pipapi.verbose")
) {
  if (!is.null(lkup)) {
    db_path <- intermediate_cache_path(lkup)
  }
  if (identical(Sys.getenv("PIPAPI_CACHE_V2"), "TRUE") &&
      !is.null(intermediate_cache_context(db_path))) {
    stop("Raw DuckDB connections are not allowed for managed cache v2; use scoped access.")
  }

  attempt <- 1
  while (attempt <= max_attempts) {
    drv <- NULL
    con <- NULL

    tryCatch(
      {
        drv <- duckdb::duckdb(dbdir = db_path, read_only = read_only)
        con <- duckdb::dbConnect(drv)
        if (verbose) {
          message("Connected on attempt ", attempt)
        }
        return(con)
      },
      error = function(e) {
        if (!is.null(con) && DBI::dbIsValid(con)) {
          try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
        } else if (!is.null(drv)) {
          suppressWarnings(
            try(duckdb::duckdb_shutdown(drv), silent = TRUE)
          )
        }
        gc()

        if (verbose) {
          message("Attempt ", attempt, " failed: ", conditionMessage(e))
        }
        # if (attempt == max_attempts) {
        #   stop("Failed to connect after ", max_attempts, " attempts.")
        # }
        if (attempt == max_attempts) {
          stop(
            "Failed to connect after ",
            max_attempts,
            " attempts.\nLast error: ",
            conditionMessage(e)
          )
        }
        Sys.sleep(delay_sec)
        attempt <<- attempt + 1
      }
    )
  }
}


# Internal helper: validate cache auth before destructive operations.
# Aborts if either env var is unset/empty or if `pass` != server key.
.check_cache_auth <- function(pass) {
  server_key <- Sys.getenv("PIP_CACHE_SERVER_KEY", unset = "")
  if (
    !nzchar(Sys.getenv("PIP_CACHE_LOCAL_KEY", unset = "")) ||
      !nzchar(server_key)
  ) {
    cli::cli_abort(
      "Cache key env var(s) not set \\
      ({.envvar PIP_CACHE_LOCAL_KEY} / {.envvar PIP_CACHE_SERVER_KEY})."
    )
  }
  if (!is.character(pass) || length(pass) != 1L || is.na(pass) ||
      !identical(pass, server_key)) {
    cli::cli_abort(
      "Cache key mismatch: supplied key does not match server key."
    )
  }
  invisible(TRUE)
}


#' Reset the cache. Only to be used internally
#'
#' @noRd
reset_cache <- function(
  pass = Sys.getenv("PIP_CACHE_LOCAL_KEY"),
  type = c("both", "rg", "fg"),
  lkup
) {
  # lkup will be passed through API and will not be an argument to endpoint,
  # same as pip call Checks if the keys match across local and server before
  # reseting the cache
  .check_cache_auth(pass)

  cache_file_path <- intermediate_cache_path(lkup)
  context <- intermediate_cache_context(cache_file_path)
  if (!is.null(context) && context$intermediate_mode == "read_only") {
    stop("Cannot reset a read-only intermediate cache.")
  }
  type <- match.arg(type)
  if (type == "both") {
    type <- c("rg", "fg")
  }
  with_intermediate_db(cache_file_path, write = TRUE, function(con) {
    DBI::dbWithTransaction(con, {
      for (kind in type) {
        table <- paste0(kind, "_master_file")
        if (DBI::dbExistsTable(con, table)) {
          DBI::dbExecute(con, paste("DELETE FROM", table))
        }
      }
    })
  })
}

#' Delete the DuckDB cache file. Only to be used internally
#'
#' @details
#' All DuckDB connections to the target cache file must be closed/disconnected
#' before calling this function. Calling with an open connection will result in
#' a locked-file error on Windows.
#'
#' @noRd
delete_cache <- function(
  pass = Sys.getenv("PIP_CACHE_LOCAL_KEY"),
  lkup
) {
  .check_cache_auth(pass)

  if (is.null(lkup$data_root) || !nzchar(lkup$data_root)) {
    cli::cli_abort("{.arg lkup$data_root} must be a non-empty string.")
  }

  if (isTRUE(getOption("pipapi.query_live_data"))) return(invisible(character()))
  cache_file_path <- intermediate_cache_path(lkup)
  context <- intermediate_cache_context(cache_file_path)
  if (!is.null(context) && context$intermediate_mode == "read_only") {
    stop("Cannot delete a read-only intermediate cache.")
  }
  if (!is.null(context)) {
    if (!dir.exists(dirname(cache_file_path))) return(invisible(character()))
    lock <- intermediate_cache_lock(cache_file_path)
    on.exit(filelock::unlock(lock), add = TRUE)
  }
  cache_sidecars <- c(
    cache_file_path,
    paste0(cache_file_path, ".wal")
  )

  cache_sidecars <- cache_sidecars[fs::file_exists(cache_sidecars)]

  if (length(cache_sidecars) == 0) {
    return(invisible(character()))
  }

  fs::file_delete(cache_sidecars)

  invisible(cache_sidecars)
}

create_duckdb_file <- function(cache_file_path) {
  if (isTRUE(getOption("pipapi.query_live_data"))) return(invisible(FALSE))
  context <- intermediate_cache_context(cache_file_path)
  with_intermediate_db(cache_file_path, write = TRUE, function(con) {
    DBI::dbWithTransaction(con, intermediate_cache_schema(con, context))
  })
}

safe_update_master_file <- function(dat, cache_file_path, fill_gaps) {
  if (isTRUE(getOption("pipapi.query_live_data"))) return(invisible(FALSE))
  context <- intermediate_cache_context(cache_file_path)
  tryCatch(
    update_master_file(dat, cache_file_path, fill_gaps),
    error = function(e) {
      if (!is.null(context)) stop(e)
      cli::cli_warn("Failed to update intermediate cache: {e$message}")
      invisible(FALSE)
    }
  )
}

#' Load Intermediate cache data
#'
#' @inheritParams return_if_exists
#' @param poverty_lines Optional poverty lines to read. `NULL` reads all rows.
#'
#' @return cached data frame
#' @export
load_inter_cache <- function(
  lkup = NULL,
  cache_file_path = NULL,
  fill_gaps = FALSE,
  poverty_lines = NULL
) {
  if (isTRUE(getOption("pipapi.query_live_data"))) return(data.table::data.table())
  target_file <- if (fill_gaps) {
    "fg_master_file"
  } else {
    "rg_master_file"
  }

  if (!is.null(lkup)) {
    cache_file_path <- intermediate_cache_path(lkup)
  }
  context <- intermediate_cache_context(cache_file_path)
  with_intermediate_db(cache_file_path, write = FALSE, function(con) {
    if (!DBI::dbExistsTable(con, target_file)) {
      if (!is.null(context) && identical(context$intermediate_mode, "read_only")) {
        stop("Managed intermediate DuckDB is missing required table: ", target_file)
      }
      return(data.table::data.table())
    }
    if (is.null(poverty_lines)) {
      master_file <- DBI::dbGetQuery(con, paste("SELECT * FROM", target_file))
    } else {
      poverty_lines <- unique(round(as.numeric(poverty_lines), 2))
      if (!length(poverty_lines) || any(!is.finite(poverty_lines))) {
        stop("poverty_lines must contain finite values.")
      }
      placeholders <- paste(rep("?", length(poverty_lines)), collapse = ", ")
      master_file <- DBI::dbGetQuery(
        con,
        paste("SELECT * FROM", target_file, "WHERE poverty_line IN (", placeholders, ")"),
        params = as.list(poverty_lines)
      )
    }
    if (!is.null(context)) {
      .cache_v2_guard(list(descriptor = context), inputs = TRUE)
      if (nrow(master_file)) .cache_v2_count("intermediate_hit", if (fill_gaps) "fg" else "rg")
    }
    data.table::as.data.table(master_file)
  }, missing = data.table::data.table())
}

# Paths carry the request identity to lower helpers, including direct R calls.
intermediate_cache_path <- function(lkup, ppp = NULL, popshare = NULL) {
  if (isTRUE(getOption("pipapi.query_live_data"))) return(NULL)
  if (!identical(Sys.getenv("PIPAPI_CACHE_V2"), "TRUE")) {
    return(fs::path(lkup$data_root, "cache", ext = "duckdb"))
  }
  if (!cache_v2_enabled()) stop("Intermediate cache v2 requires caching to be enabled.")
  context <- cache_v2_context(lkup)
  if (is.null(context)) {
    return(fs::path(lkup$data_root, "cache", ext = "duckdb"))
  }
  current <- cache_v2_context()
  context$parameters <- current$parameters
  if (missing(ppp)) ppp <- current$parameters$ppp
  if (missing(popshare)) popshare <- current$parameters$popshare
  context$custom <- list(ppp = ppp, popshare = popshare)
  context$lookup_variant <- lkup$cache_v2$lookup_variant
  path <- file.path(lkup$data_root, "cache.duckdb")
  attr(path, "cache_v2_context") <- context
  intermediate_cache_context(path)
  path
}

intermediate_cache_context <- function(path) {
  if (!identical(Sys.getenv("PIPAPI_CACHE_V2"), "TRUE")) return(NULL)
  if (!cache_v2_enabled()) stop("Intermediate cache v2 requires caching to be enabled.")
  context <- attr(path, "cache_v2_context", exact = TRUE)
  current <- cache_v2_context()
  if (is.null(context)) {
    context <- current
    if (!is.null(context)) {
      context$custom <- list(ppp = context$parameters$ppp, popshare = context$parameters$popshare)
    }
  }
  if (is.null(context)) {
    if (.cache_v2_managed_intermediate_path(path)) {
      stop("Managed cache-v2 intermediate path requires provenance.")
    }
    return(NULL)
  }
  source_root <- if (is.null(context)) NULL else context$source_root
  if (is.null(source_root) && !is.null(current)) source_root <- current$source_root
  if (!is.character(source_root) || length(source_root) != 1L ||
      is.na(source_root) || !nzchar(source_root)) {
    stop("A source version path is required for the intermediate cache.")
  }
  context$source_root <- source_root
  required <- c("root", "source_root", "version", "dependency_fingerprint", "build_fingerprint", "revision")
  if (is.null(context) || any(!vapply(context[required], function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }, logical(1)))) stop("Valid provenance is required for intermediate cache v2.")
  if (length(context$intermediate_mode) != 1L ||
      !context$intermediate_mode %in% c("write", "read_only")) {
    stop("Invalid intermediate cache mode.")
  }
  configured <- cache_v2_context(list(data_root = source_root, cache_v2 = context[c(
    "version", "dependency_fingerprint", "build_fingerprint", "lookup_variant"
  )]))
  if (is.null(configured)) stop("Valid configured provenance is required for intermediate cache v2.")
  for (field in c(required, "intermediate_mode")) {
    if (!identical(context[[field]], configured[[field]])) {
      stop("Intermediate cache context does not match the configured provenance.")
    }
  }
  if (!is.null(current)) {
    for (field in c(required, "intermediate_mode")) {
      if (!identical(context[[field]], current[[field]])) {
        stop("Intermediate cache context does not match the active provenance.")
      }
    }
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", context$version)) {
    stop("Invalid full data version for intermediate cache.")
  }
  expected <- file.path(source_root, "cache.duckdb")
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !identical(fs::path_norm(as.character(path)), fs::path_norm(expected))) {
    stop("Intermediate cache v2 requires the cache.duckdb file in its source version directory.")
  }
  context
}

intermediate_cache_identity <- function(context) {
  descriptor <- list(
    schema = "duckdb-2", version = context$version,
    dependency_fingerprint = context$dependency_fingerprint,
    build_fingerprint = context$build_fingerprint, revision = context$revision,
    parameters = context$parameters, lookup_variant = context$lookup_variant,
    custom = context$custom
  )
  canonical <- .cache_v2_json(descriptor)
  list(key = .cache_v2_sha(canonical), canonical = canonical)
}

intermediate_cache_lock <- function(path) {
  timeout <- getOption("pipapi.cache_v2_lock_timeout", 10000)
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout < 0) {
    stop("Intermediate cache lock timeout must be finite and non-negative.")
  }
  lock <- filelock::lock(paste0(path, ".lock"), timeout = timeout)
  if (is.null(lock)) stop("Timed out waiting for the intermediate cache lock.")
  lock
}

with_intermediate_db <- function(path, write, code, missing = invisible(FALSE)) {
  if (isTRUE(getOption("pipapi.query_live_data"))) return(missing)
  if (!write && (!file.exists(path) || !dir.exists(dirname(path)))) return(missing)
  context <- intermediate_cache_context(path)
  if (!is.null(context) && !write && !file.exists(path)) return(missing)
  if (!is.null(context) && write && context$intermediate_mode == "read_only") return(missing)
  if (!is.null(context)) .cache_v2_guard(list(descriptor = context), inputs = TRUE)
  if (!write && !dir.exists(dirname(path))) return(missing)
  if (write && !dir.exists(dirname(path))) {
    if (!dir.create(dirname(path), recursive = TRUE) && !dir.exists(dirname(path))) {
      stop("Cannot create the intermediate cache directory.")
    }
  }
  lock <- NULL
  if (!is.null(context) && write) {
    lock_name <- paste0(path, ".lock")
    # Bootstrap owns the bounded write lock for the complete version lifecycle.
    if (!exists(lock_name, .cache_v2_state$bootstrap_locks, inherits = FALSE)) {
      lock <- intermediate_cache_lock(path)
      on.exit(filelock::unlock(lock), add = TRUE)
    }
  }
  if (!write && !file.exists(path)) return(missing)
  drv <- NULL
  con <- NULL
  on.exit({
    tryCatch({
      if (!is.null(con)) DBI::dbDisconnect(con, shutdown = is.null(context))
    }, finally = {
      if (!is.null(drv)) duckdb::duckdb_shutdown(drv)
    })
  }, add = TRUE, after = FALSE)
  if (is.null(context)) {
    con <- connect_with_retry(path, read_only = !write)
  } else {
    drv <- duckdb::duckdb(dbdir = as.character(path), read_only = !isTRUE(write))
    con <- DBI::dbConnect(drv)
  }
  code(con)
}

intermediate_cache_schema <- function(con, context) {
  for (kind in c("rg", "fg")) {
    keys <- if (kind == "rg") c("cache_id", "reporting_level") else "interpolation_id"
    columns <- paste(paste(keys, "VARCHAR"), collapse = ", ")
    constraints <- paste0(", UNIQUE (", paste(c(keys, "poverty_line"), collapse = ", "), ")")
    table <- paste0(kind, "_master_file")
    DBI::dbExecute(con, paste0(
      "CREATE TABLE IF NOT EXISTS ", table, " (", columns,
      ", poverty_line DOUBLE, headcount DOUBLE, poverty_gap DOUBLE,",
      " poverty_severity DOUBLE, watts DOUBLE", constraints, ")"
    ))
  }
  invisible(TRUE)
}
