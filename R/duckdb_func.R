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

  # don't use cache
  if (getOption("pipapi.query_live_data")) {
    return(list(data_present_in_master = NULL, lkup = slkup, povline = povline))
  }

  # load cache
  # ZP new temp code to avoid error from load_inter_cache due to dbConnect
  master_file <- tryCatch(
    load_inter_cache(cache_file_path = cache_file_path, fill_gaps = fill_gaps),
    error = function(e) {
      cli::cli_warn("Failed to load intermediate cache: {e$message}")
      master_file <- slkup[0] # zero-row data.table with same columns as lkup
    }
  )
  # temp ZP just to bypass cache
  #master_file <- slkup[0]

  # ZP old code:
  # master_file <- load_inter_cache(cache_file_path = cache_file_path,
  #                                 fill_gaps = fill_gaps)

  # if no cached files, return selected lkup
  if (fnrow(master_file) == 0) {
    return(list(data_present_in_master = NULL, lkup = slkup, povline = povline))
  }

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

  data_present_in_master <- join(
    x = lkup_kvars_pov,
    y = master_file,
    on = key_vars_pl,
    how = "inner",
    # validate = "1:1",
    overid = 2,
    verbose = 0,
    multiple = TRUE
  )

  # now we have two dfs: lk_not_ms and data_present_in_master
  #    which gives the lkup rows not in cache (master_file),
  #    and the lkup rows in cache (master_file)

  # If no data is present in master
  #  i.e. if no common rows between
  if (fnrow(data_present_in_master) == 0) {
    return(list(data_present_in_master = NULL, lkup = slkup, povline = povline))
  }

  # There is nothing in lkup that is not present in master (i.e., all lkup in
  # master)
  if (fnrow(lk_not_ms) == 0) {
    if (verbose) {
      message("Returning data from cache.")
    }
    return(list(
      data_present_in_master = data_present_in_master,
      lkup = slkup[0],
      povline = povline
    ))
  }

  # find out if all the key-vars in slkup are in data_present_in master, so if
  # that is the case, then we subset the poverty line
  present_master_kvars <-
    data_present_in_master[, ..key_vars] |>
    funique()

  # Find which key_vars in slkup are NOT present in master
  lkup_not_in_master <-
    join(
      lkup_kvars,
      present_master_kvars,
      how = "anti",
      overid = 2,
      verbose = 0
    )

  all_in_master <- fnrow(lkup_not_in_master) == 0

  # Update povline if all key_vars in slkup are present in master_file
  if (all_in_master) {
    # For each key_vars, keep only povlines not present in master_file
    # NOTE: here the povline changes

    povline <- funique(lk_not_ms[, poverty_line])
    # povline_in_master <- funique(data_present_in_master[, poverty_line])
    # povline <- setdiff(povline, povline_in_master)

    if (length(povline) == 0) {
      stop("at this stage, povline must be 1 or greater")
    }
  } else {
    # lkup: keep only key_vars not present in master_file
    # NOTE: here the slkup changes
    slkup <- join(
      slkup,
      lkup_not_in_master,
      on = key_vars,
      how = "semi",
      overid = 2,
      verbose = 0
    )
  }

  if (verbose) {
    message("Returning data from cache.")
  }

  return(list(
    data_present_in_master = data_present_in_master,
    lkup = slkup,
    povline = povline
  ))
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

  write_con <- connect_with_retry(cache_file_path, read_only = FALSE)

  # Create schema if this is a fresh / uninitialized cache file
  if (
    !DBI::dbExistsTable(write_con, "rg_master_file") ||
      !DBI::dbExistsTable(write_con, "fg_master_file")
  ) {
    DBI::dbDisconnect(write_con, shutdown = TRUE)
    create_duckdb_file(cache_file_path)
    write_con <- connect_with_retry(cache_file_path, read_only = FALSE)
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

  # Get column names from DuckDB table
  table_info <- DBI::dbGetQuery(
    write_con,
    glue("PRAGMA table_info({target_file})")
  )
  col_names <- table_info$name
  # Add mean and median if present in table
  if (all(c("mean", "median") %in% col_names)) {
    keep_vars <- c(keep_vars, "mean", "median")
  }

  # Select variables
  dat <- dat[, ..keep_vars]

  duckdb::duckdb_register(write_con, "append_data", dat, overwrite = TRUE)

  # Insert the rows that don't exist already in the master file
  nr <- DBI::dbExecute(
    write_con,
    glue(
      "
  INSERT INTO {target_file}
  SELECT *
  FROM append_data AS a
  WHERE NOT EXISTS (
    SELECT 1
    FROM {target_file} AS t
    WHERE {glue_collapse(
          glue('t.{unique_keys} = a.{unique_keys}'), sep = ' AND ')}
     );
  "
    )
  )

  DBI::dbDisconnect(write_con, shutdown = TRUE)

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
    db_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")
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
  if (pass != server_key) {
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

  cache_file_path <- fs::path(lkup$data_root, "cache", ext = "duckdb")
  write_con <- connect_with_retry(cache_file_path, read_only = FALSE)

  type <- match.arg(type)
  if (type == "both") {
    type <- c("rg", "fg")
  }
  if ("rg" %in% type && DBI::dbExistsTable(write_con, "rg_master_file")) {
    DBI::dbExecute(write_con, "DELETE from rg_master_file")
  }
  if ("fg" %in% type && DBI::dbExistsTable(write_con, "fg_master_file")) {
    DBI::dbExecute(write_con, "DELETE from fg_master_file")
  }
  DBI::dbDisconnect(write_con, shutdown = TRUE)
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

  cache_file_path <- fs::path(lkup$data_root, "cache", ext = "duckdb")
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
  con <- connect_with_retry(cache_file_path, read_only = FALSE)
  DBI::dbExecute(
    con,
    "CREATE OR REPLACE table rg_master_file (
                 cache_id VARCHAR,
                 reporting_level   VARCHAR,
                 poverty_line   DOUBLE,

                 headcount    DOUBLE,
                 poverty_gap   DOUBLE,
                 poverty_severity  DOUBLE,
                 watts     DOUBLE)"
  )

  DBI::dbExecute(
    con,
    "CREATE OR REPLACE table fg_master_file (
                 interpolation_id VARCHAR,
                 poverty_line   DOUBLE,

                 headcount    DOUBLE,
                 poverty_gap   DOUBLE,
                 poverty_severity  DOUBLE,
                 watts     DOUBLE
  )"
  )
  DBI::dbDisconnect(con, shutdown = TRUE)
}

safe_update_master_file <- function(dat, cache_file_path, fill_gaps) {
  tryCatch(
    update_master_file(dat, cache_file_path, fill_gaps),
    error = function(e) {
      cli::cli_warn("Failed to update intermediate cache: {e$message}")
      invisible(FALSE)
    }
  )
}

#' Load Intermediate cache data
#'
#' @inheritParams return_if_exists
#'
#' @return cached data frame
#' @export
load_inter_cache <- function(
  lkup = NULL,
  cache_file_path = NULL,
  fill_gaps = FALSE
) {
  target_file <- if (fill_gaps) {
    "fg_master_file"
  } else {
    "rg_master_file"
  }

  if (!is.null(lkup)) {
    cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")
  }
  con <- connect_with_retry(cache_file_path)

  if (!DBI::dbExistsTable(con, target_file)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
    return(data.table::data.table())
  }

  master_file <- DBI::dbGetQuery(con, glue("select * from {target_file}"))

  # It is important to close the read connection before you open a write
  # connection because duckdb kind of inherits read_only flag from previous
  # connection object if it is not closed More details here
  # https://app.clickup.com/t/868cdpe3q
  DBI::dbDisconnect(con, shutdown = TRUE)
  setDT(master_file)
}
