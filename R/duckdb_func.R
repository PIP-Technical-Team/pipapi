#' Return the rows of the table if they exist in master file
#'
#' @inheritParams subset_lkup
#'
#' @return Dataframe
#' @export
return_if_exists <- function(slkup,
                             povline,
                             cache_file_path,
                             fill_gaps,
                             verbose = getOption("pipapi.verbose")) {

  if (fnrow(slkup) == 0 ) {
    return(list(data_present_in_master = NULL,
                lkup = slkup,
                povline = povline))
  }


  if (getOption("pipapi.query_live_data")) {
    return(list(data_present_in_master = NULL,
                lkup = slkup,
                povline = povline))
  }
  master_file <- load_inter_cache(cache_file_path = cache_file_path,
                                  fill_gaps = fill_gaps)

  if (fnrow(master_file) == 0) {
    return(list(data_present_in_master = NULL,
                lkup = slkup,
                povline = povline))
  }


  if (fill_gaps) {
    key_vars <- c("interpolation_id")
  } else {
    key_vars <- c("cache_id",
                  "reporting_level")
  }


  # This is probably unnecesary
  lkup_kvars <- slkup |>
    copy() |>
    funique() # this is not big.
  # get all vars
  slkup_vars <- setdiff(names(slkup), key_vars)
  # transform to NA when necessary
  lkup_kvars[is_interpolated == TRUE,
             (slkup_vars) := lapply(.SD, \(x) {
    if (fnunique(x) == 1) {
      x
    } else {
      NA
    }}),
    by = key_vars,
    .SDcols = slkup_vars]

  lkup_kvars <- unique(lkup_kvars, by = key_vars)




  # Find all (key_vars, poverty_line) combinations present in master_file
  key_vars_pl <- c(key_vars, "poverty_line")

  # Suppose lkup_kvars is a data.table and povline is a vector
  # lkup_kvars_pov <- lkup_kvars[, .(poverty_line = povline),
  #                              by = eval(names(lkup_kvars))]
  lkup_kvars_pov <- lkup_kvars[rep(seq_len(nrow(lkup_kvars)),
                                   each = length(povline))]
  lkup_kvars_pov[, poverty_line := rep(povline, times = nrow(lkup_kvars))]


  # Find which (key_vars, poverty_line) are present in master_file
  lk_not_ms <- join(x = lkup_kvars_pov,
                    y = master_file,
                    on = key_vars_pl,
                    how = "anti",
                    # validate = "1:1",
                    overid = 2,
                    verbose = 0,
                    multiple = TRUE)



  data_present_in_master <- join(x = lkup_kvars_pov,
                                 y = master_file,
                                 on = key_vars_pl,
                                 how = "inner",
                                 # validate = "1:1",
                                 overid = 2,
                                 verbose = 0,
                                 multiple = TRUE)


  # If no data is present in master
  if (fnrow(data_present_in_master) == 0) {
    return(list(data_present_in_master = NULL,
                lkup = slkup,
                povline = povline))
  }


  # There is nothing in lkup that is not present in master (i.e., all lkup in
  # master)
  if (fnrow(lk_not_ms) == 0) {
    if (verbose) message("Returning data from cache.")
    return(list(data_present_in_master = data_present_in_master,
                lkup = slkup[0],
                povline = povline))
  }


  # find out if all the key-vars in slkup are in data_present_in master, so if
  # that is the case, then we subset the poverty line
  present_master_kvars <-
    data_present_in_master[, ..key_vars] |>
    funique()

  # Find which key_vars in slkup are NOT present in master
  lkup_not_in_master <-
    join(lkup_kvars,
         present_master_kvars,
         how = "anti",
         overid = 2,
         verbose = 0)

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
    slkup <- join(slkup, lkup_not_in_master,
                  on = key_vars,
                  how = "semi",
                  overid = 2,
                  verbose = 0)
  }

  if (verbose) message("Returning data from cache.")

  return(list(data_present_in_master = data_present_in_master,
              lkup = slkup,
              povline = povline))
}

#' Update master file with the contents of the dataframe
#' @inheritParams pip
#' @param dat Dataframe to be appended
#' @param cache_file_path path where cache file is saved
#'
#' @return number of rows updated
#' @export
#'
update_master_file <- function(dat,
                               cache_file_path,
                               fill_gaps,
                               verbose = getOption("pipapi.verbose")
                               ) {

  write_con <- connect_with_retry(cache_file_path, read_only = FALSE)

  if (fill_gaps) {
    target_file <- "fg_master_file"
    unique_keys  <- c("interpolation_id", "poverty_line")
    keep_vars <- c(
      "interpolation_id",
      "poverty_line",
      "mean",
      "median",
      "headcount",
      "poverty_gap",
      "poverty_severity",
      "watts"
    )
  } else {
    target_file <- "rg_master_file"
    unique_keys <- c("cache_id",
                  "reporting_level",
                  "poverty_line")
    keep_vars <- c(
      "cache_id",
      "reporting_level",
      "poverty_line",
      "mean",
      "median",
      "headcount",
      "poverty_gap",
      "poverty_severity",
      "watts"
    )
  }

  # Select variables
  dat <- dat[, ..keep_vars]

  duckdb::duckdb_register(write_con, "append_data", dat, overwrite = TRUE)

  # Insert the rows that don't exist already in the master file
  nr <- DBI::dbExecute(write_con, glue("
  INSERT INTO {target_file}
  SELECT *
  FROM append_data AS a
  WHERE NOT EXISTS (
    SELECT 1
    FROM {target_file} AS t
    WHERE {glue_collapse(
          glue('t.{unique_keys} = a.{unique_keys}'), sep = ' AND ')}
     );
  "))

  duckdb::dbDisconnect(write_con)
  if (nr > 0 && verbose)  message(glue("{target_file} is updated."))

  return(nr)
}

connect_with_retry <- function(db_path = NULL,
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

    tryCatch({
      con <- duckdb::duckdb(dbdir = db_path, read_only = read_only) |>
        duckdb::dbConnect()
      if (verbose) message("Connected on attempt ", attempt)
      return(con)
    },
    error = function(e) {
      if (verbose) {
        message("Attempt ", attempt,
               " failed: ", conditionMessage(e))
      }
      if (attempt == max_attempts) {
        stop("Failed to connect after ", max_attempts, " attempts.")
      }
      Sys.sleep(delay_sec)
      attempt <<- attempt + 1
    })

  }
}


#' Reset the cache. Only to be used internally
#'
#' @noRd
reset_cache <- function(pass = Sys.getenv('PIP_CACHE_LOCAL_KEY'),
                        type = c("both", "rg", "fg"),
                        lkup) {
  # lkup will be passed through API and will not be an argument to endpoint,
  # same as pip call Checks if the keys match across local and server before
  # reseting the cache
  if (pass != Sys.getenv('PIP_CACHE_SERVER_KEY')) {
    rlang::abort("Either key not set or incorrect key!")
  }

  cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")
  write_con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_file_path)

  type <- match.arg(type)
  if(type == "both") type = c("rg", "fg")
  if("rg" %in% type) {
    DBI::dbExecute(write_con, "DELETE from rg_master_file")
  }
  if("fg" %in% type) {
    DBI::dbExecute(write_con, "DELETE from fg_master_file")
  }
  duckdb::dbDisconnect(write_con)
}

create_duckdb_file <- function(cache_file_path) {
  con <- connect_with_retry(cache_file_path, read_only = FALSE)
  DBI::dbExecute(con, "CREATE OR REPLACE table rg_master_file (
                 cache_id VARCHAR,
                 reporting_level   VARCHAR,
                 poverty_line   DOUBLE,
                 mean   DOUBLE,
                 median DOUBLE,
                 headcount    DOUBLE,
                 poverty_gap   DOUBLE,
                 poverty_severity  DOUBLE,
                 watts     DOUBLE)"
                 )

  DBI::dbExecute(con, "CREATE OR REPLACE table fg_master_file (
                 interpolation_id VARCHAR,
                 poverty_line   DOUBLE,
                 mean   DOUBLE,
                 median DOUBLE,
                 headcount    DOUBLE,
                 poverty_gap   DOUBLE,
                 poverty_severity  DOUBLE,
                 watts     DOUBLE
  )")
  DBI::dbDisconnect(con)
}




#' Load Intermediate cache data
#'
#' @inheritParams return_if_exists
#'
#' @return data frame
#' @export
load_inter_cache <- function(lkup = NULL,
                             cache_file_path = NULL,
                             fill_gaps = FALSE) {

  target_file <- if (fill_gaps) {
    "fg_master_file"
  } else {
    "rg_master_file"
  }

  if (!is.null(lkup)) {
    cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")
  }
  con <- connect_with_retry(cache_file_path)

  master_file <- DBI::dbGetQuery(con,
                                 glue("select * from {target_file}"))

  # It is important to close the read connection before you open a write
  # connection because duckdb kind of inherits read_only flag from previous
  # connection object if it is not closed More details here
  # https://app.clickup.com/t/868cdpe3q
  duckdb::dbDisconnect(con)

  setDT(master_file)
}

