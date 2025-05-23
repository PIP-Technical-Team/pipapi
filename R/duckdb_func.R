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


  # This is probably unnecessary
  lkup_kvars <- funique(slkup) # this is not big.


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

  # write_con <- connect_with_retry(cache_file_path, read_only = FALSE)

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

  master_file <- arrow::read_parquet(cache_file_path, mmap = FALSE)
  out <- collapse::rowbind(master_file, dat)
  arrow::write_parquet(out, cache_file_path)
  invisible(gc())
  return(NROW(dat))
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

create_duckdb_file <- function(file_path, fill_gaps) {

  if (fill_gaps) {
    dat2 <- structure(list(interpolation_id = character(0), poverty_line = numeric(0),
                           mean = numeric(0), median = numeric(0), headcount = numeric(0),
                           poverty_gap = numeric(0), poverty_severity = numeric(0),
                           watts = numeric(0)), row.names = c(NA, 0L), class = "data.frame")

    arrow::write_parquet(dat2, fs::path(file_path, "fg_master_file", ext = "parquet"))
  } else {

    dat1 <- structure(list(cache_id = character(0), reporting_level = character(0),
                           poverty_line = numeric(0), mean = numeric(0), median = numeric(0),
                           headcount = numeric(0), poverty_gap = numeric(0), poverty_severity = numeric(0),
                           watts = numeric(0)), row.names = c(NA, 0L), class = "data.frame")

    arrow::write_parquet(dat1, fs::path(file_path, "rg_master_file", ext = "parquet"))
  }


  invisible(gc())
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
    cache_file_path <- fs::path(lkup$data_root, target_file, ext = "parquet")
  }
  # con <- connect_with_retry(cache_file_path)

  master_file <- arrow::read_parquet(cache_file_path, mmap = FALSE)

  # It is important to close the read connection before you open a write
  # connection because duckdb kind of inherits read_only flag from previous
  # connection object if it is not closed More details here
  # https://app.clickup.com/t/868cdpe3q
  #duckdb::dbDisconnect(con)

  setDT(master_file)
}

