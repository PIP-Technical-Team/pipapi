#' Return the rows of the table if they exist in master file
#'
#' @inheritParams subset_lkup
#' @param con Connection object to duckdb table
#'
#' @return Dataframe
#' @export
#'
return_if_exists <- function(lkup, povline, con, fill_gaps) {
  # It is not possible to append to parquet file https://stackoverflow.com/questions/39234391/how-to-append-data-to-an-existing-parquet-file
  # Writing entire data will be very costly as data keeps on growing, better is to save data in duckdb and append to it.
  target_file <- if (fill_gaps) "fg_master_file" else "rg_master_file"
  master_file <- DBI::dbGetQuery(con, glue::glue("select * from {target_file}")) |>
    duckplyr::as_duckplyr_tibble()

  data_present_in_master <- duckplyr::inner_join(
    master_file, lkup |> collapse::fselect(country_code, reporting_year, is_interpolated),
    by = c("country_code", "reporting_year", "is_interpolated")
  ) |> duckplyr::filter(poverty_line == povline)

  keep <- TRUE
  if(nrow(data_present_in_master) > 0) {
    keep <- !with(lkup, paste(country_code, reporting_year, is_interpolated)) %in%
      with(data_present_in_master, paste(country_code, reporting_year, is_interpolated))

    lkup <- lkup[keep, ]

    message("Returning data from cache.")
  }
  # nrow(data_present_in_master) should be equal to sum(keep)
  return(list(data_present_in_master = data_present_in_master, lkup = lkup))
}

#' Update master file with the contents of the dataframe
#' @inheritParams pip
#' @param dat Dataframe to be appended
#' @param cache_file_path path where cache file is saved
#'
#' @return number of rows updated
#' @export
#'
update_master_file <- function(dat, cache_file_path, fill_gaps) {
  write_con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_file_path)
  target_file <- if (fill_gaps) "fg_master_file" else "rg_master_file"

  duckdb::duckdb_register(con, "append_data", dat, overwrite = TRUE)
  DBI::dbExecute(con, glue::glue("INSERT INTO {target_file} SELECT * FROM append_data;"))
  duckdb::dbDisconnect(write_con)
  message(glue::glue("{target_file} is updated."))

  return(nrow(dat))
}


#' Reset the cache. Only to be used internally
#'
#' @noRd
reset_cache <- function(pass = Sys.getenv('LOCAL_KEY'), type = c("both", "rg", "fg"), lkup) {
  # lkup will be passed through API and will not be an argument to endpoint, same as pip call
  # Checks if the keys match across local and server before reseting the cache
  if (pass != Sys.getenv('SERVER_KEY')) {
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
