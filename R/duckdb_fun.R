#' Return the rows of the table if they exist in master file
#'
#' @param country_code Country Code
#' @inheritParams subset_lkup
#' @param con Connection object
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
#' @param con DuckDB connection object
#'
#' @return number of rows updated
#' @export
#'
update_master_file <- function(dat, con, fill_gaps) {
  target_file <- if (fill_gaps) "fg_master_file" else "rg_master_file"

  duckdb::duckdb_register(con, "append_data", dat, overwrite = TRUE)
  DBI::dbExecute(con, glue::glue("INSERT INTO {target_file} SELECT * FROM append_data;"))
  message(glue::glue("{target_file} is updated."))

  return(nrow(dat))
}
