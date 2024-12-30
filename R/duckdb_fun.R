#' Return the rows of the table if they exist in master file
#'
#' @param country_code Country Code
#' @param year Year
#' @param poverty_line Poverty Lines
#' @param con Connection object
#'
#' @return Dataframe
#' @export
#'
return_if_exists <- function(country_code, year, poverty_line, con) {
  all_args_data <- all_args(country_code, year, poverty_line) |>
    duckplyr::as_duckplyr_tibble()
  # This file will be read from shared drive which will be an argument of this function.
  # Additionally there were will more arguments to join instead of only 3
  # In fact, it will be joined by all the arguments in `pip` call
  # It is not possible to append to parquet file https://stackoverflow.com/questions/39234391/how-to-append-data-to-an-existing-parquet-file
  # Writing entire data will be very costly as data keeps on growing, better is to save data in duckdb and append to it.
  master_file <- DBI::dbGetQuery(con, "select * from master_file") |>
    duckplyr::as_duckplyr_tibble()

  args_not_present_in_master <- duckplyr::anti_join(
    all_args_data, master_file,
    by = c("country_code", "reporting_year", "poverty_line")
  )
  args_present_in_master <- duckplyr::inner_join(
    master_file, all_args_data,
    by = c("country_code", "reporting_year", "poverty_line")
  )

  if(nrow(args_present_in_master)) {
    message("Returning data from cache.")
  }
  return(list(present_data = args_present_in_master, absent_args = args_not_present_in_master))
}

#' Create a dataframe with all possible combinations of `country_code`, `reporting_year` and `poverty_line`
#'
#' @param country_code Code of countries to be expanded
#' @param reporting_year Reported year(s)
#' @param poverty_line Poverty Line(s)
#'
#' @return A dataframe
#'
all_args <- function(country_code, reporting_year, poverty_line) {
  expand.grid(country_code = country_code, reporting_year = reporting_year, poverty_line = poverty_line)
}

#' Update master file with the contents of the dataframe
#'
#' @param dat Dataframe to be appended
#' @param con DuckDB connection object
#'
#' @return number of rows updated
#' @export
#'
update_master_file <- function(dat, con) {
  duckdb::duckdb_register(con, "append_data", dat, overwrite = TRUE)
  DBI::dbExecute(con, "INSERT INTO master_file SELECT * FROM append_data;")
  message("Master File is updated.")

  return(nrow(dat))
}
