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
  if (!getOption("pipapi.query_live_data")) {
    target_file <- if (fill_gaps) "fg_master_file" else "rg_master_file"

    master_file <- DBI::dbGetQuery(con,
                                   glue::glue("select * from {target_file}")) |>
      duckplyr::as_duckplyr_tibble()

    data_present_in_master <-
      dplyr::inner_join(
        x = master_file,
        y = lkup |>
          collapse::fselect(country_code, reporting_year, is_interpolated, welfare_type),
        by = c("country_code", "reporting_year", "is_interpolated", "welfare_type")) |>
      dplyr::filter(poverty_line %in% povline)

    keep <- TRUE
    if (nrow(data_present_in_master) > 0) {
      # Remove the rows from lkup that are present in master
      keep <- !with(lkup, paste(country_code, reporting_year, is_interpolated, welfare_type)) %in%
        with(data_present_in_master, paste(country_code, reporting_year, is_interpolated, welfare_type))

      lkup <- lkup[keep, ]

      message("Returning data from cache.")
    }
  } else {
    data_present_in_master <- NULL
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

  duckdb::duckdb_register(write_con, "append_data", dat, overwrite = TRUE)
  DBI::dbExecute(write_con, glue::glue("INSERT INTO {target_file} SELECT * FROM append_data;"))
  duckdb::dbDisconnect(write_con)
  message(glue::glue("{target_file} is updated."))

  return(nrow(dat))
}


#' Reset the cache. Only to be used internally
#'
#' @noRd
reset_cache <- function(pass = Sys.getenv('PIP_CACHE_LOCAL_KEY'), type = c("both", "rg", "fg"), lkup) {
  # lkup will be passed through API and will not be an argument to endpoint, same as pip call
  # Checks if the keys match across local and server before reseting the cache
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
  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_file_path)
  DBI::dbExecute(con, "CREATE OR REPLACE table rg_master_file (
                 country_code VARCHAR,
                 survey_id VARCHAR,
                 cache_id VARCHAR,
                 wb_region_code VARCHAR,
                 reporting_year DOUBLE,
                 surveyid_year  VARCHAR,
                 survey_year   DOUBLE,
                 survey_time VARCHAR,
                 survey_acronym  VARCHAR,
                 survey_coverage  VARCHAR,
                 survey_comparability  DOUBLE,
                 comparable_spell   VARCHAR,
                 welfare_type   VARCHAR,
                 reporting_level   VARCHAR,
                 survey_mean_lcu    DOUBLE,
                 survey_mean_ppp    DOUBLE,
                 survey_median_ppp    DOUBLE,
                 survey_median_lcu    DOUBLE,
                 predicted_mean_ppp    DOUBLE,
                 ppp  DOUBLE,
                 cpi  DOUBLE,
                 reporting_pop    DOUBLE,
                 reporting_gdp    DOUBLE,
                 reporting_pce    DOUBLE,
                 pop_data_level   VARCHAR,
                 gdp_data_level   VARCHAR,
                 pce_data_level  VARCHAR,
                 cpi_data_level   VARCHAR,
                 ppp_data_level   VARCHAR,
                 distribution_type   VARCHAR,
                 gd_type   VARCHAR,
                 is_interpolated BOOLEAN,
                 is_used_for_line_up  BOOLEAN,
                 is_used_for_aggregation BOOLEAN,
                 estimation_type    VARCHAR,
                 display_cp  DOUBLE,
                 path VARCHAR,
                 country_name  VARCHAR,
                 africa_split  VARCHAR,
                 africa_split_code  VARCHAR,
                 region_name  VARCHAR,
                 region_code  VARCHAR,
                 world  VARCHAR,
                 world_code  VARCHAR,
                 poverty_line   DOUBLE,
                 mean   DOUBLE,
                 median DOUBLE,
                 headcount    DOUBLE,
                 poverty_gap   DOUBLE,
                 poverty_severity  DOUBLE,
                 watts     DOUBLE

  )")

  DBI::dbExecute(con, "CREATE OR REPLACE table fg_master_file (
                 country_code VARCHAR,
                 survey_id VARCHAR,
                 cache_id VARCHAR,
                 wb_region_code VARCHAR,
                 reporting_year DOUBLE,
                 surveyid_year  VARCHAR,
                 survey_year   DOUBLE,
                 survey_time VARCHAR,
                 survey_acronym  VARCHAR,
                 survey_coverage  VARCHAR,
                 survey_comparability  DOUBLE,
                 comparable_spell   VARCHAR,
                 welfare_type   VARCHAR,
                 reporting_level   VARCHAR,
                 survey_mean_lcu    DOUBLE,
                 survey_mean_ppp    DOUBLE,
                 survey_median_ppp    DOUBLE,
                 survey_median_lcu    DOUBLE,
                 predicted_mean_ppp    DOUBLE,
                 ppp  DOUBLE,
                 cpi  DOUBLE,
                 reporting_pop    DOUBLE,
                 reporting_gdp    DOUBLE,
                 reporting_pce    DOUBLE,
                 pop_data_level   VARCHAR,
                 gdp_data_level   VARCHAR,
                 pce_data_level  VARCHAR,
                 cpi_data_level   VARCHAR,
                 ppp_data_level   VARCHAR,
                 distribution_type   VARCHAR,
                 gd_type   VARCHAR,
                 is_interpolated BOOLEAN,
                 is_used_for_line_up  BOOLEAN,
                 is_used_for_aggregation BOOLEAN,
                 estimation_type    VARCHAR,
                 interpolation_id VARCHAR,
                 display_cp  DOUBLE,
                 country_name  VARCHAR,
                 africa_split  VARCHAR,
                 africa_split_code  VARCHAR,
                 region_name  VARCHAR,
                 region_code  VARCHAR,
                 world  VARCHAR,
                 world_code  VARCHAR,
                 path VARCHAR,
                 data_interpolation_id VARCHAR,
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
