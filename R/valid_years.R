#' Return available valid years
#'
#' @param data_dir character: Path to the root directory
#'
#' @return numeric vector of valid years
#' @export

valid_years <- function(data_dir) {
  ref <- pipapi::get_aux_table(data_dir, 'interpolated_means')
  ref <- sort(unique(ref$reporting_year))

  svy <- pipapi::get_aux_table(data_dir, 'survey_means')
  svy <- sort(unique(svy$reporting_year))

  return(
    list(
      valid_survey_years       = svy,
      valid_interpolated_years = ref
    )
  )
}



#'  lineup year for the world
#'
#' @param data_dir character: Path to the root directory
#'
#' @return numeric vector of length one of lineup year for the world
#' @export
wld_lineup_year <- function(data_dir) {
  ref <- pipapi::get_aux_table(data_dir, 'metaregion') |>
    setDT()
  if (nrow(ref) == 0) {
    nowcast_year <- as.integer(format(Sys.Date(), "%Y"))
  } else {
    nowcast_year <- ref[region_code == "WLD", lineup_year]
  }
  data.table(nowcast_year = nowcast_year,
             tooltip_text = paste("Estimates after",nowcast_year,"are nowcasted."))

}
