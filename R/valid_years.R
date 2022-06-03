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
