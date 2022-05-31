#' Return available valid years
#'
#' @param data_dir character: Path to the root directory
#'
#' @return numeric vector of valid years
#' @export

valid_years <- function(data_dir) {
  tmp <- pipapi::get_aux_table(data_dir, 'interpolated_means')
  sort(unique(tmp$reporting_year))
}
