#' Return available valid years
#'
#' @param data_dir character: Path to the root directory
#'
#' @return numeric vector of valid years
#' @export
#' @examples
#' lkups <- create_versioned_lkups(data_dir = "/pip-fake-data")
#' lkup <- lkups$versions_paths$`20200101_2011_01_01_PROD`
#' valid_years(lkup$data_root)

valid_years <- function(data_dir) {
  tmp <- pipapi::get_aux_table(data_dir, 'interpolated_means')
  sort(unique(tmp$reporting_year))
}
