#' Return available valid years
#'
#' @param data_dir character: Path to the versioned data in main data directory
#'
#' @return numeric vector of valid years
#' @export
#' @examples valid_years('pip-fake-data/20200101_2011_01_01_PROD/')

valid_years <- function(data_dir) {
  tmp <- fst::read_fst(paste0(data_dir, '/_aux/interpolated_means.fst'))
  sort(unique(tmp$reporting_year))
}
