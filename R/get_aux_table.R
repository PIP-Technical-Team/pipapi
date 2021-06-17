#' Return specified auxiliary data
#'
#' @param data_dir character: Data directory
#' @param data_version character: Release version of the data in the format yyyymmdd
#' @param table character: Name of auxiliary table
#'
#' @return data.frame
#' @export
#'
get_aux_table <- function(data_dir, data_version, table) {

  out <- fst::read_fst(sprintf("%s/%s/_aux/%s.fst",
                               data_dir,
                               data_version,
                               table))

  return(out)
}
