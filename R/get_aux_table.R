#' Return specified auxiliary data
#'
#' @param data_dir character: Data directory
#' @param table character: Name of auxiliary table
#'
#' @return data.frame
#' @export
#'
get_aux_table <- function(data_dir, table) {
  out <- fst::read_fst(sprintf(
    "%s/_aux/%s.fst",
    data_dir,
    table
  ))
  out <- rename_cols(out)

  return(out)
}
