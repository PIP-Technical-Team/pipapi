#' Return specified auxiliary data
#'
#' @param data_dir character: Data directory
#' @param table character: Name of auxiliary table
#'
#' @return data.frame
#' @export
#'
get_aux_table <- function(data_dir, table) {
  # Strip all "non-word" characters from user input
  sanitize_table_name <- gsub("\\W", "", table)

  out <- fst::read_fst(sprintf(
    "%s/_aux/%s.fst",
    data_dir,
    sanitize_table_name
  ))

  return(out)
}
