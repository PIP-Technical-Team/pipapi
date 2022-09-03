#' Return specified auxiliary data
#'
#' @param data_dir character: Data directory
#' @param table character: Name of auxiliary table
#' @param long_format logical: do you want data long format ? (default FALSE)
#'
#' @return data.frame
#' @export
#'
get_aux_table <- function(data_dir, table, long_format = FALSE) {
  # Strip all "non-word" characters from user input
  sanitized_table <- gsub("\\W", "", table)

  out <- fst::read_fst(sprintf(
    "%s/_aux/%s.fst",
    data_dir,
    sanitized_table
  ))

  out <- data.table::data.table(out)

  if (long_format) {
    out <- data.table::melt(out, id.vars = c('country_code', 'data_level'), variable.name = "year")
  }

  return(out)
}
