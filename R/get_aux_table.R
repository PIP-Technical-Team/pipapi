#' Return specified auxiliary data
#'
#' @param data_dir character: Data directory
#' @param table character: Name of auxiliary table
#' @param long_format logical: do you want data long format ? (default FALSE)
#'
#' @return data.frame
#' @export
#'
get_aux_table <- function(data_dir = NULL,
                          table, long_format = FALSE) {

  if (is.null(data_dir)) {
    if (exists("lkup", inherits = TRUE)) {
      data_dir <- get("lkup", inherits = TRUE)$data_root
    } else {
      cli::cli_abort("{.code data_dir} not defined and {.field lkup} not found.")
  }
}

  if (long_format && !table %in% get_valid_aux_long_format_tables()) {
    long_format <- FALSE
  }
  # Strip all "non-word" characters from user input
  sanitized_table <- gsub("\\W", "", table)


  out <- fst::read_fst(sprintf(
    "%s/_aux/%s.fst",
    data_dir,
    sanitized_table),
    as.data.table = TRUE)

  if (long_format) {
    out <- data.table::melt(out,
                            id.vars = c('country_code', 'data_level'),
                            variable.name = "year")
    data.table::setorder(out, "country_code", "year", "data_level")
  }

  return(out)
}

#' Return specified auxiliary data in wide format
#' Helper function to the UI
#' @param data_dir character: Data directory
#' @param table character: Name of auxiliary table
#'
#' @return data.frame
#' @export
#'
get_aux_table_ui <- function(data_dir, table) {

  out <- get_aux_table(data_dir    = data_dir,
                       table       = table,
                       long_format = FALSE)

  return(out)
}
