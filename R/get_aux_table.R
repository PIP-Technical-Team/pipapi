#' Return specified auxiliary data
#'
#' @param data_dir character: Data directory
#' @param table character: Name of auxiliary table
#'
#' @return
#' @export
#'
#' @examples
get_aux_table <- function(data_dir, table) {

  out <- fst::read_fst(sprintf("%s_aux/%s.fst",
                               data_dir,
                               table))

  return(out)
}
