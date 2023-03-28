#' Return Societal Poverty Line (SPL) statistics
#'
#' @param country character: Country ISO 3 codes
#' @param data_dir character: Data directory
#'
#' @return data.frame
#' @export
#' @noRd

get_spl <- function(country = "ALL",
                    data_dir) {
  out <- get_aux_table(data_dir = data_dir,
                       table = "spl",
                       long_format = FALSE)

  if (!"ALL" %chin% country) {
    out <- out[out[["country_code"]] %chin% country, ]
  }

  return(out)
}
