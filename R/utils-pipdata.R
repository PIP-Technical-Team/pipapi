#' load refy list
#'
#' @param input_list list. output from [create_full_list]
#' @param path character: directory path
#'
#' @return character vector
#' @keywords internal
load_list_refy <- \(input_list, path){
  input_list <- transform_input(input_list)

  dl <- lapply(input_list, FUN = function(x) {
    qs::qread(file = fs::path(path, paste0(x$country_code, "_",
                                                x$year),
                              ext = "qs"))
    })

  names(dl) <- vapply(input_list, \(x) {
    paste0(x$country_code, x$year)
    },
    FUN.VALUE = character(1))
  dl
}


#' transform input list
#'
#' @inheritParams load_list_refy
#'
#' @return formated list
#' @keywords internal
transform_input <- function(input_list){
  country_codes <- input_list$country_code
  years <- input_list$year
  if (!is.list(years)) {
    years <- lapply(country_codes, function(x) years)
  }
  else {
    if (length(years) != length(country_codes)) {
      stop("The length of the 'year' list must match the length of the 'country_code' vector.")
    }
  }
  output_list <- lapply(seq_along(country_codes), function(i) {
    lapply(years[[i]], function(y) {
      list(country_code = country_codes[i], year = y)
    })
  })
  output_list <- unlist(output_list, recursive = FALSE)
  return(output_list)
}
