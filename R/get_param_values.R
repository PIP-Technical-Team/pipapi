#' Get valid query parameter values
#' Get vector of accepted query parameter values
#' @param param character: Query parameter
#' @inheritParams pip
#' @export
get_param_values <- function(param, lkup) {
  out <- lkup$query_controls[[param]]$values
  if (param == "parameter")
    out <- out[!out == "parameter"]
  out <- data.frame(out)
  names(out) <- param
  return(out)
}
