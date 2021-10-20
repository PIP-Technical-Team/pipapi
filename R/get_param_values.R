#' Get valid query parameter values
#' Get vector of accepted query parameter values
#' @param lkup list: A list of lkup tables
#' @param version character: Data version. Defaults to most recent version.
#' @export
get_param_values <- function(lkup, version) {
  # Extract accepted data versions values
  data_version <- data.frame(
    param_names  = "version",
    param_values = lkup$versions,
    param_boundaries = NA_character_,
    param_types = "character",
    stringsAsFactors = FALSE
  )
  # Extract correct query_controls list
  lkup <- lkup$versions_paths[[version]]
  query_controls <- lkup$query_controls[names(lkup$query_controls) != "parameter"]

  # Turn list into a dataframe
  out <- vector(mode = "list", length(query_controls))
  names(out) <- names(query_controls)
  names_out <- names(out)

  for (i in seq_along(out)) {

    param_values <- query_controls[[i]]$values
    param_values_length <- length(param_values)
    # handle numeric query controls that don't have controlled vocabulary, but
    # only boundary conditions (min and max values)
    if (query_controls[[i]]$type == "numeric") {
      param_boundaries <- names(query_controls[[i]]$values)
    } else {
      param_boundaries <- rep(NA, param_values_length)
    }
    param_names <- rep(names_out[i], param_values_length)
    param_types  <- rep(query_controls[[i]]$type, param_values_length)

    out[[i]] <- data.frame(param_names      = param_names,
                           param_values     = param_values,
                           param_boundaries = param_boundaries,
                           param_types      = param_types,
                           stringsAsFactors = FALSE)

  }

  out <- data.table::rbindlist(out)
  out <- rbind(out, data_version)

  return(out)
}
