#' Return specified auxiliary data
#'
#' @param data_dir character: Data directory
#' @param table character: Name of auxiliary table
#' @param long_format logical: do you want data long format ? (default FALSE)
#'
#' @return data.frame
#' @seealso [blocked_aux_tables()]
#' @family aux
#' @export
#'
get_aux_table <- function(data_dir = NULL, table, long_format = FALSE) {
  if (is.null(data_dir)) {
    if (exists("lkup", inherits = TRUE)) {
      data_dir <- get("lkup", inherits = TRUE)$data_root
    } else {
      cli::cli_abort(
        "{.code data_dir} not defined and {.field lkup} not found."
      )
    }
  }

  if (long_format && !table %in% get_valid_aux_long_format_tables()) {
    long_format <- FALSE
  }
  # Strip all "non-word" characters from user input
  sanitized_table <- gsub("\\W", "", table)

  # Reject tables that are currently blocked — behaves as if the table does
  # not exist. To change the blocked list, see blocked_aux_tables().
  if (sanitized_table %in% blocked_aux_tables()) {
    cli::cli_abort(
      c(
        "{.field {sanitized_table}} is not an available auxiliary table.",
        "i" = "Run {.code names(lkup$aux_tables)} to see available tables."
      )
    )
  }

  out <- fst::read_fst(
    sprintf(
      "%s/_aux/%s.fst",
      data_dir,
      sanitized_table
    ),
    as.data.table = TRUE
  )

  if (long_format) {
    out <- data.table::melt(
      out,
      id.vars = c('country_code', 'data_level'),
      variable.name = "year"
    )
    data.table::setorder(out, "country_code", "year", "data_level")
  }

  return(out)
}

#' Return specified auxiliary data in wide format
#' Helper function to the UI
#' @param data_dir character: Data directory
#' @param table character: Name of auxiliary table
#' @param esclude logical: whether or not to exclude some countries or regions...
#'
#' @return data.frame
#' @family aux
#' @export
#'
get_aux_table_ui <- function(data_dir, table, exclude = TRUE, lkup) {
  out <- get_aux_table(data_dir = data_dir, table = table, long_format = FALSE)

  if (table == "regions") {
    # TEMP START: remove old aggregations --------------
    cl <- lkup$aux_files$country_list

    regs <- cl[, .(region_code, africa_split_code)] |>
      unlist() |> # convert to vector
      na_omit() |>
      unique() |>
      unname() |>
      c("WLD") # add the world
    # TEMP END: remove old aggregations --------------

    out <- out[region_code %in% regs]
  } else if (table == "countries" && exclude == TRUE) {
    # hardcoded
    to_remove <- c("UKR")
    out <- out[!(country_code %in% to_remove)]
  }

  return(out)
}
