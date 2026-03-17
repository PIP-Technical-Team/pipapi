# utils-aux.R
#
# Thin wrappers around get_aux_table() for specific auxiliary datasets.
# Each function returns an empty data.table with the correct schema when
# the underlying data file is unavailable.
#
# Functions:
#   get_svy_data()        - read survey microdata from fst files
#   get_spr_table()       - load spr_svy or spr_lnp aux table
#   get_metaregion_table()- load metaregion aux table
#   get_pg_table()        - load pg_svy or pg_lnp aux table


#' Read survey microdata
#'
#' Reads welfare and weight columns from one or more fst files. For urban/rural
#' reporting levels the \code{area} column is used to subset rows.
#'
#' @param svy_id character: Survey ID (used only for naming output list elements)
#' @param reporting_level character: Geographical reporting level
#' @param path character: Path(s) to survey fst files
#'
#' @return Named list of data.tables, one per element of \code{path}
#' @keywords internal
get_svy_data <- function(svy_id, reporting_level, path) {
  # Each call should be made at a unique reporting_level
  reporting_level <- unique(reporting_level)
  assertthat::assert_that(
    length(reporting_level) == 1,
    msg = "Problem with input data: Multiple reporting_levels"
  )

  out <- lapply(path, function(x) {
    if (reporting_level %in% c("urban", "rural")) {
      tmp <- fst::read_fst(
        x,
        columns = c("area", "welfare", "weight"),
        as.data.table = TRUE
      )
      tmp <- tmp[area == reporting_level, ]
      tmp[, area := NULL]
    } else {
      tmp <- fst::read_fst(
        x,
        columns = c("welfare", "weight"),
        as.data.table = TRUE
      )
    }

    return(tmp)
  })

  names(out) <- sprintf("df%s", seq_along(svy_id) - 1)

  return(out)
}


#' Load SPR table from aux data
#'
#' Returns an empty data.table with the correct schema when no data is
#' available.
#'
#' @inheritParams get_aux_table
#' @param table character: one of \code{"spr_svy"} or \code{"spr_lnp"}
#'
#' @return data.table
#' @keywords internal
get_spr_table <- function(data_dir, table = c("spr_svy", "spr_lnp")) {
  table <- match.arg(table)

  spr <-
    tryCatch(
      expr = {
        get_aux_table(data_dir = data_dir, table = table)
      },
      error = function(e) {
        data.table::data.table(
          country_code = character(0),
          reporting_year = numeric(0),
          welfare_type = character(0),
          reporting_level = character(0),
          spl = numeric(0),
          spr = numeric(0),
          median = numeric(0)
        )
      }
    )
  return(spr)
}


#' Load metaregion table from aux data
#'
#' Returns an empty data.table with the correct schema when no data is
#' available.
#'
#' @inheritParams get_aux_table
#'
#' @return data.table
#' @keywords internal
get_metaregion_table <- function(data_dir) {
  spr <-
    tryCatch(
      expr = {
        get_aux_table(data_dir = data_dir, table = "metaregion")
      },
      error = function(e) {
        data.table::data.table(
          region_code = character(0),
          lineup_year = numeric(0)
        )
      }
    )
  return(spr)
}


#' Load Prosperity Gap table from aux data
#'
#' Returns an empty data.table with the correct schema when no data is
#' available.
#'
#' @inheritParams get_aux_table
#' @param table character: one of \code{"pg_svy"} or \code{"pg_lnp"}
#'
#' @return data.table
#' @keywords internal
get_pg_table <- function(data_dir, table = c("pg_svy", "pg_lnp")) {
  table <- match.arg(table)

  pg <-
    tryCatch(
      expr = {
        get_aux_table(data_dir = data_dir, table = table)
      },
      error = function(e) {
        data.table::data.table(
          country_code = character(0),
          reporting_level = character(0),
          pg = numeric(0),
          welfare_type = character(0),
          reporting_year = integer(0)
        )
      }
    )
  return(pg)
}
