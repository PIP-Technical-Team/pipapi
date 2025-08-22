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


#' Add attributes as columns (vectorized, in-place)
#'
#' @description
#' Converts survey attributes on a `data.table`—including
#' `reporting_level_rows`, `country_code`, `reporting_year`, and `dist_stats`—
#' into columns using a **loop-free, segment-replication** strategy. Designed
#' for very large tables and objects loaded via `readRDS()`/`load()`:
#' uses `setDT()` and `alloc.col()` to ensure in-place assignment.
#'
#' @details
#' The function expects an attribute `reporting_level_rows`, a list with:
#' - `reporting_level`: character vector of the level label for each segment
#'   (e.g., `c("rural","urban","rural", ...)`).
#' - `rows`: integer vector of **cumulative** row-ends (e.g.,
#'   `c(100000, 200000, 300000, ...)`).
#'
#' Segment lengths are computed as `diff(c(0L, rows))`, and `reporting_level`
#' is replicated with `rep.int(lev, counts)`. Constants `country_code`,
#' `reporting_year`, and `file` (`paste0(country_code, "_", reporting_year)`)
#' are added to all rows. If `dist_stats$mean` / `dist_stats$median` are
#' provided (as named vectors/lists keyed by level), they are mapped by level
#' name and replicated per segment. If a level is missing from the names,
#' `NA` values may result for that segment.
#'
#' This implementation avoids loops and `findInterval()` edge cases, and
#' modifies `dt` by reference.
#'
#' @param dt A `data.table` carrying the attributes described above.
#'
#' @return The same `data.table`, modified by reference, with added columns:
#'   `reporting_level`, `country_code`, `reporting_year`, `file`, and (if
#'   present) `mean`, `median`.
#'
#' @section Assumptions:
#' * `length(reporting_level_rows$reporting_level) == length(reporting_level_rows$rows)`.
#' * `rows` are cumulative and non-decreasing, and their segment lengths sum to `nrow(dt)`.
#' * If `dist_stats$mean` / `dist_stats$median` have multiple values, their names
#'   align with the level labels.
#'
#' @note For objects loaded from disk (e.g., via `readRDS()`), `alloc.col(dt)`
#'       ensures there is spare column capacity for by-reference assignment.
#'
#' @seealso [add_attributes_as_columns_multi()], [assign_stat()]
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(weight = 1:6, welfare = runif(6))
#' attr(dt, "reporting_level_rows") <- list(
#'   reporting_level = c("rural","urban","rural"),
#'   rows = c(2L, 4L, 6L)
#' )
#' attr(dt, "country_code")   <- "XXY"
#' attr(dt, "reporting_year") <- 2000L
#' attr(dt, "dist_stats") <- list(
#'   mean   = list(rural = 2.5, urban = 5.0),
#'   median = list(rural = 2.0, urban = 4.5)
#' )
#'
#' add_attributes_as_columns_vectorized(dt)
#' head(dt)
#' }
#'
#' @import data.table
#' @export
add_attributes_as_columns_vectorized <- function(dt) {

  # Ensure proper internal state & spare column capacity (handles readRDS/load cases)
  setDT(dt)        # harmless if already a data.table
  setalloccol(dt)    # pre-allocate room for new columns... #AC, I am still not sure about this.

  rl   <- attr(dt, "reporting_level_rows")
  lev  <- rl$reporting_level
  rows <- as.integer(rl$rows)
  n    <- fnrow(dt)

  counts <- diff(c(0L, rows))
  if (sum(counts) != n) cli::cli_abort("Sum of 'rows' in attribute does not equal nrow(dt).")

  # reporting_level: optimized assignment by range
  reporting_level_vec <- character(n)
  start <- 1L
  for (i in seq_along(lev)) {
    end <- rows[i]
    reporting_level_vec[start:end] <- lev[i]
    start <- end + 1L
  }
  dt[, reporting_level := reporting_level_vec]

  # constants
  cc <- attr(dt, "country_code")
  ry <- attr(dt, "reporting_year")
  dt[, `:=`(
    country_code   = cc,
    reporting_year = ry,
    file           = paste0(cc, "_", ry)
  )]

  # dist_stats per reporting_level (align by names, then replicate by counts)
  ds <- attr(dt, "dist_stats")
  if (length(ds)) {
    if (!is.null(ds$mean)) {
      for (l in lev) {
        dt[reporting_level == l,
           mean := ds$mean[[l]]]
      }
    }
    if (!is.null(ds$median)) {
      for (l in lev) {
        dt[reporting_level == l,
           median := ds$median[[l]]]
      }
    }
  }

  dt
}



#' Add attributes as columns for multi-segment reporting levels
#'
#' @description
#' Converts attributes on a survey `data.table` (e.g., `reporting_level_rows`,
#' `country_code`, `reporting_year`, and `dist_stats`) into columns, handling
#' **multiple alternating segments** (e.g., CHN rural/urban/rural/urban) or
#' single-segment cases (e.g., ZAF).
#'
#' @param dt A `data.table` with attributes:
#'   - `reporting_level_rows`: list with `reporting_level` (character) and
#'     `rows` (integer cumulative row ends).
#'   - `country_code` (character).
#'   - `reporting_year` (integer/numeric).
#'   - `dist_stats` (list) optionally containing `mean` and/or `median`, each as
#'     a named list/vector keyed by reporting level, or a single scalar.
#'
#' @return The same `data.table`, modified by reference, with new columns:
#'   `reporting_level`, `country_code`, `reporting_year`, `file`, and
#'   optionally `mean`, `median`.
#'
#' @examples
#' # chn2000_cols <- add_attributes_as_columns_multi(chn2000)
#' # zaf2000_cols <- add_attributes_as_columns_multi(zaf2000)
#' @import data.table
#' @export
add_attributes_as_columns_multi <- function(dt) {
  # Ensure DT internals and spare capacity for new columns
  setDT(dt)
  alloc.col(dt)

  # --- Pull + validate segment metadata ---
  rl <- attr(dt, "reporting_level_rows")
  if (is.null(rl) || is.null(rl$reporting_level) || is.null(rl$rows)) {
    cli::cli_abort("Missing 'reporting_level_rows' attribute with $reporting_level and $rows.")
  }
  lev  <- as.character(rl$reporting_level)
  rows <- as.integer(rl$rows)
  n    <- nrow(dt)

  if (length(lev) != length(rows)) cli::cli_abort("'reporting_level' and 'rows' lengths differ.")
  if (length(rows) == 0L) cli::cli_abort("'rows' is empty.")
  if (any(diff(rows) < 0L)) cli::cli_abort("'rows' must be non-decreasing.")
  if (rows[length(rows)] != n) cli::cli_abort("Last element of 'rows' must equal nrow(dt).")

  counts <- diff(c(0L, rows))
  if (any(counts <= 0L)) cli::cli_abort("Computed non-positive segment length(s).")

  # --- reporting_level: vectorized per-segment replication ---
  dt[, reporting_level := rep.int(lev, counts)]

  # --- constants ---
  cc <- attr(dt, "country_code")
  ry <- attr(dt, "reporting_year")
  dt[, `:=`(
    country_code   = cc,
    reporting_year = ry,
    file           = paste0(cc, "_", ry)
  )]

  # --- distribution stats ---
  ds <- attr(dt, "dist_stats")
  if (length(ds)) {
    assign_stat(dt, lev, counts, ds$mean,   "mean")
    assign_stat(dt, lev, counts, ds$median, "median")
  }

  dt
}



#' Assign a per-level statistic to a data.table column (by reference)
#'
#' @description
#' Replicates a statistic per reporting-level segment and assigns it to a new
#' column in `dt`, **in place**. `stat` can be a scalar (broadcast), a named
#' vector, or a named list (one value per level).
#'
#' @param dt A `data.table`. Modified by reference.
#' @param lev Character vector of reporting-level labels per segment
#'   (e.g., `c("rural","urban","rural", ...)`).
#' @param counts Integer vector of segment lengths matching `lev`
#'   (e.g., `c(100000, 100000, 100000, ...)`).
#' @param stat A numeric scalar, named vector, or named list with one value per
#'   level (names must match `lev` values).
#' @param colname Name of the column to create/overwrite.
#'
#' @return Invisibly returns `dt` (modified by reference).
#' @examples
#' # assign_stat(dt, lev, counts, list(rural = 2.6, urban = 5.5), "mean")
#' @import data.table
#' @export
assign_stat <- function(dt, lev, counts, stat, colname) {
  if (is.null(stat)) return(invisible(dt))
  n <- nrow(dt)

  v <- if (is.list(stat)) unlist(stat, use.names = TRUE) else stat

  # Single scalar: broadcast
  if (length(v) == 1L && is.null(names(v))) {
    dt[, (colname) := rep.int(unname(v), n)]
    return(invisible(dt))
  }

  # Need names to map values to levels
  if (is.null(names(v))) {
    stop("`stat` has length > 1 but no names; cannot map to levels.")
  }

  map_idx <- match(lev, names(v))
  if (anyNA(map_idx)) {
    missing_levels <- unique(lev[is.na(map_idx)])
    stop(
      sprintf("`stat` missing value(s) for level(s): %s",
              paste(missing_levels, collapse = ", "))
    )
  }

  dt[, (colname) := rep.int(unname(v[map_idx]), counts)]
  invisible(dt)
}
