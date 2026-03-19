# blocked_aux_tables.R
#
# PURPOSE: Define which auxiliary tables are hidden from the public API.
#
# HOW IT WORKS
# ------------
# The default blocked list is defined in `.default_blocked_tables` below —
# edit that constant to add or remove tables from the default block.
#
# At API startup, you may also call `blocked_aux_tables(tables = c(...))` to
# override the default programmatically. The value is stored in `.pipapienv`
# and persists for the duration of the R session.
#
# `create_lkups()` calls `blocked_aux_tables()` and strips the returned names
# from `lkup$aux_tables` via `setdiff()`, so blocked tables are:
#   (a) absent from the "list all tables" response, and
#   (b) rejected as an invalid `table` query parameter.
#
# `get_aux_table()` also checks the blocked list before reading from disk, so
# direct R calls to a blocked table will error with a clear message.
#
# HOW TO ADD OR REMOVE A TABLE
# ----------------------------
# Edit `.default_blocked_tables` below. After editing:
#   1. Run `devtools::document()` to refresh the NAMESPACE / man page.
#   2. Re-create lkups (or redeploy) so `create_lkups()` picks up the change.
#   3. Update `tests/testthat/test-blocked-aux-tables.R` to match.

# Default list of blocked auxiliary tables.
# This constant is intentionally visible here — edit or empty it when a
# table no longer needs to be blocked.
# Current reason: "national_poverty_lines" is temporarily unavailable to
# API consumers pending a data review (blocked 2026-03-18).
.default_blocked_tables <- c("national_poverty_lines")

#' Get or set the list of blocked auxiliary tables
#'
#' When called with no arguments, returns the currently blocked table names.
#' When called with `tables`, stores that vector as the new blocked list for
#' the duration of the R session. Blocked tables are excluded from
#' [get_aux_table()] and the API listing endpoints, and behave as if they do
#' not exist.
#'
#' The default list is defined in `.default_blocked_tables` at the top of
#' `R/blocked_aux_tables.R`. Edit or empty that constant when a table no
#' longer needs to be blocked.
#'
#' @param tables `character` vector of auxiliary table names to block, or
#'   `NULL` (default) to retrieve the current blocked list without modifying
#'   it. Table names are not validated against the available tables at this
#'   point — a misspelled name will be silently ignored at query time.
#'
#' @return When getting: a character vector of currently blocked table names.
#'   When setting: the new blocked list, invisibly.
#'
#' @seealso [reset_blocked_aux_tables()], [get_aux_table()], [create_lkups()]
#' @family aux
#' @export
#'
#' @examples
#' # Retrieve the current blocked list
#' blocked_aux_tables()
#'
#' # Override at API startup
#' blocked_aux_tables(tables = c("national_poverty_lines"))
#'
#' # Reset to defaults
#' reset_blocked_aux_tables()
blocked_aux_tables <- function(tables = NULL) {
  if (!is.null(tables)) {
    stopifnot(
      "`tables` must be a character vector" = is.character(tables),
      "`tables` must not contain NA values"  = !anyNA(tables)
    )
    set_in_pipapienv("blocked_tables", tables)
    return(invisible(tables))
  }
  # Fall back to the visible default constant if no runtime override is set.
  stored <- get_from_pipapienv("blocked_tables")
  if (is.null(stored)) .default_blocked_tables else stored
}

#' Reset blocked auxiliary tables to their default values
#'
#' Removes any runtime override set by [blocked_aux_tables()], reverting to
#' `.default_blocked_tables`. Primarily useful in tests or when reinitialising
#' the API configuration.
#'
#' @return The default blocked table list, invisibly.
#'
#' @seealso [blocked_aux_tables()]
#' @family aux
#' @export
#'
#' @examples
#' blocked_aux_tables(tables = c("foo"))
#' reset_blocked_aux_tables()
#' blocked_aux_tables() # back to .default_blocked_tables
reset_blocked_aux_tables <- function() {
  set_in_pipapienv("blocked_tables", NULL)
  invisible(.default_blocked_tables)
}
