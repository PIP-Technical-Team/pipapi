# blocked_aux_tables.R
#
# PURPOSE: Define which auxiliary tables are hidden from the public API.
#
# HOW IT WORKS
# ------------
# `create_lkups()` calls `blocked_aux_tables()` and strips the returned names
# from `lkup$aux_tables` via `setdiff()`.  Because both the API table listing
# (plumber /api/v1/aux and /api/v1/ui_aux) and the plumber validation filter
# consume `lkup$aux_tables`, a blocked table is automatically:
#   (a) absent from the "list all tables" response, and
#   (b) rejected as an invalid `table` query parameter.
#
# WHAT IS NOT AFFECTED
# --------------------
# Direct R calls to `get_aux_table()` still work for blocked tables.
# `get_aux_table()` reads from disk and never consults `lkup$aux_tables`.
#
# HOW TO ADD OR REMOVE A TABLE
# ----------------------------
# Edit the character vector returned by `blocked_aux_tables()` below.
# After editing:
#   1. Run `devtools::document()` to refresh the NAMESPACE / man page.
#   2. Re-create lkups (or redeploy) so `create_lkups()` picks up the change.
#   3. Add / remove the corresponding entry in the test file
#      `tests/testthat/test-blocked-aux-tables.R`.

#' Auxiliary tables blocked from the public API
#'
#' Returns a character vector of auxiliary table names that are hidden from
#' the PIP API.  Tables in this list are excluded from the `/api/v1/aux` and
#' `/api/v1/ui_aux` table listings, and are rejected by the plumber validation
#' filter when supplied as the `table` query parameter.
#'
#' Direct R calls to [get_aux_table()] are **not** affected — blocked tables
#' can still be read programmatically from R.
#'
#' @section Modifying the blocklist:
#' Edit the character vector returned by this function.  Then:
#' \enumerate{
#'   \item Run `devtools::document()`.
#'   \item Re-create lkups (or redeploy) so [create_lkups()] picks up the
#'         change.
#'   \item Update `tests/testthat/test-blocked-aux-tables.R` to match.
#' }
#'
#' @return A character vector of table names (may be `character(0)` when no
#'   tables are blocked).
#'
#' @seealso [get_aux_table()], [create_lkups()]
#' @family aux
#' @export
#'
#' @examples
#' blocked_aux_tables()
blocked_aux_tables <- function() {
  # ── Edit this vector to add or remove blocked tables ──────────────────────
  # Current reason: "national_poverty_lines" is temporarily unavailable to
  # API consumers pending a data review (blocked 2026-03-18).
  c("national_poverty_lines")
}
