# validate_lkup.R
#
# Reusable helpers for validating the lkup list passed to pip entry points.
# Guards against the most common failure mode: a caller passing a list() or
# a partially-constructed lkup that is missing fields accessed downstream.
#
# Functions:
#   validate_lkup()     - check a set of required fields by context name(s)
#   assert_lkup_field() - check a single named field

# Required fields by context ------------------------------------------------
# To add a new field group: append a named entry here, then call
# validate_lkup(lkup, "<new_group>") at the relevant entry point.
# All entry points (pip, pip_agg, pip_new_lineups) call validate_lkup()
# against at least the "core" and "new_pathway" groups.

.LKUP_REQUIRED_FIELDS <- list(
  core        = c("svy_lkup", "data_root", "return_cols",
                  "aux_files", "cache_data_id"),
  new_pathway = c("use_new_lineup_version", "interpolation_list",
                  "refy_lkup"),
  dist_stats  = c("dist_stats", "lineup_dist_stats"),
  censoring   = c("censored"),
  query       = c("query_controls")
)


#' Validate that required fields are present in a lkup list
#'
#' Checks that all fields required by the named context(s) are present in
#' `lkup`. Raises a tidy error via [cli::cli_abort()] on the first missing
#' field.
#'
#' @param lkup list: The lookup object to validate.
#' @param context character: One or more context names. Allowed values are
#'   `"core"`, `"new_pathway"`, `"dist_stats"`, `"censoring"`, `"query"`.
#'
#' @return Invisibly returns `lkup` if valid; aborts otherwise.
#' @examples
#' \dontrun{
#' validate_lkup(lkup, "core")
#' validate_lkup(lkup, c("core", "new_pathway"))
#' }
#' @keywords internal
validate_lkup <- function(lkup, context = "core") {
  bad_ctx <- setdiff(context, names(.LKUP_REQUIRED_FIELDS))
  if (length(bad_ctx) > 0L) {
    cli::cli_abort(
      c(
        "Unknown lkup context(s): {.val {bad_ctx}}.",
        i = "Allowed: {.val {names(.LKUP_REQUIRED_FIELDS)}}"
      )
    )
  }

  required <- unique(unlist(.LKUP_REQUIRED_FIELDS[context],
                             use.names = FALSE))

  for (field in required) {
    assert_lkup_field(lkup, field)
  }

  invisible(lkup)
}


#' Assert that a single field is present in a lkup list
#'
#' @param lkup list: The lookup object to check.
#' @param field character scalar: The field name to look for.
#'
#' @return Invisibly returns `lkup` if the field exists; aborts otherwise.
#' @examples
#' \dontrun{
#' assert_lkup_field(lkup, "svy_lkup")
#' }
#' @keywords internal
assert_lkup_field <- function(lkup, field) {
  if (!field %in% names(lkup)) {
    cli::cli_abort(
      c(
        "Required field {.field {field}} is missing from {.arg lkup}.",
        i = "Pass a fully-constructed lkup object (e.g. from {.fn create_lkups})."
      )
    )
  }
  invisible(lkup)
}
