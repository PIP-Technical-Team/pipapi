#' Compute PIP statistics
#'
#' Compute the main PIP poverty and inequality statistics.
#'
#' This function is a wrapper around the [pip_new_lineups] and [pip_old_lineups]
#' functions.
#'
#' @inheritParams pip_new_lineups
#'
#' @return data.table
#' @examples
#' \dontrun{
#' # Create lkups
#' lkups <- create_lkups("<data-folder>")
#'
#' # A single country and year
#' pip(country = "AGO",
#'     year = 2000,
#'     povline = 1.9,
#'     lkup = lkups)
#'
#' # All years for a single country
#' pip(country = "AGO",
#'     year = "all",
#'     povline = 1.9,
#'     lkup = lkups)
#'
#' # Fill gaps
#' pip(country = "AGO",
#'     year = "all",
#'     povline = 1.9,
#'     fill_gaps = TRUE,
#'     lkup = lkups)
#'
#' # Group by regions
#' pip(country = "all",
#'     year = "all",
#'     povline = 1.9,
#'     group_by = "wb",
#'     lkup = lkups)
#' }
#' @export
pip <- function(country         = "ALL",
                year            = "ALL",
                povline         = 1.9,
                popshare        = NULL,
                fill_gaps       = FALSE,
                group_by        = c("none", "wb"),
                welfare_type    = c("all", "consumption", "income"),
                reporting_level = c("all", "national", "rural", "urban"),
                ppp             = NULL,
                lkup,
                censor          = FALSE,
                lkup_hash       = lkup$cache_data_id$hash_pip,
                additional_ind  = FALSE) {

  # Validate lkup structure first — before any lkup field access
  #-------------------------------------
  validate_lkup(lkup, c("core", "new_pathway"))

  # Should pip_old or pip_new be used?
  #-------------------------------------
  use_new <- lkup$use_new_lineup_version

  # Run correct function
  #-------------------------------------
  out <- if (use_new) {
    pip_new_lineups(country         = country,
                    year            = year,
                    povline         = povline,
                    popshare        = popshare,
                    fill_gaps       = fill_gaps,
                    welfare_type    = welfare_type,
                    reporting_level = reporting_level,
                    ppp             = ppp,
                    lkup            = lkup,
                    censor          = censor,
                    lkup_hash       = lkup_hash,
                    additional_ind  = additional_ind)
  } else {
    pip_old_lineups(country         = country,
                    year            = year,
                    povline         = povline,
                    popshare        = popshare,
                    fill_gaps       = fill_gaps,
                    group_by        = group_by,
                    welfare_type    = welfare_type,
                    reporting_level = reporting_level,
                    ppp             = ppp,
                    lkup            = lkup,
                    censor          = censor,
                    lkup_hash       = lkup_hash,
                    additional_ind  = additional_ind)
  }

  # Return
  #-------------------------------------
  out

}
