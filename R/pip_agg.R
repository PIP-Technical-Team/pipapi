#' Logic for computing new aggregate
#'
#' @inheritParams pip
#' @return data.table
#' @examples
#' \dontrun{
#' # Create lkups
#' }
#' @export
pip_agg <- function(
  country = "ALL",
  year = "ALL",
  povline = 1.9,
  group_by = "wb",
  welfare_type = c("all", "consumption", "income"),
  reporting_level = c("all", "national"),
  lkup,
  censor = FALSE,
  lkup_hash = lkup$cache_data_id$hash_pip_grp,
  additional_ind = FALSE
) {
  # Should pip_old or pip_new be used?
  #-------------------------------------
  use_new <- lkup$use_new_lineup_version

  # check group_by
  group_by <- .check_group_by(group_by = group_by, lkup = lkup)

  # Run correct function
  #-------------------------------------
  out <- if (use_new) {
    pip_grp_new(
      country = country,
      year = year,
      povline = povline,
      welfare_type = welfare_type,
      reporting_level = reporting_level,
      lkup = lkup,
      censor = censor,
      additional_ind = additional_ind
    )
  } else {
    pip_grp_logic(
      country = country,
      year = year,
      povline = povline,
      group_by = group_by,
      welfare_type = welfare_type,
      reporting_level = reporting_level,
      lkup = lkup,
      censor = censor,
      lkup_hash = lkup_hash,
      additional_ind = additional_ind
    )
  }

  # Return
  #-------------------------------------
  out
}
