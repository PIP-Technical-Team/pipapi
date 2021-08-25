#' Compute imputed year stats
#'
#' Compute the main PIP poverty and inequality statistics for imputed years.
#'
#' @inheritParams pip
#' @return data.frame
#' @keywords internal
fg_pip <- function(country,
                   year,
                   povline,
                   popshare,
                   aggregate,
                   welfare_type,
                   reporting_level,
                   ppp,
                   lkup,
                   debug) {

  # Handle interpolation
  metadata <- subset_lkup(
    country = country,
    year = year,
    welfare_type = welfare_type,
    reporting_level = reporting_level,
    lkup = lkup[["ref_lkup"]]
  )

  # Return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(pipapi::empty_response)
  }

  # Extract unique combinations of country-year
  ctry_years <- unique(metadata[, .(
    country_code, reporting_year,
    pop_data_level, interpolation_id
  )])

  out <- vector(mode = "list", length = nrow(ctry_years))

  for (i in seq_along(out)) {

    # Extract records to be used for a single country-year estimation
    tmp_metadata <- metadata[interpolation_id == ctry_years[["interpolation_id"]][i], ]

    svy_data <- get_svy_data(tmp_metadata[["cache_id"]],
      reporting_level = tmp_metadata[["pop_data_level"]],
      path = tmp_metadata$path
    )

    # Compute estimated statistics using the fill_gap method
    if (debug) debugonce(wbpip:::prod_fg_compute_pip_stats)
    tmp_stats <- wbpip:::prod_fg_compute_pip_stats(
      request_year = ctry_years[["reporting_year"]][i],
      data = svy_data,
      predicted_request_mean = tmp_metadata[["predicted_mean_ppp"]],
      svy_mean_lcu = tmp_metadata[["survey_mean_lcu"]],
      survey_year = tmp_metadata[["survey_year"]],
      default_ppp = tmp_metadata[["ppp"]],
      ppp = ppp,
      distribution_type = tmp_metadata[["distribution_type"]],
      poverty_line = povline,
      popshare = popshare
    )


    # Add stats columns to data frame
    for (j in seq_along(tmp_stats)) {
      tmp_metadata[[names(tmp_stats)[j]]] <- tmp_stats[[j]]
    }

    # Ensure that tmp_metadata has a single row
    vars_to_collapse <- c(
      "survey_id", "cache_id", "surveyid_year", "survey_year",
      "survey_acronym", "survey_coverage", "survey_comparability",
      "comparable_spell", "welfare_type", "distribution_type",
      "gd_type", "predicted_mean_ppp", "survey_mean_lcu",
      "interpolation_id", "path", "cpi"
    )
    tmp_metadata <- collapse_rows(
      df = tmp_metadata,
      vars = vars_to_collapse,
      na_var = "survey_mean_ppp"
    )
    out[[i]] <- tmp_metadata
  }

  out <- data.table::rbindlist(out)

  return(out)
}
