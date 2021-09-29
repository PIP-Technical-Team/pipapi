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

  unique_survey_files <- unique(metadata$data_interpolation_id)

  # Interpolation list
  interpolation_list <- lkup$interpolation_list
  interpolation_list <- interpolation_list[names(interpolation_list) %in% unique_survey_files]

  # Unique set of survey data to be read
  out <- vector(mode = "list", length = length(unique_survey_files))

  #NEW: iterate over survey files
  for (svy_id in seq_along(unique_survey_files)) {

    # Extract country-years for which stats will be computed from the same files
    # tmp_metadata <- interpolation_list[[unique_survey_files[svy_id]]]$tmp_metadata
    svy_data <- get_svy_data(interpolation_list[[unique_survey_files[svy_id]]]$cache_ids,
                             reporting_level = interpolation_list[[unique_survey_files[svy_id]]]$reporting_level,
                             path = interpolation_list[[unique_survey_files[svy_id]]]$paths
    )

    # Extract unique combinations of country-year
    ctry_years <- subset_lkup(country = country,
                              year = year,
                              welfare_type = welfare_type,
                              reporting_level = reporting_level,
                              lkup = interpolation_list[[unique_survey_files[svy_id]]]$ctry_years)

    results_subset <- vector(mode = "list", length = nrow(ctry_years))

    for (ctry_year_id in seq_along(ctry_years$interpolation_id)) {

      # Extract records to be used for a single country-year estimation
      tmp_metadata <- metadata[metadata$interpolation_id == ctry_years[["interpolation_id"]][ctry_year_id], ]

      # Compute estimated statistics using the fill_gap method
      if (debug) debugonce(wbpip:::prod_fg_compute_pip_stats)
      tmp_stats <- wbpip:::prod_fg_compute_pip_stats(
        request_year = ctry_years[["reporting_year"]][ctry_year_id],
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

      # Ensure that tmp_metadata has a single row
      # vars_to_collapse <- c(
      #   "survey_id", "cache_id", "surveyid_year", "survey_year",
      #   "survey_acronym", "survey_coverage", "survey_comparability",
      #   "comparable_spell", "welfare_type",
      #   "gd_type", "mean", "predicted_mean_ppp", "survey_mean_lcu", "survey_mean_ppp",
      #   "interpolation_id", "path", "cpi"
      # )
      #
      # tmp_metadata[, vars_to_collapse] <- NA
      #
      # Handle multiple distribution types (for aggregated distributions)
      if (length(unique(tmp_metadata$distribution_type)) > 1) {
        tmp_metadata$distribution_type <- "mixed"
      }
      #
      # tmp_metadata <- unique(tmp_metadata)


      # Add stats columns to data frame
      for (stat in seq_along(tmp_stats)) {
        tmp_metadata[[names(tmp_stats)[stat]]] <- tmp_stats[[stat]]
      }


      # tmp_metadata <- collapse_rows(
      #   df = tmp_metadata,
      #   vars = vars_to_collapse,
      #   na_var = "survey_mean_ppp"
      # )
      results_subset[[ctry_year_id]] <- tmp_metadata
    }

    out[[svy_id]] <- results_subset
  }

  # out <- purrr::flatten(out)
  out <- unlist(out, recursive = FALSE)
  out <- data.table::rbindlist(out)

  # Ensure that out does not have duplicates
  vars_to_collapse <- c(
    "survey_id", "cache_id", "surveyid_year", "survey_year",
    "survey_acronym", "survey_coverage", "survey_comparability",
    "comparable_spell", "welfare_type",
    "gd_type", "mean", "predicted_mean_ppp", "survey_mean_lcu", "survey_mean_ppp",
    "interpolation_id", "path", "cpi"
  )
  out[, vars_to_collapse] <- NA
  out <- unique(out)

  return(out)
}
