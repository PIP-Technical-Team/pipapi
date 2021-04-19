fg_pip <- function(country   = "all",
                   povline   = NULL,
                   popshare  = NULL,
                   year      = "all",
                   aggregate = FALSE,
                   welfare_type = "all",
                   svy_coverage = "all",
                   ppp       = NULL,
                   server    = NULL,
                   format    = "csv",
                   lkup,
                   paths) {

  # Handle interpolation

  metadata <- subset_lkup(country = country,
                         year = year,
                         welfare_type = welfare_type,
                         svy_coverage = svy_coverage,
                         lkup = lkup[["ref_lkup"]])
  # return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(pipapi::empty_response)
  }
  # Extract unique combinations of country-year
  ctry_years = unique(metadata[, .(country_code, reporting_year, pop_data_level)])

  out <- vector(mode = "list", length = nrow(ctry_years))

  for (i in seq_along(out)) {

    ctry_year <- ctry_years[i, , drop = FALSE]
    tmp_year  <- ctry_year[["reporting_year"]]

    tmp_metadata <- metadata[ctry_year,
                             on = .(country_code, reporting_year, pop_data_level),
                             allow.cartesian = TRUE]

    svy_data <- get_svy_data(tmp_metadata[["cache_id"]],
                             svy_coverage = tmp_metadata[["pop_data_level"]],
                             paths = paths)

    tmp_stats <- wbpip:::prod_fg_compute_pip_stats(request_year = tmp_year,
                                                   data = svy_data,
                                                   predicted_request_mean = tmp_metadata[["predicted_mean_ppp"]],
                                                   svy_mean_lcu = tmp_metadata[["survey_mean_lcu"]],
                                                   survey_year = tmp_metadata[["survey_year"]],
                                                   default_ppp = tmp_metadata[["ppp"]],
                                                   ppp = ppp,
                                                   distribution_type = tmp_metadata[["distribution_type"]],
                                                   poverty_line = povline)

    # Ensure that tmp_metadata has a single row
    vars_to_collapse <- c("survey_id", "cache_id", "surveyid_year", "survey_year",
                         "survey_acronym", "survey_coverage", "survey_comparability",
                         "welfare_type", "distribution_type", "gd_type", "predicted_mean_ppp", "survey_mean_lcu")
    tmp_metadata <- collapse_rows(df = tmp_metadata,
                                  vars = vars_to_collapse,
                                  na_var = "survey_mean_ppp")

    # Add stats columns to data frame
    for (j in seq_along(tmp_stats)) {
      tmp_metadata[[names(tmp_stats)[j]]] <- tmp_stats[[j]]
    }

    out[[i]] <- tmp_metadata
  }
  out <- data.table::rbindlist(out)

  return(out)
}
