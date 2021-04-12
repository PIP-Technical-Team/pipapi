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
  ctry_years = unique(metadata[, c("country_code", "reporting_year", "pop_data_level")])

  out <- vector(mode = "list", length = nrow(ctry_years))

  for (i in seq_along(out)) {

    ctry_year <- ctry_years[i, , drop = FALSE]
    tmp_year  <- ctry_year[["reporting_year"]]

    tmp_metadata <- dplyr::left_join(ctry_year, metadata,
                                     by = c("country_code", "reporting_year", "pop_data_level"))

    svy_data <- get_svy_data(tmp_metadata$cache_id,
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
    var_to_collapse <- c("survey_id", "cache_id", "surveyid_year", "survey_year",
                         "survey_acronym", "survey_coverage", "survey_comparability",
                         "welfare_type", "distribution_type", "gd_type", "predicted_mean_ppp", "survey_mean_lcu")
    tmp_vars <- lapply(tmp_metadata[, var_to_collapse], unique, collapse = "|")
    tmp_vars <- lapply(tmp_vars, paste, collapse = "|")
    tmp_var_names <- names(tmp_metadata[, var_to_collapse])
    tmp_metadata$survey_mean_ppp <- NA_real_
    for (tmp_var in seq_along(tmp_vars)) {
      tmp_metadata[[tmp_var_names[tmp_var]]] <- tmp_vars[[tmp_var]]
    }
    tmp_metadata <- unique(tmp_metadata)

    # Add stats columns to data frame
    for (j in seq_along(tmp_stats)) {
      tmp_metadata[[names(tmp_stats)[j]]] <- tmp_stats[[j]]
    }

#     if (length(tmp_deciles) < 10) {
#       names_deciles <- paste0("decile", 1:10)
#       for (k in seq_along(names_deciles)) {
#         tmp_metadata[[names_deciles[k]]] <- NA
#       }
#     } else {
#       names_deciles <- paste0("decile", seq_along(tmp_deciles))
#       for (k in seq_along(names_deciles)) {
#         tmp_metadata[[names_deciles[k]]] <- tmp_deciles[k]
#       }
#     }

    out[[i]] <- tmp_metadata
  }
  out <- dplyr::bind_rows(out)

  return(out)
}
