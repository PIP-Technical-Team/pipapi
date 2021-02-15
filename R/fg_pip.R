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

  metadata <- get_svy_metadata(country = country,
                               year = year,
                               welfare_type = welfare_type,
                               svy_coverage = svy_coverage,
                               lkup = lkup[["ref_lkup"]])
  # return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(pipapi::empty_response)
  }
  # Extract unique combinations of country-year
  ctry_years = unique(metadata[, c("country_code", "reporting_year")])

  out <- vector(mode = "list", length = nrow(ctry_years))

  for (i in seq_along(out)) {

    ctry_year <- ctry_years[i, , drop = FALSE]
    tmp_year  <- ctry_year[["reporting_year"]]

    tmp_metadata <- dplyr::left_join(ctry_year, metadata,
                                     by = c("country_code", "reporting_year"))

    svy_data <- get_svy_data(tmp_metadata$survey_id,
                             paths = paths)

    tmp_stats <- wbpip:::fg_compute_pip_stats(request_year = tmp_year,
                                              data = svy_data,
                                              predicted_request_mean = tmp_metadata[["predicted_mean_ppp"]],
                                              survey_year = tmp_metadata[["survey_year"]],
                                              default_ppp = tmp_metadata[["ppp"]],
                                              ppp = ppp,
                                              distribution_type = tmp_metadata[["distribution_type"]],
                                              poverty_line = povline)

    tmp_deciles <- tmp_stats$deciles
    tmp_stats$deciles <- NULL
    # Add stats columns to data frame
    for (j in seq_along(tmp_stats)) {
      tmp_metadata[[names(tmp_stats)[j]]] <- tmp_stats[[j]]
    }


    if (length(tmp_deciles) < 10) {
      names_deciles <- paste0("decile", 1:10)
      for (k in seq_along(names_deciles)) {
        tmp_metadata[[names_deciles[k]]] <- NA
      }
    } else {
      names_deciles <- paste0("decile", seq_along(tmp_deciles))
      for (k in seq_along(names_deciles)) {
        tmp_metadata[[names_deciles[k]]] <- tmp_deciles[k]
      }
    }

    out[[i]] <- tmp_metadata
  }
  out <- dplyr::bind_rows(out)

  return(out)
}
