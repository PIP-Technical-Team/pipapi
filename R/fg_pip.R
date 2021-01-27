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
  if (is.na(metadata)) {
    return(create_empty_response())
  }
  # Extract unique combinations of country-year
  ctry_years = unique(metadata[, c("country_code", "reference_year")])

  out <- vector(mode = "list", length = nrow(ctry_years))

  for (i in seq_along(out)) {

    ctry_year <- ctry_years[i, , drop = FALSE]

    tmp_metadata <- dplyr::left_join(ctry_year, metadata)

    svy_data <- get_svy_data(tmp_metadata$survey_id,
                             paths = paths)

    tmp_stats <- wbpip:::fg_compute_pip_stats(request_year = year,
                                              data = svy_data,
                                              predicted_request_mean = tmp_metadata[["predicted_mean_ppp"]],
                                              survey_year = tmp_metadata[["survey_year"]],
                                              default_ppp = tmp_metadata[["ppp"]],
                                              ppp = ppp,
                                              distribution_type = tmp_metadata[["distribution_type"]],
                                              poverty_line = povline)

    tmp_deciles <- tmp_stats$deciles
    tmp_stats$deciles <- NULL
    tmp_out <- cbind(tmp_metadata, tmp_stats)
    names_deciles <- paste0("decile", seq_along(tmp_deciles))
    for (j in seq_along(names_deciles)) {
      tmp_out[[names_deciles[j]]] <- NA
    }

    tmp_out <- dplyr::rename(tmp_out, poverty_rate = headcount)

    out[[i]] <- tmp_out
  }
  out <- dplyr::bind_rows(out)

  return(out)
}
