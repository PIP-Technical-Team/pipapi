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
                               fill_gaps = fill_gaps,
                               lkup = lkup[["ref_lkup"]])
  # return empty dataframe if no metadata is found
  if (is.na(metadata)) {
    return(create_empty_response())
  }

  out <- vector(mode = "list", length = nrow(metadata))

  for (i in seq_along(out)) {

    tmp_metadata <- metadata[i, ]

    svy_data <- get_svy_data(metadata$survey_id[[i]],
                             paths = paths)

    tmp_stats <- wbpip:::fg_compute_pip_stats(request_year = year,
                                              data = svy_data,
                                              predicted_request_mean = metadata[["predicted_mean_ppp"]],
                                              survey_year = metadata[["survey_year"]],
                                              default_ppp = metadata[["ppp"]],
                                              ppp = ppp,
                                              distribution_type = metadata[["distribution_type"]],
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
