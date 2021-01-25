rg_pip <- function(country   = "all",
                   povline   = NULL,
                   popshare  = NULL,
                   year      = "all",
                   aggregate = FALSE,
                   welfare_type = "all",
                   svy_coverage = "all",
                   ppp       = NULL,
                   server    = NULL,
                   lkup,
                   paths) {

  metadata <- get_svy_metadata(country = country,
                               year = year,
                               welfare_type = welfare_type,
                               svy_coverage = svy_coverage,
                               fill_gaps = fill_gaps,
                               lkup = lkup[["svy_lkup"]])

  # return empty dataframe if no metadata is found
  if (is.na(metadata)) {
    return(create_empty_response())
  }

  out <- vector(mode = "list", length = nrow(metadata))

  for (i in seq_along(out)) {

    tmp_metadata <- metadata[i, ]

    svy_data <- get_svy_data(metadata$survey_id[[i]],
                             paths = paths)

    tmp_stats <- wbpip::compute_pip_stats(welfare = svy_data$df0$welfare,
                                          povline = povline,
                                          population = svy_data$df0$weight,
                                          requested_mean = metadata$dsm_mean[[i]],
                                          default_ppp = metadata$ppp[[i]],
                                          distribution_type = metadata$distribution_type[[i]])

    tmp_deciles <- tmp_stats$deciles
    tmp_stats$deciles <- NULL
    tmp_out <- cbind(tmp_metadata, tmp_stats)

      names_deciles <- paste0("decile", seq_along(tmp_deciles))
      for (j in seq_along(names_deciles)) {
        tmp_out[[names_deciles[j]]] <- tmp_deciles[j]
      }

    # tmp_out$pop_in_poverty <- tmp_out$headcount * tmp_out$survey_pop

    tmp_out <- dplyr::rename(tmp_out, poverty_rate = headcount)
    # tmp_out <- dplyr::relocate(tmp_out, pop_in_poverty, .after = poverty_rate)

    out[[i]] <- tmp_out
  }
  out <- dplyr::bind_rows(out)
  return(out)
}
