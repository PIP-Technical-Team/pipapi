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
                               lkup = lkup[["svy_lkup"]])

  # return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(pipapi::empty_response)
  }

  out <- vector(mode = "list", length = nrow(metadata))

  for (i in seq_along(out)) {

    tmp_metadata <- metadata[i, ]

    svy_data <- get_svy_data(metadata$survey_id[[i]],
                             paths = paths)

    tmp_stats <- wbpip::compute_pip_stats(welfare = svy_data$df0$welfare,
                                          povline = povline,
                                          population = svy_data$df0$weight,
                                          requested_mean = tmp_metadata$survey_mean_ppp,
                                          default_ppp = tmp_metadata$ppp,
                                          ppp = ppp,
                                          distribution_type = tmp_metadata$distribution_type)

    tmp_deciles <- tmp_stats$deciles
    tmp_stats$deciles <- NULL
    # Add stats columns to data frame
    for (j in seq_along(tmp_stats)) {
      tmp_metadata[[names(tmp_stats)[j]]] <- tmp_stats[[j]]
    }
    # Add deciles columns to data frame
      names_deciles <- paste0("decile", seq_along(tmp_deciles))
      for (k in seq_along(names_deciles)) {
        tmp_metadata[[names_deciles[k]]] <- tmp_deciles[k]
      }

    out[[i]] <- tmp_metadata
  }
  out <- dplyr::bind_rows(out)
  return(out)
}