pip <- function(country   = "all",
                povline   = NULL,
                popshare  = NULL,
                year      = "all",
                fill_gaps = FALSE,
                aggregate = FALSE,
                welfare_type = "all",
                svy_coverage = "all",
                ppp       = NULL,
                server    = NULL,
                lkup,
                paths) {


  # Handle interpolation
  if (fill_gaps == TRUE) {

    out <- fg_pip(country      = country,
                  povline      = povline,
                  popshare     = popshare,
                  year         = year,
                  aggregate    = aggregate,
                  welfare_type = welfare_type,
                  svy_coverage = svy_coverage,
                  ppp          = ppp,
                  server       = server,
                  lkup         = lkup,
                  paths        = paths)
  } else {
    out <- rg_pip(country      = country,
                  povline      = povline,
                  popshare     = popshare,
                  year         = year,
                  aggregate    = aggregate,
                  welfare_type = welfare_type,
                  svy_coverage = svy_coverage,
                  ppp          = ppp,
                  server       = server,
                  lkup         = lkup,
                  paths        = paths)
  }

  # Handle aggregated distributions
  if (svy_coverage %in% c("national", "all")) {
    aggregated <- out[out$is_aggregated == TRUE, ]
    if (nrow(aggregated) > 0) {
      aggregated <- out[out$is_aggregated == TRUE, ]
      aggregated_list <- split(aggregated,
                               interaction(aggregated$country_code,
                                           aggregated$reporting_year),
                               drop = TRUE)
      aggregated <- lapply(aggregated_list, ag_average_poverty_stats)
      aggregated <- dplyr::bind_rows(aggregated)

      out <- dplyr::bind_rows(out, aggregated)
    }
    if (svy_coverage == "national") {
      out <- out[out$pop_data_level == "national", ]
    }
  }

  # Add pre-computed distributional statistics
  dist_stats <- dplyr::select(lkup$dist_stats,
                              survey_id,
                              country_code,
                              reporting_year,
                              welfare_type,
                              pop_data_level,
                              median = survey_median_ppp,
                              gini,
                              polarization,
                              mld,
                              dplyr::starts_with("decile"))

  out <- dplyr::left_join(out, dist_stats,
                          by = c("survey_id",
                                 "country_code",
                                 "reporting_year",
                                 "welfare_type",
                                 "pop_data_level"))

  # ADD FIX FOR MEDIAN WHEN INTERPOLATING
  # median <- dist_stats[["median"]]/(data_mean/requested_mean)

  return(out)
}


