pip <- function(country   = "all",
                povline   = NULL,
                popshare  = NULL,
                year      = "all",
                fill_gaps = FALSE,
                aggregate = FALSE,
                group_by  = NULL,
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
    out <- add_agg_stats(out)
  }


  # Handle grouped aggregations
  if (!is.null(group_by)) {
    pop_rgn <- lkups[["pop_region"]]
    # Handle potential (insignificant) difference in poverty_line values  that
    # may mess-up the grouping
    out$poverty_line <- povline

    # out2 <- dplyr::filter(out,
    #                       is_used_for_aggregation == TRUE)
    out2 <- dplyr::select(out,
                          pcn_region_code,
                          reporting_year,
                          poverty_line,
                          # mean,
                          headcount,
                          poverty_gap,
                          poverty_severity,
                          watts,
                          reporting_pop)

    # Compute group aggregates
    out2 <- dplyr::group_by(out2, pcn_region_code, reporting_year, poverty_line)
    out2 <- dplyr::summarise(out2,
                             # mean              = weighted.mean(x = mean, w = reporting_pop, na.rm = TRUE),
                             headcount         = weighted.mean(x = headcount, w = reporting_pop, na.rm = TRUE),
                             poverty_gap       = weighted.mean(x = poverty_gap, w = reporting_pop, na.rm = TRUE),
                             poverty_severity  = weighted.mean(x = poverty_severity, w = reporting_pop, na.rm = TRUE),
                             watts             = weighted.mean(x = watts, w = reporting_pop, na.rm = TRUE))

    # Compute world aggregates
    out3 <- dplyr::left_join(out2, pop_rgn)

    out3 <- dplyr::group_by(out3,
                            reporting_year, poverty_line)

    out3 <- dplyr::summarise(out3,
                             # mean              = weighted.mean(x = mean, w = pop, na.rm = TRUE),
                             headcount         = weighted.mean(x = headcount, w = pop, na.rm = TRUE),
                             poverty_gap       = weighted.mean(x = poverty_gap, w = pop, na.rm = TRUE),
                             poverty_severity  = weighted.mean(x = poverty_severity, w = pop, na.rm = TRUE),
                             watts             = weighted.mean(x = watts, w = pop, na.rm = TRUE))

    out3$pcn_region_code <- "WLD"

    # Combine
    out <- dplyr::bind_rows(out2, out3)

    return(out)
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

  # Handle survey coverage
  if (svy_coverage != "all") {
    out <- out[out$pop_data_level == svy_coverage, ]
  }

  # ADD FIX FOR MEDIAN WHEN INTERPOLATING
  # median <- dist_stats[["median"]]/(data_mean/requested_mean)

  return(out)
}


