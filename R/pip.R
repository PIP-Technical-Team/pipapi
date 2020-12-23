pip <- function(country   = "all",
                povline   = NULL,
                popshare  = NULL,
                year      = "all",
                fill_gaps = FALSE,
                aggregate = FALSE,
                welfare_type = NULL,
                by        = NULL,
                ppp       = NULL,
                server    = NULL,
                format    = "csv",
                lkup,
                paths) {

  metadata <- get_svy_metadata(country = country,
                               year = year,
                               welfare_type = welfare_type,
                               fill_gaps = FALSE,
                               lkup = lkup)

  svy_data <- get_svy_data(metadata$survey_id,
                           paths = paths)

  out <- wbpip::compute_pip_stats(welfare = svy_data$welfare,
                                  povline = povline,
                                  population = svy_data$weight,
                                  requested_mean = metadata$dsm_mean,
                                  distribution_type = metadata$distribution_type)

  return(
    list(
      metadata = metadata,
      stats    = out
    )
  )





}


get_svy_metadata <- function(country   = NULL,
                             year      = NULL,
                             welfare_type = NULL,
                             survey_coverage = NULL,
                             fill_gaps = FALSE,
                             lkup      = NULL) {

  # Select data files based on requested country, year, etc.
  keep <- lkup$country_code == country & lkup$reporting_year == year
  if (!is.null(welfare_type)) {
    keep <- keep & lkup$welfare_type == welfare_type
  }
  if (!is.null(survey_coverage)) {
    keep <- keep & lkup$survey_coverage == survey_coverage
  }
  lkup <- lkup[keep, ]
  # Handle cases when no survey is found
  if (nrow(lkup) == 0) {
    return(NA)
  } else {
    return(lkup)
  }

}

get_svy_data <- function(svy_id,
                         paths)
{


  path <- paths[stringr::str_detect(paths, svy_id)]
  # Load correct data file
  out <- haven::read_stata(path)
  out <- out[, c("welfare", "weight")]

  return(out)

}


