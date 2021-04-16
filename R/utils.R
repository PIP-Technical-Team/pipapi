subset_lkup <- function(country,
                        year,
                        welfare_type,
                        svy_coverage,
                        lkup) {

  svy_n <- nrow(lkup)
  keep <- rep(TRUE, svy_n)
  # Select data files based on requested country, year, etc.
  # Select countries
  if (country[1] != "all") {
    keep <- keep & lkup$country_code %in% country
  }
  # Select years
  if (year[1] != "all") {
    keep <- keep & lkup$reporting_year %in% year
  }
  # Select welfare_type
  if (welfare_type[1] != "all") {
    keep <- keep & lkup$welfare_type == welfare_type
  }
  # Select survey coverage
  # To be updated: Fix the coverage variable names in aux data (reporting_coverage?)
  if (svy_coverage[1] != "all") {
    if ("survey_coverage" %in% names(lkup)) {
      keep <- keep &
        (lkup$survey_coverage == svy_coverage |
           lkup$pop_data_level  == svy_coverage)
    } else {
      keep <- keep & lkup$pop_data_level  == svy_coverage
    }
  }

  lkup <- lkup[keep, ]

  return(lkup)
}

get_svy_data <- function(svy_id,
                         svy_coverage,
                         paths)
{
  # Each call should be made at a unique pop_data_level (equivalent to reporting_data_level: national, urban, rural)
  svy_coverage <- unique(svy_coverage)
  assertthat::assert_that(length(svy_coverage) == 1,
                          msg = "Problem with input data: Multiple pop_data_levels")

  out <- purrr::map(svy_id, function(id) {
    path <- paths[stringr::str_detect(paths, id)]
    tmp <- fst::read_fst(path)
    if (svy_coverage %in% c("urban", "rural")) { # Not robust. Should not be hard coded here.
      tmp <- tmp[tmp$area == svy_coverage, ]
    }
    tmp <- tmp[, c("welfare", "weight")]

    return(tmp)
  })

  names_out <- paste0("df", (seq_along(svy_id) - 1))
  names(out) <- names_out

  return(out)
}


create_empty_response <- function() {
  out <- data.frame(
           survey_id = c(NA),
         region_code = c(NA),
        country_code = c(NA),
      reference_year = c(NA),
       surveyid_year = c(NA),
      reporting_year = c(NA),
      survey_acronym = c(NA),
     survey_coverage = c(NA),
         survey_year = c(NA),
        welfare_type = c(NA),
     survey_mean_ppp = c(NA),
  predicted_mean_ppp = c(NA),
                 ppp = c(NA),
       reference_pop = c(NA),
       reference_gdp = c(NA),
       reference_pce = c(NA),
      pop_data_level = c(NA),
      gdp_data_level = c(NA),
      pce_data_level = c(NA),
      cpi_data_level = c(NA),
      ppp_data_level = c(NA),
   distribution_type = c(NA),
             gd_type = c(NA),
        poverty_line = c(NA),
                mean = c(NA),
              median = c(NA),
        poverty_rate = c(NA),
         poverty_gap = c(NA),
    poverty_severity = c(NA),
               watts = c(NA),
                gini = c(NA),
                 mld = c(NA),
        polarization = c(NA),
             decile1 = c(NA),
             decile2 = c(NA),
             decile3 = c(NA),
             decile4 = c(NA),
             decile5 = c(NA),
             decile6 = c(NA),
             decile7 = c(NA),
             decile8 = c(NA),
             decile9 = c(NA),
            decile10 = c(NA),
     is_interpolated = c(NA)
  )

  return(out)
}

collapse_rows <- function(df, vars, na_var) {
  tmp_vars <- lapply(df[, ..vars], unique, collapse = "|")
  tmp_vars <- lapply(tmp_vars, paste, collapse = "|")
  tmp_var_names <- names(df[, ..vars])
  df[[na_var]] <- NA_real_
  for (tmp_var in seq_along(tmp_vars)) {
    df[[tmp_var_names[tmp_var]]] <- tmp_vars[[tmp_var]]
  }
  df <- unique(df)
}

add_dist_stats <- function(df, dist_stats) {
  cols <- c("survey_id",
            "country_code",
            "reporting_year",
            "welfare_type",
            "pop_data_level",
            "survey_median_ppp",
            "gini",
            "polarization",
            "mld",
            paste0("decile", 1:10))
  dist_stats <- dist_stats[, ..cols]

  data.table::setnames(dist_stats, "survey_median_ppp", "median")

  df <- dist_stats[df,
                    on = .(survey_id, country_code, reporting_year, welfare_type, pop_data_level),
                    allow.cartesian = TRUE]

  return(df)
}

