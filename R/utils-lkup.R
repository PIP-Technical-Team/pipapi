# utils-lkup.R
#
# Lookup-table filtering helpers used by pip_new_lineups(), fg_pip(),
# rg_pip(), and their old-pathway counterparts.
#
# Functions:
#   subset_lkup()          - top-level filter: wraps lkup_filter + cache check
#   lkup_filter()          - applies all filter steps in sequence
#   select_country()       - step 2: filter by country / region
#   select_years()         - step 3: filter by year (including MRV)
#   filter_lkup()          - drop aggregate distributions when popshare is set
#   select_reporting_level()- step 5: filter by reporting level
#   subset_ctry_years()    - filter the country-years interpolation table

#' Subset look-up data
#' @inheritParams pip
#' @inheritParams rg_pip
#' @param valid_regions character: List of valid region codes that can be used
#'   for region selection
#' @param data_dir character: directory path from lkup$data_root
#' @param cache_file_path file path for cache
#' @return data.frame
#' @keywords internal
subset_lkup <- function(
  country,
  year,
  welfare_type,
  reporting_level,
  lkup,
  valid_regions,
  data_dir = NULL,
  povline,
  cache_file_path,
  fill_gaps,
  popshare = NULL
) {
  lkup <- lkup_filter(
    lkup,
    country,
    year,
    valid_regions,
    reporting_level,
    welfare_type,
    data_dir
  )
  # If povline is NULL, this happens when popshare is passed
  # i.e popshare is not NULL
  if (is.null(povline)) {
    return(list(
      data_present_in_master = NULL,
      lkup = lkup,
      povline = NULL
    ))
  }
  # Return with grace
  return_if_exists(
    slkup = lkup,
    povline = povline,
    cache_file_path = cache_file_path,
    fill_gaps = fill_gaps
  )
}


#' Filter a survey lookup table by country, year, welfare type, and reporting level
#'
#' @keywords internal
#' @param country character: Country ISO3 codes or "ALL"
#' @param year integer or character: Reporting year(s), "ALL", or "MRV"
#' @param valid_regions character: Valid region codes from `lkup$query_controls`
#' @param reporting_level character: Requested reporting level
#' @param welfare_type character: Requested welfare type
#' @param data_dir character: Path to main data directory (`lkup$data_root`)
lkup_filter <- function(
  lkup,
  country,
  year,
  valid_regions,
  reporting_level,
  welfare_type,
  data_dir
) {
  # STEP 1 - Keep every row by default
  keep <- rep(TRUE, nrow(lkup))
  # STEP 2 - Select countries
  keep <- select_country(lkup, keep, country, valid_regions)
  # STEP 3 - Select years
  keep <- select_years(
    lkup = lkup,
    keep = keep,
    year = year,
    country = country,
    data_dir = data_dir,
    valid_regions = valid_regions
  )

  # STEP 4 - Select welfare_type
  if (welfare_type[1] != "all") {
    keep <- keep & lkup$welfare_type == welfare_type
  }
  # STEP 5 - Select reporting_level
  keep <- select_reporting_level(
    lkup = lkup,
    keep = keep,
    reporting_level = reporting_level[1]
  )

  lkup <- lkup[keep, ]
  return(lkup)
}


#' Select country rows from a lookup table
#'
#' Helper function for \code{subset_lkup()}.
#'
#' @inheritParams subset_lkup
#' @param keep logical vector: current row selection mask
#' @return logical vector
#' @keywords internal
select_country <- function(lkup, keep, country, valid_regions) {
  # Select data files based on requested country, year, etc.
  # Select countries
  if (!any(c("ALL", "WLD") %in% toupper(country))) {
    # Select regions
    if (any(country %in% valid_regions)) {
      selected_regions <- country[country %in% valid_regions]
      # Find all columns ending with _code
      code_cols <- grep("_code$", names(lkup), value = TRUE)
      code_cols <- code_cols[!code_cols %in% "wb_region_code"] # TODO: remove exclusion when wb_region_code is handled upstream
      # For each code column, check if any value matches selected_regions
      keep_regions_list <- lapply(code_cols, \(col) {
        lkup[[col]] %in% selected_regions
      })
      # Combine with logical OR across all code columns
      if (length(keep_regions_list) > 0) {
        keep_regions <- Reduce(`|`, keep_regions_list)
      } else {
        keep_regions <- rep(FALSE, nrow(lkup))
      }
    } else {
      keep_regions <- rep(FALSE, length(lkup$country_code))
    }
    keep_countries <- lkup$country_code %in% country
    keep <- keep & (keep_countries | keep_regions)
  }
  return(keep)
}


#' Select year rows from a lookup table
#'
#' Helper function for \code{subset_lkup()}.
#'
#' @inheritParams subset_lkup
#' @param keep logical vector: current row selection mask
#' @return logical vector
#' @keywords internal
select_years <- function(
  lkup,
  keep,
  year,
  country,
  data_dir,
  valid_regions = NULL
) {
  caller_names <- get_caller_names()
  is_agg <-
    grepl("pip_grp", caller_names) |>
    any()

  dtmp <- lkup

  year <- toupper(year)
  country <- toupper(country)
  keep_years <- rep(TRUE, nrow(dtmp))

  has_region <- FALSE
  has_country <- TRUE
  has_all <- "ALL" %in% country

  if (!is.null(valid_regions)) {
    if (any(country %in% valid_regions[!valid_regions %in% "ALL"])) {
      has_region <- TRUE
    }
    if (all(country %in% valid_regions[!valid_regions %in% "ALL"])) {
      has_country <- FALSE
    }
  }

  # STEP 1 - If Most Recent Value requested
  if ("MRV" %in% year) {
    # for MRV, countries and regions not allowed
    if (has_country && has_region) {
      rlang::abort(
        "country codes and region codes not allowed with MRV in year"
      )
    }
    # STEP 1.1 - If all countries selected. Select MRV for each country

    if (has_region || is_agg) {
      mr <- get_metaregion_table(data_dir)
      dtmp[mr, on = "region_code", max_year := reporting_year == i.lineup_year]

      if (isFALSE(has_all)) {
        dtmp[!region_code %in% country, max_year := FALSE]
      }
    } else {
      # STEP 1.2 - If only some countries selected. Select MRV for each selected
      # country
      if (has_all) {
        dtmp[,
          max_year := reporting_year == max(reporting_year),
          by = country_code
        ]
      } else {
        dtmp[
          country_code %in% country | region_code %in% country,
          max_year := reporting_year == max(reporting_year),
          by = country_code
        ]
      }
    }

    dtmp[is.na(max_year), max_year := FALSE]

    keep_years <- keep_years & as.logical(dtmp[["max_year"]])
  }
  # STEP 2 - If specific years are specified. Filter for these years
  if (!any(c("ALL", "MRV") %in% year)) {
    keep_years <- keep_years & dtmp$reporting_year %in% as.numeric(year)
  }

  # STEP 3 - Otherwise return all years
  keep <- keep & keep_years
  return(keep)
}


#' Filter aggregate distributions when popshare is active
#'
#' The popshare option is not supported for aggregate distributions.
#'
#' @param metadata data.frame: Output of \code{subset_lkup()}
#' @param popshare numeric: popshare value passed to \code{pip()}
#'
#' @return data.frame
#' @keywords internal
#'
#' TODO: Remove this function when popshare is fully supported for all
#' distributions.
filter_lkup <- function(metadata, popshare) {
  # popshare option not supported for aggregate distributions
  if (!is.null(popshare)) {
    return(
      metadata[metadata$distribution_type != "aggregate", ]
    )
  } else {
    return(metadata)
  }
}


#' Filter lookup table rows by reporting level
#'
#' @param lkup data.table: Main lookup table
#' @param keep logical: Logical vector of rows to be kept
#' @param reporting_level character: Requested reporting level
#'
#' @return data.table
#' @export
select_reporting_level <- function(lkup, keep, reporting_level) {
  # To be updated: Fix the coverage variable names in aux data (reporting_coverage?)
  if (reporting_level == "all") {
    return(keep)
  } else if (reporting_level == "national") {
    # Subnational levels necessary to compute national stats for aggregate distributions
    keep <- keep &
      (lkup$reporting_level == reporting_level | lkup$is_used_for_aggregation)
    return(keep)
  } else {
    if ("survey_coverage" %in% names(lkup)) {
      keep <- keep &
        (lkup$survey_coverage == reporting_level |
          lkup$reporting_level == reporting_level)
    } else {
      # This condition is not triggered
      keep <- keep & lkup$reporting_level == reporting_level
    }
    return(keep)
  }
}


#' Subset country-years interpolation table
#'
#' Filters the country-years table (part of the interpolated_list object)
#' created at start time to facilitate imputations.
#'
#' @param valid_regions character: List of valid region codes
#' @inheritParams subset_lkup
#' @return data.frame
#' @keywords internal
subset_ctry_years <- function(country, year, lkup, valid_regions, data_dir) {
  is_agg <- get_caller_names()
  is_agg <- grepl(pattern = "pip_grp", x = is_agg) |>
    any()

  keep <- TRUE
  # Select data files based on requested country, year, etc.
  # Select countries
  country_or_region <- "country_code"
  if (!any(c("ALL", "WLD") %in% country)) {
    # Select regions
    if (any(country %in% valid_regions)) {
      selected_regions <- country[country %in% valid_regions]
      keep_regions <- lkup$region_code %in% selected_regions
      country_or_region <- "region_code"
    } else {
      keep_regions <- rep(FALSE, length(lkup$region_code))
    }
    keep_countries <- lkup$country_code %chin% as.character(country)
    keep <- keep & (keep_countries | keep_regions)
  }

  # Select years
  if (year[1] == "MRV") {
    if (is_agg) {
      mr <- get_metaregion_table(data_dir)
      lkup[mr, on = "region_code", lineup_year := i.lineup_year]
    } else {
      lkup[, lineup_year := reporting_year]
    }

    if (country[1] != "ALL") {
      max_year <-
        lkup[
          get(country_or_region) == country & reporting_year == lineup_year,
          reporting_year
        ] |>
        max()
    } else {
      max_year <-
        lkup[reporting_year == lineup_year, reporting_year] |>
        max()
    }
    keep <- keep & lkup$reporting_year %in% max_year
  }

  if (!year[1] %in% c("ALL", "MRV")) {
    keep <- keep & lkup$reporting_year %in% as.numeric(year)
  }

  lkup <- as.data.frame(lkup)
  lkup <- lkup[keep, ]

  return(lkup)
}
