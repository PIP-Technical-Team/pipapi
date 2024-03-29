utils::globalVariables(
  c(
    ".", "cache_id", "country_code", "cpi", "decile1",
    "decile10", "decile2", "decile3", "decile4",
    "decile5", "decile6", "decile7", "decile8",
    "decile9", "distribution_type", "gini",
    "headcount", "interpolation_id",
    "is_interpolated", "median", "mld",
    "polarization", "pop", "reporting_level",
    "pop_in_poverty", "poverty_gap",
    "poverty_line", "poverty_severity",
    "ppp", "region_code", "reporting_pop",
    "reporting_year", "survey_comparability",
    "survey_coverage", "survey_mean_lcu",
    "survey_mean_ppp", "survey_year", "watts",
    "wb_region_code", "weighted.mean",
    "welfare_type", "pcn_region_code",
    "comparable_spell","..cols", "N", "check",
    "data_interpolation_id", "display_cp", "region_name",
    "sessionInfo"
  )
)


#' Subset look-up data
#' @inheritParams pip
#' @param valid_regions character: List of valid region codes that can be used
#' for region selection
#' @return data.frame
#' @keywords internal
subset_lkup <- function(country,
                        year,
                        welfare_type,
                        reporting_level,
                        lkup,
                        valid_regions) {

  # STEP 1 - Keep every row by default
  keep <- rep(TRUE, nrow(lkup))
  # STEP 2 - Select countries
  keep <- select_country(lkup, keep, country, valid_regions)
  # STEP 3 - Select years
  keep <- select_years(lkup, keep, year, country)
  # STEP 4 - Select welfare_type
  if (welfare_type[1] != "all") {
    keep <- keep & lkup$welfare_type == welfare_type
  }
  # STEP 5 - Select reporting_level
  keep <- select_reporting_level(lkup = lkup,
                                 keep = keep,
                                 reporting_level = reporting_level[1])

  lkup <- lkup[keep, ]

  return(lkup)
}

#' Helper to filter metadata
#' aggregate distribution need to be filtered out when popshare is not null
#' This is a temporary function until a full fix is implemented, and popshare is
#' supported for all distributions
#'
#' @param metadata data.frame: Output of `subset_lkup()`
#' @param popshare numeric: popshare value passed to `pip()`
#'
#' @return data.frame

filter_lkup <- function(metadata,
                        popshare) {
  # popshare option not supported for aggregate distributions
  if (!is.null(popshare)) {
    return(
      metadata[metadata$distribution_type != "aggregate", ]
    )
  } else {
    return(metadata)
  }

}


#' helper function to correctly filter look up table according to requested
#' reporting level
#'
#' @param lkup data.table: Main lookup table
#' @param keep logical: Logical vector of rows to be kept
#' @param reporting_level character: Requested reporting level
#'
#' @return data.table
#' @export
#'
select_reporting_level <- function(lkup,
                                   keep,
                                   reporting_level) {
  # To be updated: Fix the coverage variable names in aux data (reporting_coverage?)
  if (reporting_level == "all") {
    return(keep)

  } else if (reporting_level == "national") {
    # Subnational levels necessary to compute national stats for aggregate distributions
    keep <- keep & (lkup$reporting_level == reporting_level | lkup$is_used_for_aggregation)
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


#' Read survey data
#'
#' @param svy_id character: Survey ID
#' @param reporting_level character: geographical reporting level
#' @param path character: Path to survey data
#'
#' @return data.frame
#' @keywords internal
get_svy_data <- function(svy_id,
                         reporting_level,
                         path) {
  # Each call should be made at a unique reporting_level (equivalent to reporting_data_level: national, urban, rural)
  # This check should be conducted at the data validation stage
  reporting_level <- unique(reporting_level)
  assertthat::assert_that(length(reporting_level) == 1,
                          msg = "Problem with input data: Multiple reporting_levels"
  )
  # tictoc::tic("read_single")
  out <- lapply(path, function(x) {

    if (reporting_level %in% c("urban", "rural")) { # Not robust. Should not be hard coded here.
      tmp <- fst::read_fst(x)
      tmp <- tmp[tmp$area == reporting_level, ]
      tmp <- tmp[, c("welfare", "weight")]
    } else {
      tmp <- fst::read_fst(x, columns = c("welfare", "weight"))
    }

    return(tmp)
  })

  # Logging
  # end_read_single <- tictoc::toc(quiet = TRUE)
  # logger::log_info('read_single: {svy_id} {round(end_read_single$toc - end_read_single$tic, digits = getOption("digits", 6))}')

  names_out <- sprintf(
    "df%s",
    seq_along(svy_id) - 1
  )
  names(out) <- names_out

  return(out)
}


#' Add pre-computed distributional stats
#'
#' @param df data.table: Data frame of poverty statistics
#' @param dist_stats data.table: Distributional stats lookup
#'
#' @return data.table
#' @export
#'
add_dist_stats <- function(df, dist_stats) {
  # Keep only relevant columns
  cols <- c(
    "cache_id",
    # "country_code",
    # "reporting_year",
    # "welfare_type",
    "reporting_level",
    "gini",
    "polarization",
    "mld",
    sprintf("decile%s", 1:10)
  )
  dist_stats <- dist_stats[, .SD, .SDcols = cols]

  # merge dist stats with main table
  # data.table::setnames(dist_stats, "survey_median_ppp", "median")

  df <- dist_stats[df,
                   on = .(cache_id, reporting_level), #.(country_code, reporting_year, welfare_type, reporting_level),
                   allow.cartesian = TRUE
  ]

  return(df)
}

#' Collapse rows
#' @return data.table
#' @noRd
collapse_rows <- function(df, vars, na_var = NULL) {
  tmp_vars      <- lapply(df[, .SD, .SDcols = vars], unique, collapse = "|")
  tmp_vars      <- lapply(tmp_vars, paste, collapse = "|")
  tmp_var_names <- names(df[, .SD, .SDcols = vars])

  if (!is.null(na_var)) df[[na_var]] <- NA_real_

  for (tmp_var in seq_along(tmp_vars)) {
    df[[tmp_var_names[tmp_var]]] <- tmp_vars[[tmp_var]]
  }

  df <- unique(df)
  return(df)
}

#' Censor rows
#' Censor statistics based on a pre-defined censor table.
#' @param df data.table: Table to censor. Output from `pip()`.
#' @param censored list: List with censor tables.
#' @param type character: Type of censor table to use. Either countries or regions.
#' @return data.table
#' @noRd
censor_rows <- function(df, censored, type = c("countries", "regions")) {

  type <- match.arg(type)

  # Return early if there are no censoring observations
  # if (nrow(censored[[type]]) == 0) {
  #   return(df)
  # }

  # Create tmp_id to match with censor table
  if (type == "countries") {
    df$tmp_id <-
      sprintf(
        "%s_%s_%s_%s_%s",
        df$country_code, df$reporting_year,
        df$survey_acronym, df$welfare_type,
        df$reporting_level
      )
  } else {
    df$tmp_id <-
      sprintf(
        "%s_%s",
        df$region_code, df$reporting_year
      )
  }

  # Apply censoring
  out <- censor_stats(df, censored[[type]])
  out$tmp_id <- NULL

  return(out)
}

#' Censor stats
#' @param df data.table: Table to censor.
#' @param censored_table data.table: Censor table
#' @noRd
censor_stats <- function(df, censored_table) {

  df$to_remove <- FALSE
  if (any(df$tmp_id %in% censored_table$id)) {
    for (i in seq_len(nrow(df))) {
      for (y in seq_len(nrow(censored_table))) {
        if (df$tmp_id[i] == censored_table$id[y]) {
          # Remove entire row if all statistics should be removed
          if (censored_table$statistic[y] == "all") {
            df$to_remove[i] <- TRUE
          } else {
            # Otherwise set specific stats to NA
            df[[censored_table$statistic[y]]][i] <- NA_real_
          }
        }
      }
    }
  }
  df <- df[!df$to_remove]
  df$to_remove <- NULL

  return(df)
}

#' Create query controls
#' @param syv_lkup data.table: Survey lkup table
#' @param ref_lkup data.table: Reference lkup table
#' @param aux_files data.table: All valid regions and corresponding population
#' @param aux_tables character: List of available aux tables
#' @param versions character: List of available data versions
#' @return list
#' @noRd
create_query_controls <- function(svy_lkup,
                                  ref_lkup,
                                  aux_files,
                                  aux_tables,
                                  versions) {
  # Countries and regions
  countries <- unique(c(
      svy_lkup$country_code,
      ref_lkup$country_code
    ))

  regions <- unique(c(
    aux_files$regions$region_code
  ))

  country <- list(
    values = c(
      "ALL",
      sort(c(
        countries,
        regions)
      )
    ),
    type = "character"
  )

  region <- list(
    values = sort(c("ALL", regions)),
    type = "character"
  )
  # Year
  year <- list(
    values = c(
      "all", "MRV",
      sort(unique(c(
        svy_lkup$reporting_year,
        ref_lkup$reporting_year
      )))
    ),
    type = "character"
  )
  # Poverty line
  povline <- list(
    values = c(min = 0, max = 2700),
    type = "numeric"
  )
  # Popshare
  popshare <- list(
    values = c(min = 0, max = 1),
    type = "numeric"
  )

  # Boolean parameters
  fill_gaps        <-
    aggregate      <-
    long_format    <-
    additional_ind <-
    list(values = c(TRUE, FALSE),
         type = "logical")

  # Group by
  group_by <- list(
    values = c("none", "wb"),
    type = "character"
  )

  # Welfare type
  welfare_type <- list(
    values = c("all", sort(unique(c(
      svy_lkup$welfare_type,
      ref_lkup$welfare_type
    )))),
    type = "character"
  )
  # Reporting level
  reporting_level <- list(
    values = c(
      "all",
      sort(unique(c(
        svy_lkup$reporting_level,
        ref_lkup$reporting_level
      )))
    ),
    type = "character"
  )
  # PPPs
  ppp <- list(
    values = c(min = 0.05, max = 1000000), # CHECK THE VALUE OF MAX
    type = "numeric"
  )
  # Versions
  version <- list(
    values = versions,
    type = "character"
  )
  # Formats
  format <- list(values = c("json", "csv", "rds", "arrow"),
                 type = "character")
  # Tables
  table <- list(values = aux_tables, type = "character")
  # parameters
  parameter <-
    list(values = c("country", "year", "povline",
                    "popshare", "fill_gaps", "aggregate",
                    "group_by", "welfare_type",
                    "reporting_level", "ppp", "version",
                    "format", "table", "long_format"),
         type = "character")
  # Endpoint
  endpoint <-
    list(values = c("all",
                    "aux",
                    "pip",
                    "pip-grp",
                    "pip-info",
                    "valid-params"),
         type = "character")

    # Create list of query controls
  query_controls <- list(
    country         = country,
    region          = region,
    year            = year,
    povline         = povline,
    popshare        = popshare,
    fill_gaps       = fill_gaps,
    aggregate       = aggregate,
    long_format     = long_format,
    additional_ind  = additional_ind,
    group_by        = group_by,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    ppp             = ppp,
    version         = version,
    format          = format,
    table           = table,
    parameter       = parameter,
    endpoint        = endpoint
  )

  return(query_controls)
}

convert_empty <- function(string) {
  if (string == "") {
    "-"
  } else {
    string
  }
}


#' Subset country-years table
#' This is a table created at start time to facilitate imputations
#' It part of the interpolated_list object
#' @param valid_regions character: List of valid region codes that can be used
#' @return data.frame
#' @keywords internal
subset_ctry_years <- function(country,
                              year,
                              lkup,
                              valid_regions) {

  keep <- TRUE
  # Select data files based on requested country, year, etc.
  # Select countries
  if (!any(c("ALL", "WLD") %in% country)) {
    # Select regions
    if (any(country %in% valid_regions)) {
      selected_regions <- country[country %in% valid_regions]
      keep_regions <- lkup$region_code %in% selected_regions
    } else {
      keep_regions <- rep(FALSE, length(lkup$country_code))
    }
    keep_countries <- lkup$country_code %chin% country
    keep <- keep & (keep_countries | keep_regions)
  }

  # if (!all(country %in% c("all", valid_regions))) {
  #   keep <- keep & lkup$country_code %in% country
  # }

  # Select years
  if (year[1] == "MRV") {
    if (country[1] != "ALL") {
      max_year <- max(lkup[country_code == country]$reporting_year)
    } else {
      max_year <- max(lkup$reporting_year)
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

#' Clear cache
#' Clear cache directory if available
#' @param cd A `cachem::cache_disk()` object
#' @return list
#' @keywords internal
clear_cache <- function(cd) {
  tryCatch({
    if (cd$size() > 0) {
      cd$reset()
      n <- cd$size()
      if (n == 0) {
        out <- list(status = 'success', msg = 'Cache cleared.')
      } else {
        out <- list(status = 'error', msg = sprintf('Something went wrong. %n items remain in cache.', n))
      }
    } else {
      out <- list(status = 'success', msg = 'Cache directory is empty. Nothing to clear.')
    }
    return(out)
  }, error = function(e){
    out <- list(status = 'error', msg = 'Cache directory not found.')
    return(out)
  })
}

#' select_country
#' Helper function for subset_lkup()
#' @inheritParams subset_lkup
#' @param keep logical vector
#' @return logical vector
select_country <- function(lkup, keep, country, valid_regions) {
  # Select data files based on requested country, year, etc.
  # Select countries
  if (!any(c("ALL", "WLD") %in% toupper(country))) {
    # Select regions
    if (any(country %in% valid_regions)) {
      selected_regions <- country[country %in% valid_regions]
      keep_regions <- lkup$region_code %in% selected_regions
    } else {
      keep_regions <- rep(FALSE, length(lkup$country_code))
    }
    keep_countries <- lkup$country_code %in% country
    keep <- keep & (keep_countries | keep_regions)
  }
  return(keep)
}

#' select_years
#' Helper function for subset_lkup()
#' @inheritParams subset_lkup
#' @param keep logical vector
#' @return logical vector
select_years <- function(lkup, keep, year, country) {
  # columns i is an ID that identifies if a country has more than one
  # observation for reporting year. That is the case of IND with URB/RUR and ZWE
  # with interporaltion and microdata info
  # dtmp    <- ref_lkup[,
  #                   .i := seq_len(.N),
  #                   by = .(country_code, reporting_year)]

  dtmp <- lkup

  year       <- toupper(year)
  country    <- toupper(country)
  keep_years <- rep(TRUE, nrow(dtmp))
  # STEP 1 - If Most Recent Value requested
  if ("MRV" %in% year) {
    # STEP 1.1 - If all countries selected. Select MRV for each country
    dtmp <-
    if (any(c("ALL", "WLD") %in% country)) {
       # the i == 1 conditions ensures that it takes into account only one
       # observation  per country per reporting year. This has to bee like
       # that in order to keep the same length as the `keep_years` vector.
      # dtmp[,
      #      max_year := reporting_year == max(reporting_year) & i == 1,
      #      by = country_code]

      dtmp[,
           max_year := reporting_year == max(reporting_year),
           by = country_code]

    } else {
      # STEP 1.2 - If only some countries selected. Select MRV for each selected
      # country
      dtmp[dtmp[["country_code"]] %in% country,
           max_year := reporting_year == max(reporting_year),
           by = country_code]
    }

    # dtmp <- unique(dtmp[, .(country_code, reporting_year, max_year)])


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



#' Test whether a vector is length zero and IS not NULL
#'
#' @param x
#'
#' @return logical. TRUE if x is empty but it is not NULL
#' @export
#'
#' @examples
#' x <- vector()
#' is_empty(x)
#'
#' y <- NULL
#' length(y)
#' is_empty(y)
is_empty <- function(x) {
  if (length(x) == 0 & !is.null(x)) {
    TRUE
  } else {
    FALSE
  }
}




#' Populate list in parent frame
#'
#' Fill in maned objects of a list with the value of named objects in  the
#' parent frame in which the list has been created. This objects must have the
#' same names as the objects of the list
#'
#' @param l list to populate with names objects
#' @param assign logical: whether to assign to parent frame
#'
#' @return invisible list `l` populated with objects of the same frame
#' @export
#'
#' @examples
#' l <- list(x = NULL,
#' y = NULL,
#' z = NULL)
#'
#' x <-  2
#' y <-  "f"
#' z <- TRUE
#' fillin_list(l)
#' l
fillin_list <- function(l,
                        assign = TRUE) {

  #   ____________________________________________________________
  #   Defenses                                    ####
  stopifnot( exprs = {
    is.list(l)
    !is.data.frame(l)
  }
  )

  #   __________________________________________________________________
  #   Early returns                                               ####
  if (FALSE) {
    return()
  }

  #   _______________________________________________________________
  #   Computations                                              ####
  # name of the list in parent frame
  nm_l = deparse(substitute(l))

  #n names of the objects of the list
  nm_obj <- names(l)

  # all the objects in parent frame
  obj_in_parent <- ls(envir = parent.frame())

  # make sure that all the objects in list are in parent frame
  if (!all(nm_obj %in% obj_in_parent)) {

    non_in_parent <-nm_obj[!nm_obj %in% obj_in_parent]

    stop_msg <- paste("The following objects are not in calling function: \n",
                      paste(non_in_parent, collapse = ", "))

    stop(stop_msg)
  }

  val_obj        <- lapply(nm_obj, get, envir = parent.frame())
  names(val_obj) <- nm_obj

  for (i in seq_along(nm_obj)) {
    x <- val_obj[[nm_obj[i]]]
    if (!is_empty(x)) {
      l[[nm_obj[i]]] <- x
    }
  }

  if (assign == TRUE) {
    assign(nm_l, l, envir = parent.frame())
  }

  return(invisible(l))

}

#' Returns all auxiliary tables that support the long_format=TRUE parameter
#' @return character vector
#' @export

get_valid_aux_long_format_tables <- function() {
  c('cpi', 'ppp', 'gdp', 'pce', 'pop')
}


#' load SPR table from aux data
#'
#' If there is no data available, return an empty data.frame
#'
#' @inheritParams get_aux_table
#'
#' @return data.table
get_spr_table <- function(data_dir,
                          table = c("spr_svy", "spr_lnp")) {

  table <- match.arg(table)

  spr <-
    tryCatch(
      expr = {
        # Your code...
        get_aux_table(data_dir = data_dir,
                      table    = table)
      }, # end of expr section
      error = function(e) {
        data.table::data.table(
          country_code    = character(0),
          reporting_year  = numeric(0),
          welfare_type    = character(0),
          reporting_level = character(0),
          spl             = numeric(0),
          spr             = numeric(0),
          median          = numeric(0)
        )
      }
    ) # End of trycatch
  return(spr)
}






#' Add SPL indicators to either fg* or rg PIP output
#'
#' @param df data frame inside [fg_pip] or [rg_pip]
#' @param data_dir character: Directory path of auxiliary data. Usually
#'   `lkup$data_root`
#' @inheritParams pip
#'
#' @return data.table
add_spl <- function(df, fill_gaps, data_dir) {

  if (fill_gaps) {
    spl <-
      get_spr_table(data_dir = data_dir,
                    table = "spr_lnp")

    out <- merge.data.table(
      x = df,
      y = spl,
      by = c(
        "country_code",
        "reporting_year",
        "welfare_type",
        "reporting_level"
      ),
      all.x = TRUE
    )

  } else {
    # Add SPL ------------
    spl <-
      get_spr_table(data_dir = data_dir,
                    table = "spr_svy")

    # Remove median from survey file and use the one from wbpip:::prod_compute_pip_stats
    spl[, median := NULL]

    out <- merge.data.table(
      x = df,
      y = spl,
      by = c(
        "country_code",
        "reporting_year",
        "welfare_type",
        "reporting_level"
      ),
      all.x = TRUE
    )
  }

  return(out)
}





#' Add Aggregate medians
#'
#' @param df data frame from either [fg_pip] or [rg_pip]
#' @param data_dir character: Directory path of auxiliary data. Usually
#'   `lkup$data_root`
#' @inheritParams pip
#'
#' @return data.table
add_agg_medians <- function(df, fill_gaps, data_dir) {

  # Remove Get only obs with median == NA --------
  dtn <- df[is.na(median)]  # NAs
  dtn[, median := NULL]

  dtm <- df[!is.na(median)] # no NAs


  ## early returns -----------
  if (nrow(dtn) == 0) {
    return(df)
  }


  # Get medians from SPL data -----------
  if (fill_gaps) {
    med <-
      get_spr_table(data_dir = data_dir,
                    table    = "spr_lnp")

  } else {
    med <-
      get_spr_table(data_dir = data_dir,
                    table    = "spr_svy")
  }

  med <- med |>
    collapse::get_vars(c(
      "country_code",
      "reporting_year",
      "welfare_type",
      "reporting_level",
      "median"
    ))

  # join medians to missing data ---------
  dtnm <- merge.data.table( # joined medians
    x = dtn,
    y = med,
    by = c(
      "country_code",
      "reporting_year",
      "welfare_type",
      "reporting_level"
    ),
    all.x = TRUE
  )

  # append ------
  out <- data.table::rbindlist(list(dtnm, dtm),
                               use.names = TRUE,
                               fill      = TRUE)


  return(out)
}
