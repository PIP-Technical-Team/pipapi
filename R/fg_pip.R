#' Compute imputed year stats
#'
#' Compute the main PIP poverty and inequality statistics for imputed years.
#'
#' @inheritParams pip
#' @return data.frame
#' @keywords internal
fg_pip <- function(country,
                   year,
                   povline,
                   popshare,
                   welfare_type,
                   reporting_level,
                   ppp,
                   lkup) {

  valid_regions       <- lkup$query_controls$region$values
  interpolation_list  <- lkup$interpolation_list

  # Handle interpolation
  ref_lkup <- lkup[["ref_lkup"]]
  metadata <- subset_lkup(
    country         = country,
    year            = year,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    lkup            = ref_lkup,
    valid_regions   = valid_regions
  )
  # Remove aggregate distribution if popshare is specified
  # TEMPORARY FIX UNTIL popshare is supported for aggregate distributions
  metadata <- filter_lkup(metadata = metadata,
                          popshare = popshare)
  metadata <- as.data.frame(metadata) # data.tables speed only kicks in for big datasets

  # Return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(pipapi::empty_response)
  }

  unique_survey_files <- unique(metadata$data_interpolation_id)

  # Interpolation list
  interpolation_list <- interpolation_list[names(interpolation_list) %in% unique_survey_files]

  # Unique set of survey data to be read
  out <- vector(mode = "list", length = length(unique_survey_files))

  #NEW: iterate over survey files
  for (svy_id in seq_along(unique_survey_files)) {
    # Extract country-years for which stats will be computed from the same files
    # tmp_metadata <- interpolation_list[[unique_survey_files[svy_id]]]$tmp_metadata
    iteration           <- interpolation_list[[unique_survey_files[svy_id]]]

    svy_data <- get_svy_data(svy_id          = iteration$cache_ids,
                             reporting_level = iteration$reporting_level,
                             path            = iteration$paths)

    # Extract unique combinations of country-year
    ctry_years <- subset_ctry_years(country       = country,
                                    year          = year,
                                    lkup          = iteration$ctry_years,
                                    valid_regions = valid_regions)

    results_subset <- vector(mode = "list", length = nrow(ctry_years))

    for (ctry_year_id in seq_along(ctry_years$interpolation_id)) {

      # Extract records to be used for a single country-year estimation
      interp_id    <- ctry_years[["interpolation_id"]][ctry_year_id]
      tmp_metadata <- metadata[metadata$interpolation_id == interp_id, ]

      # Compute estimated statistics using the fill_gap method
      tmp_stats <- wbpip:::prod_fg_compute_pip_stats(
        request_year           = ctry_years[["reporting_year"]][ctry_year_id],
        data                   = svy_data,
        predicted_request_mean = tmp_metadata[["predicted_mean_ppp"]],
        svy_mean_lcu           = tmp_metadata[["survey_mean_lcu"]],
        svy_median_lcu         = tmp_metadata$survey_median_lcu,
        svy_median_ppp         = tmp_metadata$survey_median_ppp,
        survey_year            = tmp_metadata[["survey_year"]],
        default_ppp            = tmp_metadata[["ppp"]],
        ppp                    = ppp,
        distribution_type      = tmp_metadata[["distribution_type"]],
        poverty_line           = povline,
        popshare               = popshare
      )

      # Handle multiple distribution types (for aggregated distributions)
      if (length(unique(tmp_metadata$distribution_type)) > 1) {
        tmp_metadata$distribution_type <- "mixed"
      }
      #
      # tmp_metadata <- unique(tmp_metadata)

      # Add stats columns to data frame
      for (stat in seq_along(tmp_stats)) {
        tmp_metadata[[names(tmp_stats)[stat]]] <- tmp_stats[[stat]]
      }


      results_subset[[ctry_year_id]] <- tmp_metadata
    }

    out[[svy_id]] <- results_subset
  }

  out <- unlist(out, recursive = FALSE)
  out <- data.table::rbindlist(out)


  # Remove median
  out[, median := NULL]

  # Ensure that out does not have duplicates
  out <- fg_remove_duplicates(out)

  # Fix issue with rounding of poverty lines
  out[,
      poverty_line := round(poverty_line, digits = 3) ]

  # Add SPL and SPR  ---------------
  out <- add_spl(df        = out,
                 fill_gaps = TRUE,
                 data_dir  = lkup$data_root)

  return(out)
}

#' Remove duplicated rows created during the interpolation process
#'
#' @param df data.table: Table of results created in `fg_pip()`
#' @param cols character: Columns with potential duplicate values
#'
#' @return data.table
#'

fg_remove_duplicates <- function(df,
                                 cols = c("comparable_spell",
                                          "cpi",
                                          "display_cp",
                                          "gd_type",
                                          "interpolation_id",
                                          "path",
                                          "predicted_mean_ppp",
                                          "survey_acronym",
                                          "survey_comparability",
                                          "survey_coverage",
                                          "survey_id",
                                          "survey_mean_lcu",
                                          "survey_mean_ppp",
                                          "survey_median_lcu",
                                          "survey_median_ppp",
                                          "survey_time",
                                          "survey_year",
                                          "surveyid_year")) {
  # Modify cache_id
  # * Ensures that cache_id is unique for both extrapolated and interpolated surveys
  # * Ensures that cache_id can be kept as an output of fg_pip() while still removing duplicated rows
  df$cache_id <- fg_standardize_cache_id(cache_id = df$cache_id,
                                         interpolation_id = df$data_interpolation_id,
                                         reporting_level = df$reporting_level)
  # Set collapse vars to NA (by type)
  df <- fg_assign_nas_values_to_dup_cols(df = df,
                                         cols = cols)

  # Ensure that out does not have duplicates
  df <- unique(df)

  return(df)
}

#' Standardize cache_id format to avoid duplication of rows
#'
#' @param cache_id character
#' @param interpolation_id character
#' @param reporting_level character
#'
#' @return character

fg_standardize_cache_id <- function(cache_id,
                                    interpolation_id,
                                    reporting_level) {

  out <- ifelse(grepl("|", interpolation_id, fixed = TRUE),
                gsub(paste0("_",
                            unique(reporting_level),
                            collapse = '|'),
                     '',
                     interpolation_id),
                cache_id)
  return(out)
}

#' Coerce variable causing potential duplicates to NAs
#'
#' @inheritParams fg_remove_duplicates
#'
#' @return data.table

fg_assign_nas_values_to_dup_cols <- function(df,
                                             cols) {
  #Classes are maintained by default.
  df[, (cols) := NA]
  return(df)
}
