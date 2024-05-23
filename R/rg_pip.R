#' Compute survey year stats
#'
#' Compute the main PIP poverty and inequality statistics for survey years.
#'
#' @inheritParams pip
#' @param valid_regions character: Vector of accpeted code for regions
#' @return data.frame
#' @keywords internal
rg_pip <- function(country,
                   year,
                   povline,
                   popshare,
                   welfare_type,
                   reporting_level,
                   ppp,
                   lkup) {
  # get values from lkup
  valid_regions <- lkup$query_controls$region$values
  svy_lkup      <- lkup$svy_lkup


  metadata <- subset_lkup(
    country         = country,
    year            = year,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    lkup            = svy_lkup,
    valid_regions   = valid_regions
  )
  # Remove aggregate distribution if popshare is specified
  # TEMPORARY FIX UNTIL popshare is supported for aggregate distributions
  metadata <- filter_lkup(metadata = metadata,
                          popshare = popshare)

  # return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(pipapi::empty_response)
  }

  out <- vector(mode = "list", length = nrow(metadata))

  for (i in seq_along(out)) {
    tmp_metadata <- metadata[i, ]

    svy_data <- get_svy_data(
      tmp_metadata$cache_id,
      reporting_level = tmp_metadata$reporting_level,
      path = tmp_metadata$path
    )
    #browser()
    tmp_stats <- wbpip:::prod_compute_pip_stats(
      welfare           = svy_data$df0$welfare,
      povline           = povline,
      popshare          = popshare,
      population        = svy_data$df0$weight,
      requested_mean    = tmp_metadata$survey_mean_ppp,
      svy_mean_lcu      = tmp_metadata$survey_mean_lcu,
      svy_median_lcu    = tmp_metadata$survey_median_lcu,
      svy_median_ppp    = tmp_metadata$survey_median_ppp,
      default_ppp       = tmp_metadata$ppp,
      ppp               = ppp,
      distribution_type = tmp_metadata$distribution_type
    )
    # Add stats columns to data frame
    for (j in seq_along(tmp_stats)) {
      tmp_metadata[[names(tmp_stats)[j]]] <- list(tmp_stats[[j]])
    }
    # To allow multiple povline values, we store them in a list and unnest
    tmp_metadata <- tmp_metadata %>% tidyr::unnest_longer(col = dplyr::everything())
    out[[i]] <- tmp_metadata
  }
  out <- data.table::rbindlist(out)

  # Add SPL and SPR  ---------------
  out <- add_spl(df        = out,
                 fill_gaps = FALSE,
                 data_dir  = lkup$data_root)

  return(out)
}
