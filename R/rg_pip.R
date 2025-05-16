#' Compute survey year stats
#'
#' Compute the main PIP poverty and inequality statistics for survey years.
#'
#' @inheritParams pip
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
  data_dir      <- lkup$data_root

  cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")

  metadata <- subset_lkup(
    country         = country,
    year            = year,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    lkup            = svy_lkup,
    valid_regions   = valid_regions,
    data_dir        = data_dir,
    povline         = povline,
    cache_file_path = cache_file_path,
    fill_gaps       = FALSE
  )
  data_present_in_master <- metadata$data_present_in_master
  metadata <- metadata$lkup

  # Remove aggregate distribution if popshare is specified
  # TEMPORARY FIX UNTIL popshare is supported for aggregate distributions
  metadata <- filter_lkup(metadata = metadata,
                          popshare = popshare)

  # return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(list(main_data = empty_response,
                data_in_cache = data_present_in_master))
  }

  out <- vector(mode = "list", length = nrow(metadata))

  for (i in seq_along(out)) {
    tmp_metadata <- metadata[i, ]

    svy_data <- get_svy_data(
      tmp_metadata$cache_id,
      reporting_level = tmp_metadata$reporting_level,
      path = tmp_metadata$path
    )
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
    tmp_metadata <-
      tmp_metadata %>%
      unnest_dt_longer(names(tmp_metadata)[sapply(tmp_metadata, is.list)])
    out[[i]] <- tmp_metadata
  }
  #browser()
  out <- data.table::rbindlist(out)

  return(list(main_data = out, data_in_cache = data_present_in_master))
}



# Efficient FGT calculation for a data.table and vector of poverty lines
#' Title
#'
#' @param dt data frame with `welfare` and `weight` columns
#' @param welfare character: welfare variable name
#' @param weight character: weight variable name
#' @param povlines double: vector with poveryt lines
#'
#' @return data.table with estimates poverty estimates
compute_fgt_dt <- function(dt, welfare, weight, povlines) {
  w <- dt[[welfare]]
  wt <- dt[[weight]]
  n <- length(w)
  m <- length(povlines)

  # Pre-allocate result matrix
  res <- matrix(NA_real_, nrow = m, ncol = 3)
  colnames(res) <- c("FGT0", "FGT1", "FGT2")
  watts_vec <- numeric(m)

  # Precompute log(w) for efficiency
  logw <- rep(NA_real_, n)
  pos <- w > 0
  logw[pos] <- log(w[pos])

  for (i in seq_along(povlines)) {
    pov <- povlines[i]
    poor <- w < pov
    rel_dist <- 1 - (w / pov)
    rel_dist[!poor] <- 0
    res[i, 1] <- fmean(poor, w = wt) # FGT0
    res[i, 2] <- fmean(rel_dist, w = wt) # FGT1
    res[i, 3] <- fmean(rel_dist^2, w = wt) # FGT2

    # Optimized Watts index calculation
    keep <- poor & pos
    if (any(keep)) {
      watts_vec[i] <- (fsum((log(pov) - logw[keep]) * wt[keep])) / fsum(wt)
    } else {
      watts_vec[i] <- 0
    }
  }
  data.table(
    povline = povlines,
    headcount = res[, 1],
    poverty_gap = res[, 2],
    poverty_severity = res[, 3],
    watts = watts_vec
  )
}




#' Compute survey year stats
#'
#' Compute the main PIP poverty and inequality statistics for survey years.
#'
#' @inheritParams pip
#' @return data.frame
#' @keywords internal
rg_pip_old <- function(country,
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
  data_dir      <- lkup$data_root

  cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")

  metadata <- subset_lkup(
    country         = country,
    year            = year,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    lkup            = svy_lkup,
    valid_regions   = valid_regions,
    data_dir        = data_dir,
    povline         = povline,
    cache_file_path = cache_file_path,
    fill_gaps       = FALSE
  )
  data_present_in_master <- metadata$data_present_in_master
  metadata <- metadata$lkup

  # Remove aggregate distribution if popshare is specified
  # TEMPORARY FIX UNTIL popshare is supported for aggregate distributions
  metadata <- filter_lkup(metadata = metadata,
                          popshare = popshare)

  # return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(list(main_data = empty_response,
                data_in_cache = data_present_in_master))
  }

  out <- vector(mode = "list", length = nrow(metadata))

  for (i in seq_along(out)) {
    tmp_metadata <- metadata[i, ]

    svy_data <- get_svy_data(
      tmp_metadata$cache_id,
      reporting_level = tmp_metadata$reporting_level,
      path = tmp_metadata$path
    )
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
    tmp_metadata <-
      tmp_metadata %>%
      unnest_dt_longer(names(tmp_metadata)[sapply(tmp_metadata, is.list)])
    out[[i]] <- tmp_metadata
  }
  #browser()
  out <- data.table::rbindlist(out)

  return(list(main_data = out, data_in_cache = data_present_in_master))
}
