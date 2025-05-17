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
  metadata  <- metadata$lkup


  # Remove aggregate distribution if popshare is specified
  # TEMPORARY FIX UNTIL popshare is supported for aggregate distributions
  metadata <- filter_lkup(metadata = metadata,
                          popshare = popshare)

  # return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(list(main_data = empty_response,
                data_in_cache = data_present_in_master))
  }

  # load data
  lt <- load_data_list(metadata)

  # perform calcualtions
  res <- lapply(lt, function(dt) { #Should we use future_lapply ???
    dt[, compute_fgt_dt(.SD, "welfare", "weight", povline),
       by = .(file, reporting_level)]
  })
  res <- rbindlist(res, fill = TRUE)

  # clean data
  metadata[, file := basename(path)]

  out <- join(res,
              metadata,
              on = c("file", "reporting_level"),
              how = "full",
              validate = "m:1",
              verbose = 0)

  out[, `:=`(
    mean = survey_mean_ppp,
    median = survey_median_ppp,
    file = NULL
  )]

  setnames(out, "povline", "poverty_line")


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
#' @keywords internal
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
    if (any(keep, na.rm = TRUE)) {
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




#' load survey year files and store them in a list
#'
#' @param metadata data frame from `subset_lkup()`
#'
#' @return list with survey years data
#' @keywords internal
load_data_list <- \(metadata) {

  # unique values
  mdout      <- metadata[, lapply(.SD, list), by = path]
  upaths     <- mdout$path
  urep_level <- mdout$reporting_level
  uppp       <- mdout$ppp
  ucpi       <- mdout$cpi

  seq_along(upaths) |>
    lapply(\(f) {
      path      <- upaths[f]
      rep_level <- urep_level[f][[1]]
      ppp       <- uppp[f][[1]]
      cpi       <- ucpi[f][[1]]

      # Build a data.table to merge cpi and ppp
      fdt <- data.table(reporting_level = rep_level,
                        ppp             = ppp,
                        cpi             = cpi)

      # load data and format
      dt <-  fst::read_fst(path, as.data.table = TRUE)

      if (length(rep_level) == 1) {
        if (rep_level == "national") dt[, area := "national"]
      }
      dt[, file := basename(path)]
      setnames(dt, "area", "reporting_level")

      dt <- join(dt, fdt,
                 on = "reporting_level",
                 validate = "m:1",
                 how = "left",
                 verbose = 0)

      dt[, welfare := welfare/(cpi * ppp)
         ][,
           c("cpi", "ppp") := NULL]

    })

}

