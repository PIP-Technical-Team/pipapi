# compute_fgt_new.R
#
# Core FGT (Foster-Greer-Thorbecke) poverty index computation.
# All functions are pure numeric — no I/O, no lkup dependency.
#
# Functions:
#   compute_fgt_dt() - FGT for a data.table, vectorised over poverty lines
#   compute_fgt()    - FGT for bare vectors (no data.table)
#   process_dt()     - apply compute_fgt_dt() grouped by id_var + reporting_level


#' Efficient FGT calculation for a data.table and vector of poverty lines
#'
#'
#' @param dt data frame with `welfare` and `weight` columns
#' @param welfare character: welfare variable name
#' @param weight character: weight variable name
#' @param povlines double: vector with poveryt lines
#'
#' @return data.table with estimates poverty estimates
#' @keywords internal
compute_fgt_dt <- function(
  dt,
  welfare,
  weight,
  povlines,
  mean_and_med = FALSE
) {
  w <- dt[[welfare]]
  wt <- dt[[weight]]
  n <- length(w)
  m <- length(povlines)

  # Pre-allocate result matrix
  res <- matrix(NA_real_, nrow = m, ncol = 3)
  colnames(res) <- c("FGT0", "FGT1", "FGT2")
  watts_vec <- numeric(m)

  # Precompute log(w) for efficiency (vectorized)

  pos <- w > 0
  # logw <- log(w)
  logw <- copyv(log(w), pos, NA_real_, invert = TRUE) |>
    suppressWarnings()
  # logw <- fifelse(w > 0, log(w), NA_real_)

  for (i in seq_along(povlines)) {
    pov <- povlines[i]
    poor <- w < pov
    rel_dist <- 1 - (w / pov)
    setv(rel_dist, poor, 0, invert = TRUE)
    # rel_dist[!poor] <- 0
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

  if (mean_and_med) {
    mn <- ffirst(dt$mean)
    med <- ffirst(dt$median)
    cy <- ffirst(dt$country_code)
    ry <- ffirst(dt$reporting_year)
    out <- data.table(
      povline = povlines,
      headcount = res[, 1],
      poverty_gap = res[, 2],
      poverty_severity = res[, 3],
      watts = watts_vec,
      mean = mn,
      median = med,
      country_code = cy,
      reporting_year = ry
    )
  } else {
    out <- data.table(
      povline = povlines,
      headcount = res[, 1],
      poverty_gap = res[, 2],
      poverty_severity = res[, 3],
      watts = watts_vec
    )
  }

  out
}

#' Efficient FGT calculation for vectors (No data.table)
#'
#' @param w character: welfare variable name
#' @param wt character: weight variable name
#' @param povlines double: vector with poverty lines
#'
#' @return data.table with estimates poverty estimates
#' @keywords internal
compute_fgt <- function(w, wt, povlines) {
  m <- length(povlines)

  # Pre-allocate result matrix
  res <- matrix(NA_real_, nrow = m, ncol = 3)
  colnames(res) <- c("FGT0", "FGT1", "FGT2")
  watts_vec <- numeric(m)

  # Precompute log(w) for efficiency (vectorized)

  pos <- w > 0
  # logw <- log(w)
  # logw <- copyv(log(w), pos, NA_real_, invert = TRUE) |>
  #   suppressWarnings()
  # logw <- fifelse(w > 0, log(w), NA_real_)
  logw <- log(w) |>
    suppressWarnings()

  tot_pop <- fsum(wt)

  for (i in seq_along(povlines)) {
    pov <- povlines[i]
    poor <- w < pov
    rel_dist <- 1 - (w / pov)
    setv(rel_dist, poor, 0, invert = TRUE)
    # rel_dist[!poor] <- 0
    res[i, 1] <- fmean(poor, w = wt) # FGT0
    res[i, 2] <- fmean(rel_dist, w = wt) # FGT1
    res[i, 3] <- fmean(rel_dist^2, w = wt) # FGT2

    # Optimized Watts index calculation
    keep <- poor & pos
    if (any(keep, na.rm = TRUE)) {
      watts_vec[i] <- (fsum((log(pov) - logw[keep]) * wt[keep])) / tot_pop
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

#' Apply FGT computation across groups in a data.table
#'
#' Splits `dt` by `id_var` and `reporting_level`, then calls
#' [compute_fgt_dt()] on each group for the given `povlines`.
#'
#' @param dt data.table: survey data with `welfare`, `weight`, and `id_var`
#'   columns.
#' @param povline numeric: vector of poverty lines to evaluate.
#' @param mean_and_med logical: if `TRUE`, include `mean`, `median`,
#'   `country_code`, and `reporting_year` in the output. Default `FALSE`.
#' @param id_var character: name of the grouping id column. Default `"file"`.
#'
#' @return data.table with FGT0, FGT1, FGT2, and watts columns (plus id and
#'   optional summary stats), one row per poverty line per group.
#' @keywords internal
process_dt <- function(dt, povline, mean_and_med = FALSE, id_var = "file") {
  byvars <- c(id_var, "reporting_level")
  dt[,
    compute_fgt_dt(.SD, "welfare", "weight", povline, mean_and_med),
    by = byvars
  ]
}

