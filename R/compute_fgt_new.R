# OLD APPROACH WITH MEAN --------------

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
compute_fgt_dt <- function(dt, welfare, weight, povlines, mean_and_med = FALSE) {
  w   <- dt[[welfare]]
  wt  <- dt[[weight]]
  n   <- length(w)
  m   <- length(povlines)

  # Pre-allocate result matrix
  res <- matrix(NA_real_, nrow = m, ncol = 3)
  colnames(res) <- c("FGT0", "FGT1", "FGT2")
  watts_vec <- numeric(m)

  # Precompute log(w) for efficiency (vectorized)

  pos  <- w > 0
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
    mn  <- ffirst(dt$mean)
    med <- ffirst(dt$median)
    cy  <- ffirst(dt$coutnry_code)
    ry  <- ffirst(dt$reporting_year)
    out <- data.table(
      povline          = povlines,
      headcount        = res[, 1],
      poverty_gap      = res[, 2],
      poverty_severity = res[, 3],
      watts            = watts_vec,
      mean             = mn,
      median           = med,
      country_code     = cy,
      reporting_year   = ry)
  } else {
    out <- data.table(
      povline          = povlines,
      headcount        = res[, 1],
      poverty_gap      = res[, 2],
      poverty_severity = res[, 3],
      watts            = watts_vec)
  }

  out

}



# Efficient FGT calculation for vectors (No data.table)
#'
#' @param w character: welfare variable name
#' @param wt character: weight variable name
#' @param povlines double: vector with poverty lines
#'
#' @return data.table with estimates poverty estimates
#' @keywords internal
compute_fgt <- function(w, wt, povlines) {
  n   <- length(w)
  m   <- length(povlines)

  # Pre-allocate result matrix
  res <- matrix(NA_real_, nrow = m, ncol = 3)
  colnames(res) <- c("FGT0", "FGT1", "FGT2")
  watts_vec <- numeric(m)

  # Precompute log(w) for efficiency (vectorized)

  pos  <- w > 0
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
      povline          = povlines,
      headcount        = res[, 1],
      poverty_gap      = res[, 2],
      poverty_severity = res[, 3],
      watts            = watts_vec)

}

#' compute FGT using indices by reporting level
#'
#' This function is intended to be used inside [map_fgt]
#'
#' @param x data.table from lt list, with welfare and weight vectors
#' @param y list of indices for each reporting level
#' @param nx name of data table. Usuall country code and year in the form "CCC_YYYY"
#'
#' @rdname map_fgt
#' @keywords  internal
DT_fgt_by_rl <- \(x, y, nx, povline) {
  uni_rl <- names(y) |>
    unique()
  DT_fgt <- lapply(uni_rl, \(rl) {

    idx <- y[[rl]]
    w   <- x[idx, welfare]
    wt  <- x[idx, weight]
    RL  <- compute_fgt(w = w, wt = wt, povlines = povline)
    RL[, reporting_level := rl]

  }) |>
    rbindlist(fill = TRUE)


  DT_fgt[, `:=`(
    country_code   = gsub("([^_]+)(_.+)", "\\1", nx),
    reporting_year = gsub("(.+_)([^_]+)", "\\2", nx)
  )]
}



#' jkoin reporting level and lt list into one data.table
#'
#' @rdname map_fgt
lt_to_dt <- \(x, y, nx, povline) {
  DT <- lapply(names(y), \(rl) {

    idx <- y[[rl]]
    x[idx, reporting_level := rl]

  }) |>
    rbindlist(fill = TRUE)


  DT[, `:=`(
    country_code   = gsub("([^_]+)(_.+)", "\\1", nx),
    reporting_year = gsub("(.+_)([^_]+)", "\\2", nx)
  )]
}

#' Map lt_to_dt
#'
#' @rdname map_fgt
map_lt_to_dt <- \(lt, l_rl_rows, povline) {
  Map(lt_to_dt, lt, l_rl_rows, names(lt),
      MoreArgs = list(povline = povline)) |>
    rbindlist(fill = TRUE)
}


#' map over list of data.tables and indices to compute FGT by reporting_level
#'
#' @param lt list of data.tables with welfare and weight data
#' @param l_rl_rows list of indeces
#'
#' @return data.table with all measured
#' @keywords internal
map_fgt <- \(lt, l_rl_rows, povline) {
  Map(DT_fgt_by_rl, lt, l_rl_rows, names(lt),
      MoreArgs = list(povline = povline)) |>
    rbindlist(fill = TRUE)
}

process_dt <- function(dt, povline, mean_and_med = FALSE) {
  dt[, compute_fgt_dt(.SD, "welfare", "weight", povline, mean_and_med),
     by = .(file, reporting_level)]
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
      fdt <- data.table(reporting_level = as.character(rep_level),
                        ppp             = ppp,
                        cpi             = cpi)

      # load data and format
      dt <-  fst::read_fst(path, as.data.table = TRUE)

      if (length(rep_level) == 1) {
        if (rep_level == "national") dt[, area := "national"]
      }
      setnames(dt, "area", "reporting_level")
      dt[,
         `:=`(
           file = basename(path),
           reporting_level = as.character(reporting_level)
         )
      ]

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



pov_from_DT <- function(DT, povline, g, cores = 1) {
  w       <- DT$welfare
  wt      <- DT$weight
  n_pov   <- length(povline)

  ng      <- g$N.groups
  grp_ids <- qDT(g$groups)

  # Precompute log(w) for efficiency
  pos <- w > 0
  logw <- fifelse(pos, log(w), NA_real_)

  # Prepare result lists
  fgt0 <- vector("list", n_pov)
  fgt1 <- vector("list", n_pov)
  fgt2 <- vector("list", n_pov)
  watts <- vector("list", n_pov)

  for (i in seq_along(povline)) {
    pov <- povline[i]
    poor <- w < pov
    rel_dist <- fifelse(poor, 1 - w/pov, 0)
    keep <- poor & pos
    watts_val <- fmean((log(pov) - logw) * keep,
                       g = g, w = wt, nthreads  = cores )
    fgt0[[i]] <- fmean(poor, g = g, w = wt,
                       nthreads  = cores)
    fgt1[[i]] <- fmean(rel_dist, g = g, w = wt,
                       nthreads  = cores)
    fgt2[[i]] <- fmean(rel_dist^2, g = g, w = wt,
                       nthreads  = cores)
    watts[[i]] <- watts_val
  }

  out <- data.table(
    povline = rep(povline, each = ng),
    fgt0 = unlist(fgt0),
    fgt1 = unlist(fgt1),
    fgt2 = unlist(fgt2),
    watts = unlist(watts)
  )
  # Repeat group columns for each povline
  grp_dt <- grp_ids[rep(seq_len(ng), times = n_pov)]
  add_vars(out, pos = "front") <- grp_dt
  out
}





# pov_from_DT2 <- function(DT, povline, g) {
#   fgt0 <- numeric(length(povline))
#   fgt1 <- numeric(length(povline))
#   fgt2 <- numeric(length(povline))
#   w <- DT$welfare
#   wt <- DT$weight
#
#
#   for (i in seq_along(povline)) {
#     pov <- povline[i]
#     poor <- w < pov
#     rel_dist <- fifelse(poor, 1 - w/pov, 0)
#     fgt0[i] <- fmean(poor, g = g, w = wt)
#     fgt1[i] <- fmean(rel_dist, g = g, w = wt)
#     fgt2[i] <- fmean(rel_dist^2, g = g, w = wt)
#   }
#
#   list(fgt0 = fgt0, fgt1 = fgt1, fgt2 = fgt2)
# }


