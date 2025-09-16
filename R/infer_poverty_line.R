#' Infer the poverty line for a given population share
#'
#' Computes the welfare value (poverty line) corresponding to a given population
#' share, using either nearest or interpolated weighted quantile methods.
#' Supports both discrete (nearest) and linear interpolation approaches, and can
#' optionally average neighbors for ties.
#'
#' @param welfare Numeric vector of welfare values (e.g., income or
#'   consumption).
#' @param weight Numeric vector of sampling weights (must be non-negative, same
#'   length as welfare).
#' @param popshare Numeric vector of population shares (probabilities in [0,1]);
#'   default is 0.5 (median).
#' @param include Logical; if TRUE, averages neighbors for ties (only for method
#'   = "nearest").
#' @param method Character; either "nearest" (default, discrete quantile) or
#'   "interp" (weighted linear interpolation).
#' @param assume_sorted Logical; if TRUE, assumes welfare and weight are already
#'   sorted by welfare.
#'
#' @return Numeric vector of poverty line(s) corresponding to the requested
#'   population share(s).
#' @details
#' - If method = "nearest", returns the welfare value at the closest cumulative weight fraction to each popshare.
#' - If method = "interp", uses collapse::fquantile for weighted linear interpolation.
#' - If include = TRUE (and method = "nearest"), averages the two closest neighbors using their weights.
#' - Returns numeric(0) if popshare is empty.
#' @keywords internal
infer_poverty_line <- function(welfare, weight, popshare = 0.5,
                               include = FALSE,
                               method = c("nearest","interp"),
                               assume_sorted = FALSE) {
  method <- match.arg(method)

  # basic checks
  if (length(welfare) != length(weight)) stop("welfare and weight must have the same length")
  if (anyNA(welfare) || anyNA(weight))   stop("welfare and weight cannot contain NA")
  if (any(weight < 0))                   stop("weights must be non-negative")
  if (!length(popshare)) return(numeric(0))

  # clamp probs
  p <- pmin(pmax(as.numeric(popshare), 0), 1)

  # fast sort (or not)
  if (!assume_sorted) {
    o <- if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::forder(welfare, na.last = FALSE)
    } else {
      order(welfare, na.last = NA, method = "radix")
    }
    y <- welfare[o]; w <- weight[o]
  } else {
    y <- welfare;   w <- weight
  }

  if (method == "interp") {
    # collapse::fquantile: weighted linear interpolation
    # - 'sorted' tells fquantile that 'y' is already sorted
    # - 'w' supplies weights
    # 'include' is not used here: interpolation doesn't have that discrete toggle
    return(collapse::fquantile(y, probs = p, w = w, sorted = TRUE))
  }

  # ---- method == "nearest" (matches your function) ----
  # cumulative weight fractions
  W  <- collapse::fsum(w)
  if (W <= 0) stop("sum(weight) must be > 0")
  cw   <- collapse::fcumsum(w)
  prob <- cw / W
  n    <- length(y)

  # for each p, find the nearest cumulative location (ties -> lower index)
  j <- findInterval(p, prob, left.open = FALSE)        # j ∈ {0..n}
  j[j < 0L] <- 0L; j[j > n] <- n

  prev_idx <- pmax.int(j, 1L)                          # 1..n
  next_idx <- pmin.int(j + 1L, n)                      # 1..n
  d_prev   <- p - prob[prev_idx]
  d_next   <- prob[next_idx] - p
  use_next <- (d_next < d_prev) & (j < n)
  idx      <- ifelse(use_next, next_idx, prev_idx)     # final index

  if (!include) {
    # take the discrete value at the nearest location
    return(y[idx])
  } else {
    # average the two neighbors using their weights (your original rule)
    idx2 <- pmin.int(idx + 1L, n)
    wi   <- w[idx]
    wi2  <- w[idx2]
    s    <- wi + wi2
    num  <- wi * y[idx] + wi2 * y[idx2]
    out  <- ifelse((idx == n) | (s <= 0), y[idx], num / s)
    return(out)
  }
}
