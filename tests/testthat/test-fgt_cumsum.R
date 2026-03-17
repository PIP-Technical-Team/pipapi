# Tests for R/fgt_cumsum.R
# Functions: build_pair_dict(), encode_pairs(), decode_pairs(),
#            update_pair_dict(), format_lfst(), get_total_pop(), fgt_cumsum()
#
# All tests here are pure-unit: synthetic data only, no file system dependency.

library(data.table)
library(collapse)

# ---------------------------------------------------------------------------
# Helpers: minimal synthetic data factories
# ---------------------------------------------------------------------------

.make_lkup_stub <- function(fill_gaps = TRUE) {
  rows <- data.table(
    country_code    = c("AAA", "AAA", "BBB"),
    reporting_year  = c(2000L, 2000L, 2000L),
    reporting_level = c("rural", "urban", "national")
  )
  if (fill_gaps) {
    list(refy_lkup = rows)
  } else {
    list(svy_lkup = rows)
  }
}

# Build a minimal welfare DT pre-processed with cumulative sums.
# Parameters control the group (id / reporting_level) and observations.
.make_cumsum_dt <- function(id = "AAA_2000",
                            reporting_level = "national",
                            welfare = c(1, 2, 3, 4, 5),
                            weight  = c(1, 1, 1, 1, 1),
                            id_rl   = 1L) {
  n   <- length(welfare)
  ord <- order(welfare)
  w   <- welfare[ord]
  wt  <- weight[ord]

  cw    <- cumsum(wt)
  cwy   <- cumsum(wt * w)
  cwy2  <- cumsum(wt * w^2)
  cwylog <- cumsum(wt * log(w))

  sentinel <- data.table(
    id_rl   = id_rl,
    index   = 0L,
    welfare = 0,
    weight  = 0,
    cw      = 0,
    cwy     = 0,
    cwy2    = 0,
    cwylog  = 0
  )

  obs <- data.table(
    id_rl   = id_rl,
    index   = seq_len(n),
    welfare = w,
    weight  = wt,
    cw      = cw,
    cwy     = cwy,
    cwy2    = cwy2,
    cwylog  = cwylog
  )

  rbindlist(list(sentinel, obs))
}

# Wrap a cumsum DT + GRP into an LDTg list (as returned by format_lfst).
# GRP must be built on the full DT (sentinel row included) so that
# collapse::fsum(get_vars(DT, "weight"), g) has matching lengths.
.make_ldtg <- function(dt) {
  g <- GRP(dt, ~ id_rl, sort = FALSE)
  list(DT = dt, g = g)
}


# ===========================================================================
# 1) build_pair_dict()
# ===========================================================================

test_that("build_pair_dict: returns data.table with required columns", {
  stub <- .make_lkup_stub(fill_gaps = TRUE)
  dict <- build_pair_dict(stub, fill_gaps = TRUE)

  expect_true(is.data.table(dict))
  expect_true(all(c("id", "reporting_level", "code") %in% names(dict)))
})

test_that("build_pair_dict: code column is sequential integer starting at 1", {
  stub <- .make_lkup_stub(fill_gaps = TRUE)
  dict <- build_pair_dict(stub, fill_gaps = TRUE)

  expect_type(dict$code, "integer")
  expect_equal(dict$code, seq_len(nrow(dict)))
})

test_that("build_pair_dict: uses refy_lkup when fill_gaps = TRUE", {
  stub <- .make_lkup_stub(fill_gaps = TRUE)
  dict <- build_pair_dict(stub, fill_gaps = TRUE)

  # Input has AAA_2000 (rural + urban) and BBB_2000 (national) -> 3 pairs
  expect_equal(nrow(dict), 3L)
})

test_that("build_pair_dict: uses svy_lkup when fill_gaps = FALSE", {
  stub <- .make_lkup_stub(fill_gaps = FALSE)
  dict <- build_pair_dict(stub, fill_gaps = FALSE)

  expect_equal(nrow(dict), 3L)
  expect_true(all(c("id", "reporting_level") %in% names(dict)))
})

test_that("build_pair_dict: deduplicates rows from input lookup", {
  # Duplicate rows in refy_lkup — dict must still be unique pairs
  dup_lkup <- list(
    refy_lkup = data.table(
      country_code    = c("AAA", "AAA", "AAA"),
      reporting_year  = c(2000L, 2000L, 2000L),
      reporting_level = c("national", "national", "national")
    )
  )
  dict <- build_pair_dict(dup_lkup, fill_gaps = TRUE)
  expect_equal(nrow(dict), 1L)
})

test_that("build_pair_dict: result is deterministically ordered", {
  stub <- .make_lkup_stub(fill_gaps = TRUE)
  dict1 <- build_pair_dict(stub, fill_gaps = TRUE)
  dict2 <- build_pair_dict(stub, fill_gaps = TRUE)
  expect_identical(dict1, dict2)
})


# ===========================================================================
# 2) encode_pairs()
# ===========================================================================

test_that("encode_pairs: adds id_rl integer code column", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  dt <- data.table(
    id              = c("AAA_2000", "BBB_2000"),
    reporting_level = c("rural",    "national"),
    welfare         = c(10, 20)
  )
  out <- encode_pairs(dt, dict, drop_labels = FALSE)

  expect_true("id_rl" %in% names(out))
  expect_type(out$id_rl, "integer")
  expect_false(anyNA(out$id_rl))
})

test_that("encode_pairs: drop_labels removes id and reporting_level", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  dt <- data.table(
    id              = "AAA_2000",
    reporting_level = "urban",
    welfare         = 5
  )
  out <- encode_pairs(dt, dict, drop_labels = TRUE)

  expect_false("id" %in% names(out))
  expect_false("reporting_level" %in% names(out))
  expect_true("id_rl" %in% names(out))
})

test_that("encode_pairs: strict mode errors on unseen pair", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  dt <- data.table(
    id              = "ZZZ_9999",
    reporting_level = "national",
    welfare         = 1
  )
  expect_error(
    encode_pairs(dt, dict, strict = TRUE),
    regexp = "unseen"
  )
})

test_that("encode_pairs: non-strict mode leaves NA for unseen pairs", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  dt <- data.table(
    id              = "ZZZ_9999",
    reporting_level = "national",
    welfare         = 1
  )
  out <- encode_pairs(dt, dict, strict = FALSE)
  expect_true(anyNA(out$id_rl))
})


# ===========================================================================
# 3) decode_pairs()
# ===========================================================================

test_that("decode_pairs: round-trips with encode_pairs", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  original <- data.table(
    id              = c("AAA_2000", "BBB_2000"),
    reporting_level = c("rural",    "national"),
    value           = c(1.0, 2.0)
  )

  encoded <- encode_pairs(copy(original), dict, drop_labels = FALSE)
  decoded <- decode_pairs(encoded, dict,
                          add_true_vars = FALSE,
                          keep_code     = FALSE)

  expect_equal(sort(decoded$id),              sort(original$id))
  expect_equal(sort(decoded$reporting_level), sort(original$reporting_level))
})

test_that("decode_pairs: add_true_vars splits id into country_code + reporting_year", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  dt <- data.table(id_rl = 1L, x = 99)
  out <- decode_pairs(dt, dict, add_true_vars = TRUE, keep_code = FALSE)

  expect_true("country_code"   %in% names(out))
  expect_true("reporting_year" %in% names(out))
  expect_false("id"            %in% names(out))
  expect_type(out$reporting_year, "integer")
})

test_that("decode_pairs: keep_code preserves id_rl column", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  dt <- data.table(id_rl = 1L, x = 1)
  out <- decode_pairs(dt, dict, keep_code = TRUE)

  expect_true("id_rl" %in% names(out))
})


# ===========================================================================
# 4) update_pair_dict()
# ===========================================================================

test_that("update_pair_dict: appends new pairs with next codes", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)
  orig_nrow <- nrow(dict)

  new_dt <- data.table(
    id              = "CCC_2005",
    reporting_level = "national"
  )
  updated <- update_pair_dict(dict, new_dt)

  expect_equal(nrow(updated), orig_nrow + 1L)
  expect_equal(max(updated$code), orig_nrow + 1L)
})

test_that("update_pair_dict: no-op when all pairs already present", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  existing_dt <- data.table(
    id              = "AAA_2000",
    reporting_level = "rural"
  )
  updated <- update_pair_dict(dict, existing_dt)
  expect_equal(nrow(updated), nrow(dict))
})

test_that("update_pair_dict: preserves existing codes after append", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)
  orig_codes <- copy(dict$code)

  new_dt <- data.table(id = "DDD_2010", reporting_level = "national")
  updated <- update_pair_dict(dict, new_dt)

  shared_rows <- updated[id != "DDD_2010"]
  expect_equal(shared_rows$code, orig_codes)
})


# ===========================================================================
# 5) format_lfst()
# ===========================================================================

test_that("format_lfst: returns list with DT and g elements", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  # Build a minimal lfst: named list of data.tables with id + reporting_level
  make_survey <- function(id, rl, n = 5) {
    data.table(
      id              = id,
      reporting_level = rl,
      welfare         = sort(runif(n, 1, 10)),
      weight          = rep(1, n),
      index           = seq_len(n),
      cw              = cumsum(rep(1, n)),
      cwy             = cumsum(sort(runif(n, 1, 10))),
      cwy2            = cumsum(sort(runif(n, 1, 100))),
      cwylog          = cumsum(log(sort(runif(n, 1, 10))))
    )
  }
  lfst <- list(
    AAA_2000 = make_survey("AAA_2000", "rural"),
    BBB_2000 = make_survey("BBB_2000", "national")
  )

  result <- format_lfst(lfst, dict)

  expect_type(result, "list")
  expect_true(all(c("DT", "g") %in% names(result)))
})

test_that("format_lfst: DT has id_rl column (labels dropped)", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  make_survey <- function(id, rl, n = 3) {
    data.table(
      id              = id,
      reporting_level = rl,
      welfare         = seq_len(n),
      weight          = rep(1, n),
      index           = seq_len(n),
      cw = cumsum(rep(1, n)), cwy = cumsum(seq_len(n)),
      cwy2 = cumsum(seq_len(n)^2), cwylog = cumsum(log(seq_len(n)))
    )
  }
  lfst <- list(
    AAA_2000 = make_survey("AAA_2000", "rural"),
    BBB_2000 = make_survey("BBB_2000", "national")
  )
  result <- format_lfst(lfst, dict)

  expect_true("id_rl" %in% names(result$DT))
  expect_false("id" %in% names(result$DT))
  expect_false("reporting_level" %in% names(result$DT))
})

test_that("format_lfst: g is a GRP object", {
  stub <- .make_lkup_stub()
  dict <- build_pair_dict(stub)

  lfst <- list(
    AAA_2000 = data.table(
      id = "AAA_2000", reporting_level = "rural",
      welfare = 1:3, weight = 1, index = 1:3,
      cw = 1:3, cwy = 1:3, cwy2 = 1:3, cwylog = log(1:3)
    )
  )
  result <- format_lfst(lfst, dict)
  expect_s3_class(result$g, "GRP")
})


# ===========================================================================
# 6) get_total_pop()
# ===========================================================================

test_that("get_total_pop: returns data.table with id_rl and W", {
  dt  <- .make_cumsum_dt(welfare = 1:4, weight = c(1, 2, 1, 2))
  ldtg <- .make_ldtg(dt)
  tpop <- get_total_pop(ldtg)

  expect_true(is.data.table(tpop))
  expect_true(all(c("id_rl", "W") %in% names(tpop)))
})

test_that("get_total_pop: W equals sum of weights", {
  wt  <- c(1, 2, 3, 4)
  dt  <- .make_cumsum_dt(welfare = 1:4, weight = wt)
  ldtg <- .make_ldtg(dt)
  tpop <- get_total_pop(ldtg)

  expect_equal(tpop$W, sum(wt))
})


# ===========================================================================
# 7) fgt_cumsum() vs compute_fgt() agreement
# ===========================================================================

test_that("fgt_cumsum: headcount matches compute_fgt for uniform weights", {
  welfare <- c(1, 2, 3, 4, 5)
  weight  <- rep(1, 5)
  povline <- 2.5

  # Reference: compute_fgt (positional: w, wt, povlines)
  ref <- compute_fgt(w = welfare, wt = weight, povlines = povline)

  # fgt_cumsum path
  dt    <- .make_cumsum_dt(welfare = welfare, weight = weight)
  ldtg  <- .make_ldtg(dt)
  tpop  <- get_total_pop(ldtg)
  res   <- fgt_cumsum(ldtg, tpop, povline)

  expect_equal(res$headcount,   ref$headcount,   tolerance = 1e-9)
  expect_equal(res$poverty_gap, ref$poverty_gap, tolerance = 1e-9)
})

test_that("fgt_cumsum: poverty measures agree across multiple poverty lines", {
  welfare  <- c(0.5, 1.0, 2.0, 3.5, 6.0)
  weight   <- c(2,   1,   3,   1,   2)
  povlines <- c(1.0, 2.5, 4.0)

  ref_list <- lapply(povlines, \(z) {
    compute_fgt(w = welfare, wt = weight, povlines = z)
  })

  dt   <- .make_cumsum_dt(welfare = welfare, weight = weight)
  ldtg <- .make_ldtg(dt)
  tpop <- get_total_pop(ldtg)
  res  <- fgt_cumsum(ldtg, tpop, povlines)
  setorder(res, povline)

  for (i in seq_along(povlines)) {
    r <- res[i]
    expect_equal(r$headcount,        ref_list[[i]]$headcount,        tolerance = 1e-9)
    expect_equal(r$poverty_gap,      ref_list[[i]]$poverty_gap,      tolerance = 1e-9)
    expect_equal(r$poverty_severity, ref_list[[i]]$poverty_severity, tolerance = 1e-9)
  }
})

test_that("fgt_cumsum: drop_vars=FALSE includes extra columns", {
  dt    <- .make_cumsum_dt(welfare = 1:5, weight = rep(1, 5))
  ldtg  <- .make_ldtg(dt)
  tpop  <- get_total_pop(ldtg)
  res   <- fgt_cumsum(ldtg, tpop, povline = 3, drop_vars = FALSE)

  expect_true(all(c("cw", "cwy", "cwy2", "cwylog") %in% names(res)))
})

test_that("fgt_cumsum: zero poverty line yields zero headcount", {
  dt    <- .make_cumsum_dt(welfare = 1:5, weight = rep(1, 5))
  ldtg  <- .make_ldtg(dt)
  tpop  <- get_total_pop(ldtg)
  res   <- fgt_cumsum(ldtg, tpop, povline = 0)

  expect_equal(res$headcount, 0, tolerance = 1e-9)
})

test_that("fgt_cumsum: poverty line above all welfare yields headcount 1", {
  dt    <- .make_cumsum_dt(welfare = 1:5, weight = rep(1, 5))
  ldtg  <- .make_ldtg(dt)
  tpop  <- get_total_pop(ldtg)
  res   <- fgt_cumsum(ldtg, tpop, povline = 100)

  expect_equal(res$headcount, 1, tolerance = 1e-9)
})
