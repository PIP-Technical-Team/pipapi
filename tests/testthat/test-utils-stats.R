# test-utils-stats.R
#
# Unit tests for enrichment helpers in utils-stats.R.
# Uses synthetic inline data — no external files required.

library(data.table)


# add_dist_stats_old() -------------------------------------------------------

test_that("add_dist_stats_old merges distributional columns onto df", {
  df <- data.table(
    cache_id        = "AAA_2000",
    reporting_level = "national",
    headcount       = 0.3
  )
  dist_stats <- data.table(
    cache_id        = "AAA_2000",
    reporting_level = "national",
    gini            = 0.35,
    polarization    = 0.20,
    mld             = 0.18,
    decile1         = 0.04,
    decile2         = 0.05,
    decile3         = 0.06,
    decile4         = 0.07,
    decile5         = 0.08,
    decile6         = 0.09,
    decile7         = 0.10,
    decile8         = 0.11,
    decile9         = 0.12,
    decile10        = 0.13
  )
  res <- add_dist_stats_old(df = df, dist_stats = dist_stats)
  expect_true("gini" %in% names(res))
  expect_equal(res$gini, 0.35)
})

test_that("add_dist_stats_old yields NA for unmatched rows", {
  df <- data.table(
    cache_id        = "ZZZ_1990",   # not in dist_stats
    reporting_level = "national",
    headcount       = 0.5
  )
  dist_stats <- data.table(
    cache_id        = "AAA_2000",
    reporting_level = "national",
    gini            = 0.35,
    polarization    = 0.20,
    mld             = 0.18,
    decile1         = 0.04,
    decile2         = 0.05,
    decile3         = 0.06,
    decile4         = 0.07,
    decile5         = 0.08,
    decile6         = 0.09,
    decile7         = 0.10,
    decile8         = 0.11,
    decile9         = 0.12,
    decile10        = 0.13
  )
  res <- add_dist_stats_old(df = df, dist_stats = dist_stats)
  expect_true(is.na(res$gini))
})

test_that("add_dist_stats_old preserves original row count", {
  df <- data.table(
    cache_id        = c("AAA_2000", "BBB_2005"),
    reporting_level = c("national", "national"),
    headcount       = c(0.3, 0.4)
  )
  dist_stats <- data.table(
    cache_id        = "AAA_2000",
    reporting_level = "national",
    gini            = 0.35,
    polarization    = 0.20,
    mld             = 0.18,
    decile1         = 0.04,
    decile2         = 0.05,
    decile3         = 0.06,
    decile4         = 0.07,
    decile5         = 0.08,
    decile6         = 0.09,
    decile7         = 0.10,
    decile8         = 0.11,
    decile9         = 0.12,
    decile10        = 0.13
  )
  res <- add_dist_stats_old(df = df, dist_stats = dist_stats)
  expect_equal(nrow(res), 2L)
})


# add_agg_medians() fill_gaps = TRUE -----------------------------------------

test_that("add_agg_medians with fill_gaps=TRUE sets median to NA_real_", {
  df <- data.table(
    country_code    = "AAA",
    reporting_year  = 2000L,
    welfare_type    = "consumption",
    reporting_level = "national",
    median          = 3.5
  )
  res <- add_agg_medians(df = df, fill_gaps = TRUE, data_dir = tempdir())
  # fill_gaps=TRUE branch unconditionally sets median := NA_real_
  expect_true(is.na(res$median))
})

test_that("add_agg_medians with fill_gaps=TRUE is a data.table", {
  df <- data.table(
    country_code    = "AAA",
    reporting_year  = 2000L,
    welfare_type    = "consumption",
    reporting_level = "national",
    median          = 3.5
  )
  res <- add_agg_medians(df = df, fill_gaps = TRUE, data_dir = tempdir())
  expect_s3_class(res, "data.table")
})


# add_distribution_type() relies on lkup / data files: skip -----------------

test_that("add_distribution_type with invalid data_dir does not error silently", {
  skip("Requires lkup$ref_lkup and file system — integration only")
})


# get_mean_median() early-return when !use_new_lineup_version ----------------

test_that("get_mean_median returns input unchanged when use_new_lineup_version=FALSE", {
  fgt <- data.table(headcount = 0.3, poverty_gap = 0.1)
  lkup <- list(
    use_new_lineup_version = FALSE,
    svy_lkup     = list(),
    data_root    = tempdir(),
    return_cols  = list(),
    aux_files    = list(),
    cache_data_id = list()
  )
  res <- get_mean_median(fgt = fgt, lkup = lkup, fill_gaps = FALSE)
  expect_identical(res, fgt)
})
