# test-pip_lineups_postprocess.R
#
# Unit tests for pip_lineups_format_output().
# Uses minimal inline stub data — no external files required.

library(data.table)


# Helpers: minimal stubs ------------------------------------------------------

.cols <- c(
  "country_code", "reporting_year", "reporting_level", "welfare_type",
  "headcount", "poverty_gap", "poverty_severity", "watts",
  "poverty_line", "mean", "median",
  "gini", "polarization", "mld",
  "decile1", "decile2", "decile3", "decile4", "decile5",
  "decile6", "decile7", "decile8", "decile9", "decile10",
  "estimate_type"
)

# Minimal dist_stats skeleton (same shape that add_dist_stats_old() expects)
.make_dist_stats <- function() {
  data.table(
    cache_id         = "ZZZ_2000",
    reporting_level  = "national",
    gini             = 0.3,
    polarization     = 0.2,
    mld              = 0.15,
    decile1          = 0.05,
    decile2          = 0.06,
    decile3          = 0.07,
    decile4          = 0.08,
    decile5          = 0.09,
    decile6          = 0.10,
    decile7          = 0.11,
    decile8          = 0.12,
    decile9          = 0.13,
    decile10         = 0.14
  )
}

# censor_rows() expects:
#   lkup$censored to be a list with $countries and $regions sub-tables,
#   each with columns: id (character), statistic (character).
# The stub in .make_stub_lkup() uses an empty flat table — correct it here.
.make_censored_list <- function(country_code = character(0),
                                reporting_level = character(0)) {
  list(
    countries = data.table(
      id        = character(0),
      statistic = character(0)
    ),
    regions = data.table(
      id        = character(0),
      statistic = character(0)
    )
  )
}

# Minimal lkup that satisfies pip_lineups_format_output() accessors
.make_stub_lkup <- function() {
  list(
    return_cols  = list(pip = list(cols = .cols)),
    dist_stats   = .make_dist_stats(),
    lineup_dist_stats = .make_dist_stats(),
    data_root    = tempdir(),
    censored     = .make_censored_list(),
    # validate_lkup fields (not directly used here but present for consistency)
    svy_lkup              = list(),
    aux_files             = list(),
    cache_data_id         = list(hash_pip = "abc"),
    use_new_lineup_version = TRUE,
    interpolation_list    = list(),
    refy_lkup             = list(),
    query_controls        = list()
  )
}

# Minimal output data.table from a hypothetical upstream step
.make_out_dt <- function(country = "ZZZ", year = 2000L,
                         reporting_level = "national") {
  data.table(
    country_code    = country,
    reporting_year  = year,
    reporting_level = reporting_level,
    welfare_type    = "consumption",
    headcount       = 0.4,
    poverty_gap     = 0.2,
    poverty_severity = 0.12,
    watts           = 0.25,
    poverty_line    = 1.9,
    mean            = 3.5,
    median          = 2.8,
    cache_id        = paste0(country, "_", year)
  )
}


# Core output structure -------------------------------------------------------

test_that("pip_lineups_format_output returns a data.table", {
  out   <- .make_out_dt()
  lkup  <- .make_stub_lkup()
  res   <- pip_lineups_format_output(
    out              = out,
    lkup             = lkup,
    fill_gaps        = FALSE,
    reporting_level  = "all",
    censor           = FALSE,
    additional_ind   = FALSE,
    use_old_dist_stats = TRUE
  )
  expect_s3_class(res, "data.table")
})

test_that("output columns are exactly the names2keep set", {
  out   <- .make_out_dt()
  lkup  <- .make_stub_lkup()
  res   <- pip_lineups_format_output(
    out              = out,
    lkup             = lkup,
    fill_gaps        = FALSE,
    reporting_level  = "all",
    censor           = FALSE,
    additional_ind   = FALSE,
    use_old_dist_stats = TRUE
  )
  expect_equal(sort(names(res)), sort(.cols))
})


# reporting_level filtering ---------------------------------------------------

test_that("reporting_level filter keeps only matching rows", {
  out <- rbind(
    .make_out_dt(reporting_level = "national"),
    .make_out_dt(reporting_level = "urban")
  )
  out$cache_id <- c("ZZZ_2000", "ZZZ_2000")
  lkup <- .make_stub_lkup()
  res  <- pip_lineups_format_output(
    out              = out,
    lkup             = lkup,
    fill_gaps        = FALSE,
    reporting_level  = "national",
    censor           = FALSE,
    additional_ind   = FALSE,
    use_old_dist_stats = TRUE
  )
  expect_true(all(res$reporting_level == "national"))
})

test_that("reporting_level = 'all' retains all rows", {
  out <- rbind(
    .make_out_dt(reporting_level = "national"),
    .make_out_dt(reporting_level = "urban")
  )
  out$cache_id <- c("ZZZ_2000", "ZZZ_2000")
  lkup <- .make_stub_lkup()
  res  <- pip_lineups_format_output(
    out              = out,
    lkup             = lkup,
    fill_gaps        = FALSE,
    reporting_level  = "all",
    censor           = FALSE,
    additional_ind   = FALSE,
    use_old_dist_stats = TRUE
  )
  expect_equal(nrow(res), 2L)
})


# fill_gaps = TRUE sets estimate_type ----------------------------------------

test_that("fill_gaps=TRUE sets estimate_type column (not NA)", {
  skip("estimate_type_ctr_lnp requires full lkup — integration test only")
})


# fill_gaps = FALSE sets estimate_type to NA_character_ ----------------------

test_that("fill_gaps=FALSE sets estimate_type to NA_character_", {
  out  <- .make_out_dt()
  lkup <- .make_stub_lkup()
  res  <- pip_lineups_format_output(
    out              = out,
    lkup             = lkup,
    fill_gaps        = FALSE,
    reporting_level  = "all",
    censor           = FALSE,
    additional_ind   = FALSE,
    use_old_dist_stats = TRUE
  )
  expect_true(all(is.na(res$estimate_type)))
})


# Censoring ------------------------------------------------------------------
# censor_rows() builds tmp_id as:
#   {country_code}_{reporting_year}_{survey_acronym}_{welfare_type}_{reporting_level}
# and joins against lkup$censored$countries (id, statistic).

.make_censorable_dt <- function(country = "ZZZ") {
  data.table(
    country_code    = country,
    reporting_year  = 2000L,
    reporting_level = "national",
    welfare_type    = "consumption",
    survey_acronym  = "ZZZ_2000_XXX",
    headcount       = 0.4,
    poverty_gap     = 0.2,
    poverty_severity = 0.12,
    watts           = 0.25,
    poverty_line    = 1.9,
    mean            = 3.5,
    median          = 2.8,
    cache_id        = paste0(country, "_2000")
  )
}

test_that("censor=TRUE removes rows matching lkup$censored", {
  out  <- .make_censorable_dt(country = "ZZZ")
  lkup <- .make_stub_lkup()
  # Build the tmp_id that censor_rows will construct and flag it as "all"
  expected_id <- sprintf("%s_%s_%s_%s_%s",
    "ZZZ", 2000L, "ZZZ_2000_XXX", "consumption", "national")
  lkup$censored <- list(
    countries = data.table(id = expected_id, statistic = "all"),
    regions   = data.table(id = character(0), statistic = character(0))
  )
  res <- pip_lineups_format_output(
    out              = out,
    lkup             = lkup,
    fill_gaps        = FALSE,
    reporting_level  = "all",
    censor           = TRUE,
    additional_ind   = FALSE,
    use_old_dist_stats = TRUE
  )
  expect_equal(nrow(res), 0L)
})

test_that("censor=FALSE does not remove any rows", {
  out  <- .make_censorable_dt(country = "ZZZ")
  lkup <- .make_stub_lkup()
  expected_id <- sprintf("%s_%s_%s_%s_%s",
    "ZZZ", 2000L, "ZZZ_2000_XXX", "consumption", "national")
  lkup$censored <- list(
    countries = data.table(id = expected_id, statistic = "all"),
    regions   = data.table(id = character(0), statistic = character(0))
  )
  res <- pip_lineups_format_output(
    out              = out,
    lkup             = lkup,
    fill_gaps        = FALSE,
    reporting_level  = "all",
    censor           = FALSE,
    additional_ind   = FALSE,
    use_old_dist_stats = TRUE
  )
  expect_equal(nrow(res), 1L)
})


# Output is sorted -----------------------------------------------------------

test_that("output is sorted by country_code, reporting_year", {
  out <- rbind(
    .make_out_dt(country = "ZZZ", year = 2010L),
    .make_out_dt(country = "AAA", year = 2005L)
  )
  out$cache_id <- c("ZZZ_2010", "AAA_2005")
  lkup <- .make_stub_lkup()
  res  <- pip_lineups_format_output(
    out              = out,
    lkup             = lkup,
    fill_gaps        = FALSE,
    reporting_level  = "all",
    censor           = FALSE,
    additional_ind   = FALSE,
    use_old_dist_stats = TRUE
  )
  expect_equal(res$country_code[1], "AAA")
  expect_equal(res$country_code[2], "ZZZ")
})
