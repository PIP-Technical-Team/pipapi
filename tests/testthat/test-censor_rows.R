# constants — loaded once at file scope; skip entire file if fixtures are absent
.fixture_files <- c(
  censored  = test_path("testdata", "censored.rds"),
  censored2 = test_path("testdata", "censored-2.rds"),
  reg_agg   = test_path("testdata", "ohi-sample.rds"),
  chn       = test_path("testdata", "chn-2016.rds")
)
.missing_fixtures <- .fixture_files[!file.exists(.fixture_files)]
if (length(.missing_fixtures) > 0L) {
  skip(paste("Missing fixture files:", paste(names(.missing_fixtures), collapse = ", ")))
}

censored  <- readRDS(.fixture_files[["censored"]])
censored2 <- readRDS(.fixture_files[["censored2"]])
reg_agg   <- readRDS(.fixture_files[["reg_agg"]])
chn       <- readRDS(.fixture_files[["chn"]])

test_that("censor_rows() removes entire row when statistic is 'all'", {

  # Country table
  res <- censor_rows(chn, censored, type = "countries")
  expect_equal(nrow(res), 0)
  expect_equal(names(chn), names(res))

  # Region table
  res <- censor_rows(reg_agg, censored, type = "regions")
  expect_equal(nrow(res), 3)
  expect_false(all(censored$region$reporting_year %in%
                     res$reporting_year))

  expect_equal(reg_agg$reporting_pop[1:3], res$reporting_pop[1:3])
  expect_equal(reg_agg$headcount[1:3], res$headcount[1:3])
  expect_equal(reg_agg$poverty_gap[1:3], res$poverty_gap[1:3])
  expect_equal(reg_agg$poverty_severity[1:3], res$poverty_severity[1:3])
  expect_equal(reg_agg$watts[1:3], res$watts[1:3])
  expect_equal(reg_agg$mean[1:3], res$mean[1:3])
  expect_equal(reg_agg$pop_in_poverty[1:3], res$pop_in_poverty[1:3])

})

test_that("censor_rows() sets specific stats to NA_real_", {

  # Country table
  res <- censor_rows(chn, censored2, type = "countries")
  expect_equal(nrow(res), 3)

  # Check that stats are correctly set to NA
  expect_equal(unique(res$headcount), NA_real_)
  expect_equal(unique(res$gini), NA_real_)
  expect_equal(unique(res$mld), NA_real_)

  # Check that other stats idn't change
  expect_equal(res[,!c("headcount", "mld", "gini")],
               chn[,!c("headcount","mld", "gini")])

  # Region table
  res <- censor_rows(reg_agg, censored2, type = "regions")
  expect_equal(nrow(res), 5)

  # Check that stats are correctly set to NA
  expect_equal(unique(res$watts[4:5]), NA_real_)
  expect_equal(unique(res$headcount[4:5]), NA_real_)
  expect_equal(unique(res$mean[4:5]), NA_real_)

  # Check that other stats (for same indicators) didn't change
  expect_equal(reg_agg$headcount[1:3], res$headcount[1:3])
  expect_equal(reg_agg$watts[1:3], res$watts[1:3])
  expect_equal(reg_agg$mean[1:3], res$mean[1:3])

  # Check that other stats (for other indicators) didn't change
  expect_equal(reg_agg$reporting_pop[1:5], res$reporting_pop[1:5])
  expect_equal(reg_agg$poverty_gap[1:5], res$poverty_gap[1:5])
  expect_equal(reg_agg$poverty_severity[1:5], res$poverty_severity[1:5])
  expect_equal(reg_agg$pop_in_poverty[1:5], res$pop_in_poverty[1:5])
})

test_that("censor_rows() returns early when there no censoring observations", {
  tmp <- list(regions = data.frame(
    region_code = character(0),
    reporting_year = numeric(0),
    statistic = character(0),
    id       = character(0)
  ))
  res <- censor_rows(reg_agg, tmp, type = "regions")
  expect_equal(res, reg_agg)
})


# censor_stats() pure-unit tests (synthetic data, no file dependency) --------

.make_censor_dt <- function() {
  data.table::data.table(
    tmp_id    = c("AAA_2000", "BBB_2005", "CCC_2010"),
    headcount = c(0.3, 0.4, 0.5),
    mean      = c(100, 200, 300),
    gini      = c(0.35, 0.40, 0.45)
  )
}

test_that("censor_stats: removes rows with statistic 'all'", {
  df <- .make_censor_dt()
  ct <- data.table::data.table(id = "AAA_2000", statistic = "all")
  res <- censor_stats(df, ct)
  expect_equal(nrow(res), 2L)
  expect_false("AAA_2000" %in% res$tmp_id)
})

test_that("censor_stats: sets specific statistic to NA (partial censor)", {
  df <- .make_censor_dt()
  ct <- data.table::data.table(id = "BBB_2005", statistic = "headcount")
  res <- censor_stats(df, ct)
  expect_equal(nrow(res), 3L)
  expect_true(is.na(res[tmp_id == "BBB_2005", headcount]))
  expect_false(is.na(res[tmp_id == "AAA_2000", headcount]))
})

test_that("censor_stats: leaves df unchanged with empty censor table", {
  df  <- .make_censor_dt()
  ct  <- data.table::data.table(id = character(0), statistic = character(0))
  res <- censor_stats(df, ct)
  expect_equal(nrow(res), 3L)
  expect_equal(res$headcount, df$headcount)
})

test_that("censor_stats: multiple 'all' rows each remove their row", {
  df <- .make_censor_dt()
  ct <- data.table::data.table(
    id        = c("AAA_2000", "CCC_2010"),
    statistic = c("all", "all")
  )
  res <- censor_stats(df, ct)
  expect_equal(nrow(res), 1L)
  expect_equal(res$tmp_id, "BBB_2005")
})

test_that("censor_stats: unmatched censor id leaves df unchanged", {
  df <- .make_censor_dt()
  ct <- data.table::data.table(id = "ZZZ_9999", statistic = "all")
  res <- censor_stats(df, ct)
  expect_equal(nrow(res), 3L)
})


# estimate_type initial labelling (pure logic, no file dependency) -----------

test_that("estimate_type initial: survey rows labelled 'actual'", {
  dt <- data.table::data.table(
    estimation_type = c("survey", "survey", "interpolated"),
    reporting_year  = c(2000L, 2005L, 2010L)
  )
  dt[, estimate_type := fifelse(estimation_type == "survey",
                                "actual", "projection")]
  expect_equal(dt$estimate_type[1:2], c("actual", "actual"))
})

test_that("estimate_type initial: non-survey rows labelled 'projection'", {
  dt <- data.table::data.table(
    estimation_type = c("interpolated", "extrapolated"),
    reporting_year  = c(2010L, 2015L)
  )
  dt[, estimate_type := fifelse(estimation_type == "survey",
                                "actual", "projection")]
  expect_true(all(dt$estimate_type == "projection"))
})
