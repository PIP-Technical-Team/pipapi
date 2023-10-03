skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

data_dir <- fs::path(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkups <- create_versioned_lkups(data_dir = data_dir)
lkup <-  lkups$versions_paths[[lkups$latest_release]]

ctr_alt_agg <- c("BEN", "BFA", "CAF", "CIV", "CMR", "COG", "CPV", "GAB", "GHA", "GIN",
                 "GMB", "GNB", "GNQ", "LBR", "MLI", "MRT", "NER", "NGA", "SEN", "SLE",
                 "TCD", "TGO")

aux_files   <- readRDS(testthat::test_path("testdata", "mock_aux_files.rds"))
valid_years <- readRDS(testthat::test_path("testdata", "mock_valid_years.rds"))

md <- aux_files$missing_data

test_that("filter_md works when year is passed as a character", {
  # single year
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = "1983")

  expect_equal(length(unique(out$year)), 1)
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))

  # 2 years
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = c("1983", "2000"))

  expect_equal(length(unique(out$year)), 2)
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))

  # All years
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = "ALL")

  expect_equal(length(unique(out$year)), length(unique(md$year)))
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))
})

test_that("filter_md works when year is passed as a numeric", {
  # single year
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = 1983)

  expect_equal(length(unique(out$year)), 1)
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))

  # 2 years
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = c(1983, 2000))

  expect_equal(length(unique(out$year)), 2)
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))
})

test_that("create_vector_countries works for regions selection", {
  country <- "ALL"
  year = "2010"
  out <- create_countries_vctr(country = country,
                               year    = year,
                               valid_years = valid_years,
                               aux_files = aux_files)

  # Returns official and non-official regions when country="ALL"
  expect_equal(out$user_aggs, out$all_agg)
  # List of all aggregates is correct (no duplicates for instance)
  expect_equal(out$all_agg,  c("EAP",
                               "ECA",
                               "LAC",
                               "MNA",
                               "OHI",
                               "SAS",
                               "SSA",
                               "WLD",
                               "AFE",
                               "AFW",
                               "WLD"))

  expect_equal(out$ctr_off_reg, out$est_ctrs)

  expect_true(all(out$user_off_reg %in% out$off_reg))
  expect_true(all(out$user_alt_agg %in% out$alt_agg))
})
