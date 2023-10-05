skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

data_dir <- fs::path(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkups <- create_versioned_lkups(data_dir = data_dir)
lkup <-  lkups$versions_paths[[lkups$latest_release]]

ctr_alt_agg <- c("BEN", "BFA", "CAF", "CIV", "CMR", "COG", "CPV", "GAB", "GHA", "GIN",
                 "GMB", "GNB", "GNQ", "LBR", "MLI", "MRT", "NER", "NGA", "SEN", "SLE",
                 "TCD", "TGO")

aux_files   <- readRDS(testthat::test_path("testdata", "mock_aux_files.rds"))
valid_years <- readRDS(testthat::test_path("testdata", "mock_valid_years.rds"))
aggs      <- aux_files$regions
off_gt <-  c("region") # c("region", "world")--
off_reg <- aggs[["region_code"]][aggs[["grouping_type"]] %in% off_gt]
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

test_that("select_off_alt_agg works as expected", {
  skip("TEMPORARY SKIP")
  tmp <- select_off_alt_agg(user_gt = "", off_gt = "region")

  expect_true(FALSE)




})

test_that("select_user_aggs works as expected", {
  # All official regions are returned when user selects "ALL" or "WLD"
  tmp <- select_user_aggs(country = "ALL",
                          off_reg = off_reg,
                          aggs = aggs)
  expect_equal(tmp, off_reg)

  tmp <- select_user_aggs(country = "WLD",
                          off_reg = off_reg,
                          aggs = aggs)
  expect_equal(tmp, off_reg)

  # If user selects specific regions, these regions are being returned
  region = "AFE"
  tmp <- select_user_aggs(country = region,
                          off_reg = off_reg,
                          aggs = aggs)
  expect_equal(tmp, region)

  region = c("AFE", "AFW", "LAC")
  tmp <- select_user_aggs(country = region,
                          off_reg = off_reg,
                          aggs = aggs)
  expect_equal(tmp, region)

  # Empty vector is being returned is user selects a country code
  country = "COL"
  tmp <- select_user_aggs(country = country,
                          off_reg = off_reg,
                          aggs = aggs)
  expect_equal(tmp, character(0))

  country = c("COL", "FRA")
  tmp <- select_user_aggs(country = country,
                          off_reg = off_reg,
                          aggs = aggs)
  expect_equal(tmp, character(0))

  # When mixing countries and regions, countries are being ignored
  country = c("COL", "FRA")
  region  = "LAC"
  tmp <- select_user_aggs(country = c(country, region),
                          off_reg = off_reg,
                          aggs = aggs)
  expect_equal(tmp, region)
})

test_that("create_vector_countries output the expected object", {
  skip("TEMPORARY SKIP")
  country <- "ALL"
  year = "2010"
  out <- create_countries_vctr(country = country,
                               year    = year,
                               valid_years = valid_years,
                               aux_files = aux_files)
  expect_true(is.list(out))
  expect_equal(sort(names(out)),
               sort(c("user_off_reg",
                      "user_alt_agg",
                      "est_ctrs",
                      "md_off_reg",
                      "md_year",
                      "grp_use",
                      "md",
                      "user_alt_gt_code"))
  )
})

test_that("create_vector_countries works for countries selection", {
  skip("TEMPORARY SKIP")
  country <- "ALL"
  year = "ALL"
  out <- create_countries_vctr(country = country,
                               year    = year,
                               valid_years = valid_years,
                               aux_files = aux_files)

  # Selects all countries with survey data when country="ALL"
  expect_equal(sort(out$est_ctrs), sort(aux_files$countries$country_code))
})

test_that("create_vector_countries works for regions selection", {
  skip("TEMPORARY SKIP")
  country <- "ALL"
  year = "2010"
  out <- create_countries_vctr(country = country,
                               year    = year,
                               valid_years = valid_years,
                               aux_files = aux_files)

  # Returns official and non-official regions when country="ALL"
  expect_equal(out$user_off_reg,
               aux_files$regions$region_code[aux_files$regions$grouping_type == "region"])
  # List of all aggregates is correct (no duplicates for instance)
  expect_equal(out$user_alt_agg,
               aux_files$regions$region_code[!aux_files$regions$grouping_type %in% c("region", "world")])

  # Correctly selects alternative regions
  country <- c("AFE")
  year = "ALL"
  out <- create_countries_vctr(country = country,
                               year    = year,
                               valid_years = valid_years,
                               aux_files = aux_files)

  # Returns no official region
  expect_true(is.null(out$user_off_reg))
  # Returns selected alternative region
  expect_equal(out$user_alt_agg, country)
  # Selects all SSA countries for estimation: ARE THOSE TESTS CORRECT?
  expect_equal(out$est_ctrs, aux_files$countries$country_code[aux_files$countries$region_code == "SSA"])
  # Selects correct missing data region
  expect_equal(out$md_off_reg, "SSA")
  # Selects years with missing data - NEED TO CHECK THE LOGIC
  # expect_equal(out$md_year, valid_years$valid_survey_years)


})
