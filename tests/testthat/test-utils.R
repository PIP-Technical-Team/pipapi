# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")
library(data.table)

lkups          <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
latest_release <- lkups$latest_release
lkups          <- lkups$versions_paths[[lkups$latest_release]]

int_means_path <- fs::path(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"),
                           latest_release,
                           "/estimations/interpolated_means.fst")


ref_lkup <- fst::read_fst(int_means_path, as.data.table = TRUE)
ref_lkup$region_code <- ref_lkup$wb_region_code
valid_regions <- sort(unique(ref_lkup$region_code))
# ref_lkup <- fst::read_fst("./tests/testdata/app_data/20210401/estimations/interpolated_means.fst")

test_that("select_reporting_level is working as expected", {
  keep <- rep(TRUE, nrow(ref_lkup))
  tmp <- select_reporting_level(lkup = ref_lkup,
                                keep = keep,
                                reporting_level = "all")

  expect_equal(sum(keep), sum(tmp))

  tmp <- select_reporting_level(lkup = ref_lkup,
                                keep = keep,
                                reporting_level = "national")
  # Accounting for aggregate distribution does make a difference here
  # CHECK THAT THIS IS THE CORRECT BEHAVIOR

  nat_n <- ref_lkup[reporting_level == "national" | is_used_for_aggregation == TRUE , .N ]
  expect_equal(sum(tmp), nat_n)

  tmp <- select_reporting_level(lkup = ref_lkup,
                                keep = keep,
                                reporting_level = "urban")
  # Deals with the case of Argentina: 6 records where survey covertage is "urban"
  # while reporting_level is "national"
  # CHECK THAT THIS IS THE CORRECT BEHAVIOR

  urb_n <- ref_lkup[reporting_level == "urban", .N ]
  expect_equal(sum(tmp), urb_n)

  tmp <- select_reporting_level(lkup = ref_lkup,
                                keep = keep,
                                reporting_level = "rural")
  # CHECK THAT THIS IS THE CORRECT BEHAVIOR
  rur_n <- ref_lkup[reporting_level == "rural", .N ]
  expect_equal(sum(tmp), rur_n)
})

test_that("subset_lkup correctly selects all countries", {
  tmp <- subset_lkup(country         = "all",
                     year            = "all",
                     welfare_type    = "all",
                     reporting_level = "all",
                     lkup            = ref_lkup,
                     valid_regions = valid_regions)

  expect_equal(nrow(tmp), nrow(ref_lkup))
})

test_that("subset_lkup correctly selects countries", {
  selection <- c("AGO", "THA")
  tmp <- subset_lkup(country         = selection,
                     year            = "all",
                     welfare_type    = "all",
                     reporting_level = "all",
                     lkup            = ref_lkup,
                     valid_regions = valid_regions)

  expect_equal(sort(unique(tmp$country_code)), sort(selection))
})

test_that("subset_lkup correctly selects single regions", {
  selection <- "SSA"
  tmp <- subset_lkup(country         = selection,
                     year            = "all",
                     welfare_type    = "all",
                     reporting_level = "all",
                     lkup            = ref_lkup,
                     valid_regions = valid_regions)

  expect_equal(sort(unique(tmp$region_code)), sort(selection))
})

test_that("subset_lkup correctly selects multiple regions", {
  selection <- c("LAC", "SSA")
  tmp <- subset_lkup(country         = selection,
                     year            = "all",
                     welfare_type    = "all",
                     reporting_level = "all",
                     lkup            = ref_lkup,
                     valid_regions = valid_regions)

  expect_equal(sort(unique(tmp$region_code)), sort(selection))
})

test_that("subset_lkup correctly selects countries and regions", {

  region_selection <- "LAC"
  country_selection <- c("AGO", "THA")
  selection <- c(region_selection, country_selection)

  tmp <- subset_lkup(country         = selection,
                     year            = "all",
                     welfare_type    = "all",
                     reporting_level = "all",
                     lkup            = ref_lkup,
                     valid_regions = valid_regions)

  # Regions are selected
  expect_true(all(region_selection %in% (unique(tmp$region_code))))
  # Countries are selected
  expect_true(all(country_selection %in% (unique(tmp$country_code))))
})

# select_country() test suite
test_that("select_country works for complete country selection", {

  expected_countries <- nrow(ref_lkup)
  keep <- rep(TRUE, expected_countries)

  keep <- select_country(ref_lkup, keep, "all")
  expect_equal(length(keep), expected_countries)
  expect_equal(all(keep), TRUE)

  keep <- select_country(ref_lkup, keep, "WLD")
  expect_equal(length(keep), expected_countries)
  expect_equal(all(keep), TRUE)

  keep <- select_country(ref_lkup, keep, c("WLD", "COL"))
  expect_equal(length(keep), expected_countries)
  expect_equal(all(keep), TRUE)
})

test_that("select_country works for region selection", {

  region <- "SSA"
  expected_countries <- nrow(ref_lkup[ref_lkup$region_code == region, ])
  keep <- rep(TRUE, nrow(ref_lkup))

  keep <- select_country(ref_lkup, keep, region, valid_regions = valid_regions)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_countries)
  expect_equal(unique(ref_lkup$pcn_region_code[keep]), region)
})

test_that("select_country works for country selection", {

  country <- "COL"
  expected_countries <- nrow(ref_lkup[ref_lkup$country_code == country, ])
  keep <- rep(TRUE, nrow(ref_lkup))

  keep <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_countries)
  expect_equal(unique(ref_lkup$country_code[keep]), country)

  country <- c("COL", "YEM", "ZMB")
  expected_countries <- nrow(ref_lkup[ref_lkup$country_code %in% country, ])
  keep <- rep(TRUE, nrow(ref_lkup))

  keep <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_countries)
  expect_equal(sort(unique(ref_lkup$country_code[keep])), country)
})


test_that("select_country works for country & region selection", {

  country <- "COL"
  region <- "SSA"
  row_keep <- ref_lkup$country_code %in% country | ref_lkup$region_code %in% region
  expected_countries <- nrow(ref_lkup[row_keep, ])
  keep <- rep(TRUE, nrow(ref_lkup))

  keep <- select_country(ref_lkup, keep, c(country, region), valid_regions = valid_regions)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_countries)
  expect_true(all(country %in% unique(ref_lkup$country_code[keep])))
  expect_true(all(region %in% unique(ref_lkup$region_code[keep])))
})

# Most recent Value ---------
test_that("select_years works for most recent value", {
  # Single country
  country <- "BFA"
  year <- "mrv"
  keep <- rep(TRUE, nrow(ref_lkup))
  keep_country <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)
  tmp <- ref_lkup[keep_country, ]
  mrv_year <- max(tmp$reporting_year)
  expected_countries <- nrow(tmp[tmp$reporting_year == mrv_year, ])

  keep <- select_years(ref_lkup, keep_country, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_countries)
  expect_equal(unique(ref_lkup$reporting_year[keep]), mrv_year)

  # Multiple countries
  country <- c("BFA", "CAN")
  year <- "mrv"
  keep <- rep(TRUE, nrow(ref_lkup))
  keep_country <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)
  tmp <- ref_lkup[keep_country, ]
  mrv_year <- tmp[country_code %in% country,
                  .SD[which.max(reporting_year)],
                  by = country_code
                  ][,
                    reporting_year]

  expected_countries <- nrow(tmp[reporting_year %in% mrv_year, ])

  keep <- select_years(ref_lkup, keep_country, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_countries)
  expect_equal(sort(unique(ref_lkup$reporting_year[keep])), sort(unique(mrv_year)))

  # All countries
  country <- "all"
  year <- "mrv"
  keep <- rep(TRUE, nrow(ref_lkup))
  keep_country <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)
  tmp <- ref_lkup[keep_country, ]

  mrv_year <- tmp[, max_year := reporting_year == max(reporting_year),
                  by = country_code
                  ][max_year == TRUE,
                    reporting_year]

  expected_countries <- length(unique(ref_lkup$country_code)) # Here we expect a single year to be returned for each single country

  keep <- select_years(ref_lkup, keep_country, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))

  # expect_equal(sum(keep), expected_countries)
  expect_equal(sort(ref_lkup$reporting_year[keep]), sort(mrv_year))
})

test_that("select_years works for all year", {
  # Single country
  country <- "BFA"
  year <- "all"
  keep <- rep(TRUE, nrow(ref_lkup))
  keep_country <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)

  keep <- select_years(ref_lkup, keep_country, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), sum(keep_country))

  # Multiple countries
  country <- c("BFA", "CAN")
  year <- "all"
  keep <- rep(TRUE, nrow(ref_lkup))
  keep_country <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)

  keep <- select_years(ref_lkup, keep_country, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), sum(keep_country))

  # All countries
  country <- "all"
  year <- "all"
  keep <- rep(TRUE, nrow(ref_lkup))
  keep_country <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)

  keep <- select_years(ref_lkup, keep_country, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), sum(keep_country))
})

test_that("select_years works for specific year selections", {
  # Single year
  country <- "BFA"
  year <- 2008
  keep <- rep(TRUE, nrow(ref_lkup))
  keep_country <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)
  tmp <- ref_lkup[keep_country, ]
  expected_countries <- nrow(tmp[tmp$reporting_year %in% year, ])

  keep <- select_years(ref_lkup, keep_country, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_countries)
  expect_equal(unique(ref_lkup$reporting_year[keep]), year)

  # Multiple years
  country <- c("BFA", "CAN")
  year <- c(2008, 2010, 2018)
  keep <- rep(TRUE, nrow(ref_lkup))
  keep_country <- select_country(ref_lkup, keep, country, valid_regions = valid_regions)
  tmp <- ref_lkup[keep_country, ]
  expected_countries <- nrow(tmp[tmp$reporting_year %in% year, ])

  keep <- select_years(ref_lkup, keep_country, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_countries)
  expect_equal(sort(unique(ref_lkup$reporting_year[keep])), sort(year))

  # Multiple years + MRV
  # We expect all countries with their MRV year + all country/years not part of MRV query
  # This not currently working and specific are being dropped
  # This test is expected to fail once the correct behavior is implemented
  country <- "all"
  not_mrv_year <- "1991"
  year <- c("mrv", not_mrv_year)
  keep <- rep(TRUE, nrow(ref_lkup))
  mrv_year <- ref_lkup[, .SD[which.max(reporting_year)],
                       by = country_code]$reporting_year
  expected_row_mrv <- length(unique(ref_lkup$country_code)) # Here we expect a single year to be returned for each single country
  expected_row_not_mrv <- nrow(ref_lkup[ref_lkup$reporting_year == not_mrv_year, ])

  keep <- select_years(ref_lkup, keep, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_row_mrv)
  expect_equal(sort(unique(ref_lkup$reporting_year[keep])),
               sort(unique(mrv_year)))
})

skip("Specific year selections are dropped when MRV is selected")
test_that("select_years works for MRV + specific year selections", {

  # Multiple years + MRV
  # We expect all countries with their MRV year + all country/years not part of MRV query
  country <- "all"
  not_mrv_year <- "1991"
  year <- c("mrv", not_mrv_year)
  keep <- rep(TRUE, nrow(ref_lkup))
  mrv_year <- ref_lkup[, .SD[which.max(reporting_year)],
                       by = country_code]$reporting_year
  expected_row_mrv <- length(unique(ref_lkup$country_code)) # Here we expect a single year to be returned for each single country
  expected_row_not_mrv <- nrow(ref_lkup[ref_lkup$reporting_year == not_mrv_year, ])

  keep <- select_years(ref_lkup, keep, year, country = country)
  expect_equal(length(keep), nrow(ref_lkup))
  expect_equal(sum(keep), expected_row_mrv + expected_row_not_mrv)
  expect_equal(sort(unique(ref_lkup$reporting_year[keep])),
               sort(unique(c(mrv_year, as.numeric(not_mrv_year)))))
})

