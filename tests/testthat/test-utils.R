# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")
library(data.table)

lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkups <- lkups$versions_paths[[lkups$latest_release]]

int_means_path <- fs::path(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"),
                           "20220810_2017_01_02_PROD/estimations/interpolated_means.fst")

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

