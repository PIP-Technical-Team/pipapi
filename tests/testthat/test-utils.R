ref_lkup <- fst::read_fst("../testdata/app_data/20210401/estimations/interpolated_means.fst")
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
  expect_equal(sum(tmp), 179)

  tmp <- select_reporting_level(lkup = ref_lkup,
                                keep = keep,
                                reporting_level = "urban")
  # Deals with the case of Argentina: 6 records where survey covertage is "urban"
  # while reporting_level is "national"
  # CHECK THAT THIS IS THE CORRECT BEHAVIOR
  expect_equal(sum(tmp), 6)

  tmp <- select_reporting_level(lkup = ref_lkup,
                                keep = keep,
                                reporting_level = "rural")
  # CHECK THAT THIS IS THE CORRECT BEHAVIOR
  expect_equal(sum(tmp), 2)
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

test_that("create_folder_id selects approporiate version", {
  x1 <- c("20211212_2011_01_01_PROD", "20200101_2011_01_01_PROD")

  expect_equal(create_folder_id('20211212', '2011', x1), "20211212_2011_01_01_PROD")
  expect_equal(create_folder_id('20211212', '2012', x1), "404")
})

test_that("select_max_version select the max version correctly", {
  x1 <- c("20211212_2011_01_01_PROD", "20211212_2011_03_01_PROD")
  x2 <- c("20200101_2011_10_11_PROD", "20200101_2011_10_10_PROD")

  expect_equal(select_max_version(x1), "20211212_2011_03_01_PROD")
  expect_equal(select_max_version(x2), "20200101_2011_10_11_PROD")
})
