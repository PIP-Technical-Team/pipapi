ref_lkup <- fst::read_fst("../testdata/app_data/20210401/estimations/interpolated_means.fst")
ref_lkup$region_code <- ref_lkup$wb_region_code
valid_regions <- sort(unique(ref_lkup$region_code))
#Using fake dataset
fake_data <- fst::read_fst("../pip-fake-data/20200101_2011_01_01_PROD/estimations/interpolated_means.fst")

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

## Writing tests using fake dataset, probably we'll need to change this tests later to use ref_lkup instead
test_that("select_country returns correct logical values", {
  keep <- select_country(fake_data, "ARG", c("LAC", "SSA"))
  expect_equal(keep, rep(c(FALSE, TRUE, FALSE), c(145, 10, 54)))

  keep <- select_country(fake_data, "WLD", c("LAC", "SSA"))
  expect_true(keep)

  keep <- select_country(fake_data, c("SSA","EAP"), c("LAC", "SSA", "EAP"))
  expect_equal(keep, rep(c(TRUE, FALSE, TRUE), c(145, 46, 18)))
})


test_that("select_years returns correct logical values", {
  keep <- select_country(fake_data, "ARG", c("LAC", "SSA"))
  keep <- select_years(fake_data, keep, 1945, "ARG")
  expect_false(any(keep))

  keep <- select_country(fake_data, "WLD", c("LAC", "SSA"))
  keep <- select_years(fake_data, keep, 2000, "WLD")
  expect_equal(keep, rep(c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), c(41, 1, 77, 1, 4, 1, 84)))

  keep <- select_country(fake_data, c("SSA","EAP"), c("LAC", "SSA", "EAP"))
  keep <- select_years(fake_data, keep, 2000:2010, c("SSA","EAP"))
  expect_equal(keep, rep(c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                         c(41L, 15L, 63L, 3L, 2L, 8L, 59L, 14L, 4L)))
})
