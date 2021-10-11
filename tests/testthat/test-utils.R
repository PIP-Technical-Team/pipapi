ref_lkup <- fst::read_fst("../testdata/app_data/20210401/estimations/interpolated_means.fst")
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
  expect_equal(sum(tmp), 173)

  tmp <- select_reporting_level(lkup = ref_lkup,
                                keep = keep,
                                reporting_level = "urban")
  # Deals with the case of Argentina: 6 records where survey covertage is "urban"
  # while reporting_level is "national"
  # CHECK THAT THIS IS THE CORRECT BEHAVIOR
  expect_equal(sum(tmp), 24)

  tmp <- select_reporting_level(lkup = ref_lkup,
                                keep = keep,
                                reporting_level = "rural")
  # CHECK THAT THIS IS THE CORRECT BEHAVIOR
  expect_equal(sum(tmp), 18)
})
