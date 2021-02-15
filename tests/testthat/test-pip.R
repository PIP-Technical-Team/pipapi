library(dplyr)

main <- fst::read_fst("../../TEMP/data/DB/main.fst") %>%
  filter(distribution_type == "micro")
# pfw$surveyid_year <- as.character(pfw$surveyid_year)
svy_lkup <- fst::read_fst("../../TEMP/data/DB/dsm.fst") %>%
  filter(distribution_type == "micro")
ref_lkup <- fst::read_fst("../../TEMP/data/DB/refyear.fst") %>%
  filter(distribution_type == "micro")

paths <- fs::dir_ls("../../TEMP/data/survey_data/", recurse = FALSE, type = "file")

lkups <- list(svy_lkup = svy_lkup,
              ref_lkup = ref_lkup)

test_that("output type is correct", {

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = lkups,
             paths = paths)

  expect_equal(class(tmp), "data.frame")
})

test_that("welfare_type selection are correct", {

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = svy_lkup,
             welfare_type = "all",
             paths = paths)

  expect_equal(sort(unique(tmp$welfare_type)), c("consumption", "income"))

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = svy_lkup,
             welfare_type = "consumption",
             paths = paths)

  expect_equal(unique(tmp$welfare_type), "consumption")

  tmp <- pip(country = "all",
             year    = "all",
             povline = 3.5,
             lkup = svy_lkup,
             welfare_type = "income",
             paths = paths)

  expect_equal(unique(tmp$welfare_type), "income")
})

