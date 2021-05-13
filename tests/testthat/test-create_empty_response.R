library(dplyr)

# main <- fst::read_fst("../../TEMP/data/DB/main.fst") %>%
#   filter(distribution_type == "micro")
# # pfw$surveyid_year <- as.character(pfw$surveyid_year)
# svy_lkup <- fst::read_fst("../../TEMP/data/DB/dsm.fst") %>%
#   filter(distribution_type == "micro")
# paths <- fs::dir_ls("../../TEMP/data/survey_data/", recurse = FALSE, type = "file")
#
# svy_lkup <- svy_lkup %>%
#   left_join(ppp)
# ref_lkup <- ref_lkup %>%
#   left_join(ppp)
# lkups <- list(svy_lkup = svy_lkup,
#               ref_lkup = ref_lkup)
#
#
# tmp <- pip(country = "all",
#            year    = "all",
#            povline = 3.5,
#            lkup = lkups,
#            paths = paths)
#
#
# test_that("create_empty_response returns expected columns", {
#   empty_response <- create_empty_response()
#   expect_equal(names(empty_response), names(tmp))
# })
