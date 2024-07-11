# Disable until a full set of anonymous package data has been created
# skip("Disable until a full set of anonymous package data has been created")

# # Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip("This test is fully repeated in pip-local")
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

# files <- sub("[.]fst", "", list.files("../testdata/app_data/20210401/survey_data/"))
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkup <- lkups$versions_paths[[lkups$latest_release]]

# lkup_path <- test_path("testdata", "lkup.rds")
# lkup      <- readRDS(lkup_path)


test_that("Reporting level filtering is working", {
  reporting_levels <- c("national", "urban", "rural", "all")
  tmp <- lapply(reporting_levels,
                function(x) {
                  pip(
                    country         = "CHN",
                    year            = "2008",
                    povline         = 1.9,
                    popshare        = NULL,
                    welfare_type    = "all",
                    reporting_level = x,
                    fill_gaps       = FALSE,
                    ppp             = 10,
                    lkup            = lkup
                  )
                })
  names(tmp) <- reporting_levels

  expect_equal(nrow(tmp$national), 1)
  expect_equal(tmp$national$reporting_level, "national")

  expect_equal(nrow(tmp$urban), 1)
  expect_equal(tmp$urban$reporting_level, "urban")

  expect_equal(nrow(tmp$rural), 1)
  expect_equal(tmp$rural$reporting_level, "rural")

  expect_equal(nrow(tmp$all), 3)
  expect_equal(sort(tmp$all$reporting_level), c("national", "rural", "urban"))
  })

# Use only test data
# lkup$svy_lkup <- lkup$svy_lkup[(cache_id %in% files | country_code == "AGO")]
# lkup$ref_lkup <- lkup$ref_lkup[(cache_id %in% files | country_code == "AGO")]
