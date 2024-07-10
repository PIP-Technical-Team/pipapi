options(joyn.verbose = FALSE)
# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")

skip_if(data_dir == "")

latest_version <-
  available_versions(data_dir) |>
  max()

lkups <- create_versioned_lkups(data_dir,
                                vintage_pattern = latest_version)
lkup <- lkups$versions_paths[[lkups$latest_release]]


# default parameters

lp <- list(
  year             = c(1990, 2000),
  povline          =  2.25,
  group_by         =  c("wb"),
  welfare_type     =  c("all"),
  reporting_level  =  c("all"),
  censor           =  TRUE,
  lkup             =  lkup
)


test_that("pip_grp and pip_grp_logic give the same results for official and alternative region selection", {

  ## regular official regions
  country <- c("SSA", "LAC")
  year    <- c(2000, 2019) # out of range
  lp$country <- country
  lp$year    <- year

  de1 <- do.call(pip_grp_logic, lp)
  dc <- do.call(pip_grp, lp)
  expect_equal(de1, dc, label = "same results for official aggregates
               between grp and grp_logic")

  # One alternative aggregate within one  official  region
  country <- c("AFE", "SSA", "LAC")
  lp$country <- country

  de2 <- do.call(pip_grp_logic, lp)
  de3 <- de2[region_code %in% c("SSA", "LAC")]
  data.table::setcolorder(de3, names(dc))
  data.table::setorder(de3, region_code, reporting_year)
  data.table::setorder(dc, region_code, reporting_year)

  expect_equal(de3, dc, label = "same results for official aggregates
               between grp and grp_logic when alt aggregate is included")



  ## multiple alternative aggregations  and official  aggregation
  country <- c("AFE", "SSA", "LAC", "AFW", "LIC")
  lp$country <- country

  de4 <- do.call(pip_grp_logic, lp)
  de5 <- de4[region_code %in% c("SSA", "LAC")]
  data.table::setcolorder(de5, names(dc))
  data.table::setorder(de5, region_code, reporting_year)

  expect_equal(de5, dc, label = "same results for official aggregates
               between grp and grp_logic when alt aggregate is included")

})

test_that("pip_grp_logic selection works correctly", {
  # Returns ALL official + alternative regions when country = "ALL"
  country = "ALL"
  tmp = pip_grp_logic(country         = country,
                      year            = 2010,
                      group_by = "wb",
                      lkup            = lkup)
  expect_equal(tmp$region_code, lkup$aux_files$regions$region_code)

  # Returns only "WLD" when country = "WLD"
  country = "WLD"
  tmp = pip_grp_logic(country         = country,
                      year            = 2010,
                      group_by = "wb",
                      lkup            = lkup)
  expect_equal(tmp$region_code, country)

  # Returns only "AFE" when country = "AFE"
  country = "AFE"
  tmp = pip_grp_logic(country         = country,
                      year            = 2010,
                      group_by = "wb",
                      lkup            = lkup)
  expect_equal(tmp$region_code, country)

  # Returns only "SSA" when country = "SSA"
  country = "SSA"
  tmp = pip_grp_logic(country         = country,
                      year            = 2010,
                      group_by = "wb",
                      lkup            = lkup)
  expect_equal(tmp$region_code, country)

  # Returns correct results when mixing official alternative regions
  country = c("AFE", "LAC")
  tmp = pip_grp_logic(country         = country,
                      year            = 2010,
                      group_by = "wb",
                      lkup            = lkup)
  expect_equal(tmp$region_code, country)

  # Returns correct results when mixing official, alternative, and "WLD" regions
  country = c("AFE", "LAC", "WLD")
  tmp = pip_grp_logic(country         = country,
                      year            = 2010,
                      group_by = "wb",
                      lkup            = lkup)
  expect_equal(tmp$region_code, country)
})
