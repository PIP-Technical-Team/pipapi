options(joyn.verbose = FALSE)

# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

data_dir <- fs::path(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))

lkups <- create_versioned_lkups(data_dir = data_dir,
                                vintage_pattern = "TEST$")

lkup <-  lkups$versions_paths$`20220810_2017_01_02_TEST`

# default parameters

lp <- list(
year             = c(1990, 2000),
povline          =  2.25,
popshare         =  NULL,
group_by         =  c("wb"),
welfare_type     =  c("all"),
reporting_level  =  c("all"),
debug            =  FALSE,
censor           =  TRUE,
lkup             =  lkup
)



test_that("invalid parameters", {

  country <- "FLE" # wrong names
  lp$country <- country
  expect_error(do.call(pip_grp_logic, lp))


  country <- c("LAC", "FRE") # wrong names
  lp$country <- country
  expect_error(do.call(pip_grp_logic, lp))



  country <- c("LAC") # right
  year    <- c("BLAH") # right
  lp$country <- country
  lp$year    <- year
  expect_error(do.call(pip_grp_logic, lp))



  year    <- c(1970) # out of range
  lp$country <- country
  lp$year    <- year
  expect_error(do.call(pip_grp_logic, lp))


  year    <- c(1970, 2019) # out of range
  lp$country <- country
  lp$year    <- year
  expect_warning(do.call(pip_grp_logic, lp))


})

test_that("expected results", {

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

  expect_equal(de3, dc, label = "same results for official aggregates
               between grp and grp_logic when alt aggregate is included")



  ## multiple alternative aggregations  and official  aggregation
  country <- c("AFE", "SSA", "LAC", "AFW", "LIC")
  lp$country <- country

  de4 <- do.call(pip_grp_logic, lp)
  de5 <- de4[region_code %in% c("SSA", "LAC")]
  data.table::setcolorder(de5, names(dc))

  expect_equal(de5, dc, label = "same results for official aggregates
               between grp and grp_logic when alt aggregate is included")


  ## Just alternative aggregations
  country <- c("AFE", "AFW", "LIC", "IDX")
  lp$country <- country

  de6 <- do.call(pip_grp_logic, lp)

})