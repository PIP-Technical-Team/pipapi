options(joyn.verbose = FALSE)
# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")

skip_if(data_dir == "")

latest_version <-
  available_versions(data_dir) |>
  max()

lkups <- create_versioned_lkups(data_dir, vintage_pattern = latest_version)
lkup <- lkups$versions_paths[[lkups$latest_release]]


# default parameters

lp <- list(
  year = c(1990, 2000),
  povline = 2.25,
  group_by = c("wb"),
  welfare_type = c("all"),
  reporting_level = c("all"),
  censor = TRUE,
  lkup = lkup
)
local_mocked_bindings(
  get_caller_names = function() c("pip_grp")
)

test_that("pip_grp and pip_grp_logic give the same results for official and alternative region selection", {
  ## regular official regions
  country <- c("SSF", "LCN")
  year <- c(2000, 2019) # out of range
  lp$country <- country
  lp$year <- year

  # de1 <- do.call(pip_grp_logic, lp)
  de1 <- pip_grp_logic(
    year = c(2000, 2019), # out of range
    povline = 2.25,
    group_by = c("wb"),
    welfare_type = c("all"),
    reporting_level = c("all"),
    censor = TRUE,
    lkup = lkup,
    country = c("SSF", "LCN")
  )

  # dc <- do.call(pip_grp, lp)
  dc <- pip_grp(
    year = c(2000, 2019), # out of range
    povline = 2.25,
    group_by = c("wb"),
    welfare_type = c("all"),
    reporting_level = c("all"),
    censor = TRUE,
    lkup = lkup,
    country = c("SSF", "LCN")
  )
  expect_equal(
    de1,
    dc,
    label = "same results for official aggregates
               between grp and grp_logic"
  )

  # One alternative aggregate within one  official  region
  country <- c("AFE", "SSF", "LCN")
  lp$country <- country

  # de2 <- do.call(pip_grp_logic, lp)
  de2 <- pip_grp_logic(
    year = c(2000, 2019), # out of range
    povline = 2.25,
    group_by = c("wb"),
    welfare_type = c("all"),
    reporting_level = c("all"),
    censor = TRUE,
    lkup = lkup,
    country = c("AFE", "SSF", "LCN")
  )

  de3 <- de2[region_code %in% c("SSF", "LCN")]
  data.table::setcolorder(de3, names(dc))
  data.table::setorder(de3, region_code, reporting_year)
  data.table::setorder(dc, region_code, reporting_year)

  expect_equal(
    de3[, !"mean"],
    dc[, !"mean"],
    label = "same results for official aggregates
               between grp and grp_logic when alt aggregate is included"
  )

  ## multiple alternative aggregations  and official  aggregation
  country <- c("AFE", "SSF", "LCN", "AFW", "LIC")
  lp$country <- country

  # de4 <- do.call(pip_grp_logic, lp)
  de4 <- pip_grp_logic(
    year = c(2000, 2019), # out of range
    povline = 2.25,
    group_by = c("wb"),
    welfare_type = c("all"),
    reporting_level = c("all"),
    censor = TRUE,
    lkup = lkup,
    country = c("AFE", "SSF", "LCN", "AFW", "LIC")
  )
  de5 <- de4[region_code %in% c("SSF", "LCN")]
  data.table::setcolorder(de5, names(dc))
  data.table::setorder(de5, region_code, reporting_year)

  expect_equal(
    de5[, !"mean"],
    dc[, !"mean"],
    label = "same results for official aggregates
               between grp and grp_logic when alt aggregate is included"
  )
})

test_that("pip_grp_logic selection works correctly", {
  # Returns ALL official + alternative regions when country = "ALL"
  country = "ALL"
  tmp = pip_grp_logic(
    country = country,
    year = 2010,
    group_by = "wb",
    lkup = lkup
  )
  expect_equal(
    sort(unique(tmp$region_code)),
    sort(lkup$aux_files$regions[
      grouping_type %in% c("africa_split", "region", "regionpcn", "world"),
      region_code
    ])
  )

  # Returns only "WLD" when country = "WLD"
  country = "WLD"
  tmp = pip_grp_logic(
    country = country,
    year = 2010,
    group_by = "wb",
    lkup = lkup
  )
  expect_equal(tmp$region_code, country)

  # Returns only "AFE" when country = "AFE"
  country = "AFE"
  tmp = pip_grp_logic(
    country = country,
    year = 2010,
    group_by = "wb",
    lkup = lkup
  )
  expect_equal(unique(tmp$region_code), country)

  # Returns only "SSF" when country = "SSF"
  country = "SSF"
  tmp = pip_grp_logic(
    country = country,
    year = 2010,
    group_by = "wb",
    lkup = lkup
  )
  expect_equal(unique(tmp$region_code), country)

  # Returns correct results when mixing official alternative regions
  country = c("AFE", "LCN")
  tmp = pip_grp_logic(
    country = country,
    year = 2010,
    group_by = "wb",
    lkup = lkup
  )
  expect_equal(sort(unique(tmp$region_code)), sort(country))

  # Returns correct results when mixing official, alternative, and "WLD" regions
  country = c("AFE", "LCN", "WLD")
  tmp = pip_grp_logic(
    country = country,
    year = 2010,
    group_by = "wb",
    lkup = lkup
  )
  expect_equal(sort(unique(tmp$region_code)), sort(country))
})
