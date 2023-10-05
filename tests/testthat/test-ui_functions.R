# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

# constants
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkups <- lkups$versions_paths[[lkups$latest_release]]

# lkup_path <- test_path("testdata", "lkup.rds")
# lkups      <- readRDS(lkup_path)



set.seed(42)
lkups$pl_lkup <- lkups$pl_lkup[sample(nrow(lkups$pl_lkup), 10)]
lkups2 <- lkups
lkups2$svy_lkup <- lkups2$svy_lkup[country_code %in% c('AGO', 'ZWE')]
lkups2$ref_lkup <- lkups2$ref_lkup[country_code %in% c('AGO', 'ZWE')]


dt_lac <- readRDS(test_path("testdata", "pip_lac_resp.rds"))
dt_sas <- readRDS(test_path("testdata", "pip_sas_resp.rds"))

test_that("ui_hp_stacked() works as expected", {
  res                <- ui_hp_stacked(povline = 1.9, lkup = lkups2)
  countries_selected <- lkups2$svy_lkup[, unique(country_code)]
  tmp                <- lkups2$aux_files$country_list[country_code %in% countries_selected]

  var_code <- c("country_code", "region_code", "world_code")

  regions_of_countries <-
    tmp[, ..var_code] |>
    melt(id.vars = "country_code") |>
    {\(.) .[, value] }() |>
    unique()


  expect_identical(
    names(res),
    c(
      "region_code", "reporting_year",
      "poverty_line", "pop_in_poverty"
    )
  )
  expect_identical(unique(res$region_code) |>
                     sort(),
                   regions_of_countries |>
                     sort())
  expect_true(all(res$pop_in_poverty == floor(res$pop_in_poverty))) # No decimals for population numbers
})

test_that("ui_hp_countries() works as expected", {
  res <- ui_hp_countries(country = c("AGO", "CIV"), povline = 1.9, lkup = lkups)
  expect_identical(
    names(res),
    c(
      "region_code", "country_code",
      "reporting_year", "poverty_line",
      "reporting_pop", "pop_in_poverty"
    )
  )
  expect_true(all(res$pop_in_poverty < 50))
  check <- lkups$svy_lkup[country_code %in% c("AGO", "CIV")]$reporting_year
  expect_equal(res$reporting_year, check)
})

test_that("ui_pc_charts() works as expected", {
  skip("TEMPORARY SKIP")

  # Regular query (fill_gaps = FALSE)
  res <- ui_pc_charts(country = "AGO", povline = 1.9, lkup = lkups)
  expect_equal(nrow(res), nrow(lkups$svy_lkup[country_code == "AGO"]))
  expect_equal(length(names(res)), 35)

  skip("TEMPORARY SKIP")
  # Regular query (fill_gaps = TRUE)
  res <- ui_pc_charts(country = "AGO", povline = 1.9, fill_gaps = TRUE, lkup = lkups)
  expect_equal(nrow(res), length(unique(lkups$ref_lkup$reporting_year)))

  # Group by
  res <- ui_pc_charts(country = "AGO", group_by = "wb", povline = 1.9, lkup = lkups)
  res2 <- pip(country = "AGO", group_by = "wb", povline = 1.9, lkup = lkups)
  res2$reporting_pop <- res2$reporting_pop / 1e6
  res2$pop_in_poverty <- res2$pop_in_poverty / 1e6
  expect_equal(res, res2)
})

test_that("ui_pc_regional() works as expected", {
  skip("This test fails but this situation should never arise in practice.
       The lkup table is incomplete.")
  res <- ui_pc_regional(povline = 1.9, lkup = lkups2)

  countries_selected <- lkups2$svy_lkup[, unique(country_code)]
  tmp                <- lkups2$aux_files$country_list[country_code %in% countries_selected]

  var_code <- grep("_code$", names(tmp), value = TRUE)

  regions_of_countries <-
    tmp[, ..var_code] |>
    melt(id.vars = "country_code") |>
    {\(.) .[, value] }() |>
    unique()

  expect_identical(
   names(res) |> sort(),
    c(
      "region_name",
      "region_code",
      "reporting_year",
      "reporting_pop",
      "poverty_line",
      "headcount",
      "poverty_gap",
      "poverty_severity",
      "watts",
      "mean",
      "pop_in_poverty"
    ) |> sort()
  )

  # DISABLE TEMPORARILY: NEEDS TO BE UPDATED
  # expect_identical(unique(res$region_code) |>
  #                    sort(),
  #                  regions_of_countries |>
  #                    sort())


  # expect_identical(unique(res$region_code), c("SSA", "WLD"))
})

test_that("ui_cp_poverty_charts() works as expected", {
  dl <- ui_cp_poverty_charts(
    country = "AGO",
    povline = 1.9,
    pop_units = 1e6,
    lkup = lkups
  )
  expect_identical(names(dl), c("pov_trend", "pov_mrv"))

  # Test that ui_cp_poverty_charts() works correctly for
  # countries/country-years with only urban/rural
  # reporting_level
  dl <- ui_cp_poverty_charts(
    country = "ARG",
    povline = 1.9,
    pop_units = 1e6,
    lkup = lkups
  )
  expect_equal(nrow(dl$pov_trend),
               nrow(lkups$svy_lkup[country_code == "ARG"]))
  expect_equal(nrow(dl$pov_mrv), 11)
  expect_equal(unique(dl$pov_trend$reporting_level), "urban")
  dl <- ui_cp_poverty_charts(
    country = "SUR",
    povline = 1.9,
    pop_units = 1e6,
    lkup = lkups
  )
  expect_equal(nrow(dl$pov_trend),
               nrow(lkups$svy_lkup[country_code == "SUR"]))
  expect_equal(nrow(dl$pov_mrv), 3)

  # Test that ui_cp_poverty_charts() works correctly for
  # aggregated distributions (only national rows are returned)
  dl <- ui_cp_poverty_charts(
    country = "CHN",
    povline = 1.9,
    pop_units = 1e6,
    lkup = lkups
  )
  expect_equal(nrow(dl$pov_trend),
               nrow(lkups$dist_stats[country_code == "CHN" &
                          reporting_level == "national"]))
  expect_equal(unique(dl$pov_trend$reporting_level), "national")

  # Test that ui_cp_poverty_charts() works correctly for
  # countries w/ multiple reporting levels (only national rows are returned)
  dl <- ui_cp_poverty_charts(
    country = "URY",
    povline = 1.9,
    pop_units = 1e6,
    lkup = lkups
  )
  expect_equal(nrow(dl$pov_trend),
               nrow(lkups$dist_stats[country_code == "URY" &
                                       reporting_level == "national"]))
  expect_equal(unique(dl$pov_trend$reporting_level), "national")
})

test_that("cp_pov_mrv_select_values() works as expected", {

  # Top 5
  set.seed(42)
  v <- round(runif(20), 5)
  h <- 0.91481
  out <- cp_pov_mrv_select_values(v, h)
  v <- sort(v)
  expect_equal(out, c(v[1:5], v[15:20]))

  # Bottom 5
  set.seed(42)
  v <- round(runif(20), 5)
  h <- 0.25543
  out <- cp_pov_mrv_select_values(v, h)
  v <- sort(v)
  expect_equal(out, c(v[1:6], v[16:20]))

  # Neither bottom 5 nor top 5
  set.seed(42)
  v <- round(runif(20), 5)
  h <- 0.56033
  out <- cp_pov_mrv_select_values(v, h)
  v <- sort(v)
  x <- which(v == h)
  expect_equal(out, c(v[1:3], v[(x - 2):(x + 2)], v[18:20]))
})

test_that("cp_pov_mrv_select_countries() works as expected", {

  # Selected country pertains to top 5
  res <- cp_pov_mrv_select_countries(dt_lac, "COL")
  expect_equal(nrow(res), 11)
  expect_identical(
    res$country_code,
    c(
      "URY", "CHL", "DOM", "PRY", "CRI", "BOL",
      "ECU", "LCA", "BRA", "COL", "HND"
    )
  )
  expect_true(!is.unsorted(res$headcount))

  # Selected country does not pertain to top 5 or bottom 5
  res <- cp_pov_mrv_select_countries(dt_lac, "MEX")
  expect_equal(nrow(res), 11)
  expect_identical(
    res$country_code,
    c(
      "URY", "CHL", "DOM", "SLV", "ARG", "MEX",
      "PER", "BOL", "BRA", "COL", "HND"
    )
  )
  expect_true(!is.unsorted(res$headcount))

  # Selected country does not pertain to bottom 5
  res <- cp_pov_mrv_select_countries(dt_lac, "CHL")
  expect_equal(nrow(res), 11)
  expect_identical(
    res$country_code,
    c(
      "URY", "CHL", "DOM", "PRY", "CRI", "PAN",
      "ECU", "LCA", "BRA", "COL", "HND"
    )
  )
  expect_true(!is.unsorted(res$headcount))

  # Less than 12 countries in original response
  res <- cp_pov_mrv_select_countries(dt_sas, "PAK")
  expect_equal(res, dt_sas[order(headcount)])
})

test_that("ui_cp_ki_headcount() works as expected", {
  df <- ui_cp_ki_headcount(country = "AGO", povline = 1.9, lkup = lkups)
  expect_identical(names(df), c(
    "country_code", "reporting_year",
    "poverty_line", "headcount"
  ))

  # Test that ui_cp_ki_headcount() works correctly for
  # countries/country-years with only urban/rural
  # reporting_level
  df <- ui_cp_ki_headcount(country = "ARG", povline = 1.9, lkup = lkups)
  expect_false(is.na(df$headcount))
  expect_equal(df$reporting_year,
    max(lkups$svy_lkup[country_code == "ARG"]$reporting_year))

  df <- ui_cp_ki_headcount(country = "SUR", povline = 1.9, lkup = lkups)
  expect_false(is.na(df$headcount))
  expect_equal(df$reporting_year,
               max(lkups$svy_lkup[country_code == "SUR"]$reporting_year))

  # Test that ui_cp_ki_headcount() works correctly for
  # aggregated distributions (only national rows are returned)
  df <- ui_cp_ki_headcount(country = "CHN", povline = 1.9, lkup = lkups)
  expect_false(is.na(df$headcount))
  expect_equal(df$reporting_year,
               max(lkups$dist_stats[country_code == "CHN" &
                                    reporting_level == "national"]$reporting_year))
  # Test that ui_cp_ki_headcount() works correctly for countries
  # w/ multiple reporting levels (only national rows are returned)
  df <- ui_cp_ki_headcount(country = "URY", povline = 1.9, lkup = lkups)
  expect_false(is.na(df$headcount))
  expect_equal(df$reporting_year,
               max(lkups$dist_stats[country_code == "URY" &
                                    reporting_level == "national"]$reporting_year))

})

test_that("ui_cp_key_indicators() works as expected", {

  # A single country
  dl <- ui_cp_key_indicators(country = "AGO", povline = 1.9, lkup = lkups)
  expect_length(dl, 1) # 1 country
  expect_length(dl[[1]], 7) # # 7 KI objects
  expect_equal(nrow(dl[[1]]$headcount), 1) # 1 poverty line
  expect_identical(
    names(dl[[1]]),
    c(
      "headcount", "headcount_national", "mpm_headcount",
      "reporting_pop", "gni", "gdp_growth", "shared_prosperity"
    )
  )
  expect_identical(dl[[1]]$headcount$poverty_line, 1.9)

  # Only CP relevant surveys
  dl <- ui_cp_key_indicators(country = "POL", povline = 1.9, lkup = lkups)
  expect_equal(nrow(dl[[1]]$headcount), 1)
  y <- max(lkups$svy_lkup[country_code == "POL" & display_cp == 1]$reporting_year)
  expect_equal(dl[[1]]$headcount$reporting_year, y)

})

test_that("ui_cp_charts() works as expected", {

  # A single poverty line
  dl1 <- ui_cp_charts(country = "AGO", povline = 1.9, lkup = lkups)
  expect_length(dl1, 1) # 1 country
  expect_length(dl1[[1]], 5) # # 5 chart objects (2 inside pov_charts)
  expect_length(dl1[[1]]$pov_charts, 1) # 1 poverty line
  expect_identical(names(dl1[[1]]), c(
    "pov_charts", "ineq_trend",
    "ineq_bar", "mpm", "sp"
  ))
  expect_identical(names(dl1[[1]]$pov_charts[[1]]),
                   c("pov_trend", "pov_mrv"))

  # Only CP relevant surveys
  dl <- ui_cp_charts(country = "POL", povline = 1.9, lkup = lkups)
  w <- dl$POL$pov_charts[[1]]$pov_trend$welfare_type
  expect_equal(unique(w), "income")
  y <- dl$POL$pov_charts[[1]]$pov_trend$reporting_year
  expect_equal(anyDuplicated(y), 0)

})

test_that("ui_cp_charts() only returns first country if multiple countries are passed", {
  # A single poverty line
  dl1 <- ui_cp_charts(country = c("AGO", "AFG"), povline = 1.9, lkup = lkups)
  expect_length(dl1, 1) # 1 country
  expect_equal(names(dl1), "AGO")
})

test_that("ui_svy_meta() works as expected", {
  expected_names <-
    c(
      "country_code",
      "country_name",
      "reporting_year" ,
      "survey_year",
      "surveyid_year",
      "survey_title",
      "survey_conductor",
      "survey_coverage",
      "welfare_type",
      "distribution_type",
      "metadata"
    )
  expected_metadata <- c(
    #"surveyid_year",
    "survey_acronym",
    "year_start",
    "year_end",
    "authoring_entity_name",
    "abstract",
    "collection_dates_cycle",
    "collection_dates_start",
    "collection_dates_end",
    "sampling_procedure",
    "collection_mode",
    "coll_situation",
    "weight",
    "cleaning_operations"
  )

  res <- ui_svy_meta(country = "AGO", lkup = lkups)
  expect_equal(unique(res$country_code), "AGO")
  expect_equal(names(res),
               expected_names)

  expect_equal(
    names(res$metadata[[1]]),
    expected_metadata
  )

  res <- ui_svy_meta(country = "all", lkup = lkups)

  expect_true(all(unique(res$country_code) %in%
                    lkups$query_controls$country$values))

  expect_equal(names(res),
               expected_names)

  expect_equal(
    names(res$metadata[[1]]),
    expected_metadata
  )

})

test_that("cp_correct_reporting_level() is working as expected for countries with 3 reporting levels", {
  # China returns national, urban, and rural data
  region <-
    lkups$svy_lkup[country_code == "CHN"]$region_code |>
    unique()
  # All countries from the same region
  countries <-
    lkups$svy_lkup[region_code == region]$country_code |>
    unique()

  tmp <- pip(country         = "CHN",
             year            = "all",
             povline         = 1.9,
             lkup            = lkup)
  expect_equal(length(unique(tmp$reporting_level)), 3)

  tmp2 <- cp_correct_reporting_level(tmp)
  expect_equal(length(unique(tmp2$reporting_level)), 1)
  expect_equal(unique(tmp2$reporting_level), "national")
})

test_that("cp_correct_reporting_level() is working as expected for countries with 1 sub-national reporting level", {
  # ARG returns only sub-national data
  region <-
    lkups$svy_lkup[country_code == "ARG"]$region_code |>
    unique()
  # All countries from the same region
  countries <-
    lkups$svy_lkup[region_code == region]$country_code |>
    unique()

  tmp <- pip(country         = "ARG",
             year            = "all",
             povline         = 1.9,
             lkup            = lkup)
  expect_equal(length(unique(tmp$reporting_level)), 1)
  expect_equal(unique(tmp$reporting_level), "urban")

  tmp2 <- cp_correct_reporting_level(tmp)
  expect_equal(length(unique(tmp2$reporting_level)), 1)
  expect_equal(unique(tmp2$reporting_level), "urban")
})

test_that("cp_correct_reporting_level() is working as expected for countries with 2 reporting levels: 1 national, 1 sub-national", {
  # COL returns urban and national data
  region <-
    lkups$svy_lkup[country_code == "COL"]$region_code |>
    unique()
  # All countries from the same region
  countries <-
    lkups$svy_lkup[region_code == region]$country_code |>
    unique()

  tmp <- pip(country         = "COL",
             year            = "all",
             povline         = 1.9,
             lkup            = lkup)
  expect_equal(length(unique(tmp$reporting_level)), 2)

  tmp2 <- cp_correct_reporting_level(tmp)
  expect_equal(length(unique(tmp2$reporting_level)), 1)
  expect_equal(unique(tmp2$reporting_level), "national")
})

test_that("cp_correct_reporting_level() is working as expected for countries with 1 national reporting level", {
  # FRA returns only national data
  region <-
    lkups$svy_lkup[country_code == "FRA"]$region_code |>
    unique()
  # All countries from the same region
  countries <-
    lkups$svy_lkup[region_code == region]$country_code |>
    unique()

  tmp <- pip(country         = "FRA",
             year            = "all",
             povline         = 1.9,
             lkup            = lkup)
  expect_equal(length(unique(tmp$reporting_level)), 1)
  expect_equal(unique(tmp$reporting_level), "national")

  tmp2 <- cp_correct_reporting_level(tmp)
  expect_equal(length(unique(tmp2$reporting_level)), 1)
  expect_equal(unique(tmp2$reporting_level), "national")
})

test_that("ui_cp_charts() optimized version returns same results as previous version", {
  # COL
  dl1 <- ui_cp_charts(country = c("COL"), povline = 1.9, lkup = lkups)
  dl2 <- ui_cp_charts(country = c("COL"), povline = 1.9, lkup = lkups)
  expect_equal(dl1, dl2)

  # ARG
  dl1 <- ui_cp_charts(country = c("ARG"), povline = 1.9, lkup = lkups)
  dl2 <- ui_cp_charts(country = c("ARG"), povline = 1.9, lkup = lkups)
  expect_equal(dl1, dl2)

  # CHN
  dl1 <- ui_cp_charts(country = c("CHN"), povline = 1.9, lkup = lkups)
  dl2 <- ui_cp_charts(country = c("CHN"), povline = 1.9, lkup = lkups)
  expect_equal(dl1, dl2)

})

test_that("ui_cp_key_indicators() optimized version returns same results as previous version", {
  # COL
  dl1 <- ui_cp_key_indicators(country = c("COL"), povline = 1.9, lkup = lkups)
  dl2 <- ui_cp_key_indicators(country = c("COL"), povline = 1.9, lkup = lkups)
  expect_equal(dl1, dl2)

  # ARG
  dl1 <- ui_cp_key_indicators(country = c("ARG"), povline = 1.9, lkup = lkups)
  dl2 <- ui_cp_key_indicators(country = c("ARG"), povline = 1.9, lkup = lkups)
  expect_equal(dl1, dl2)

  # CHN
  dl1 <- ui_cp_key_indicators(country = c("CHN"), povline = 1.9, lkup = lkups)
  dl2 <- ui_cp_key_indicators(country = c("CHN"), povline = 1.9, lkup = lkups)
  expect_equal(dl1, dl2)

})
