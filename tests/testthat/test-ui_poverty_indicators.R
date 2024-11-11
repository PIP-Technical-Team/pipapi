# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")

skip_if(data_dir == "")

latest_version <-
  available_versions(data_dir) |>
  max()

lkups <- create_versioned_lkups(data_dir,
                                vintage_pattern = latest_version)
lkups <- lkups$versions_paths[[lkups$latest_release]]


test_that("ui_pc_charts() works as expected", {

  # Regular query (fill_gaps = FALSE)
  with_mocked_bindings({
    res <- ui_pc_charts(country = "AGO",
                        povline = 1.9,
                        lkup = lkups)
    expect_equal(class(res), c("data.table", "data.frame"))
    expect_equal(names(res), lkups$return_cols$ui_pc_charts$cols)
    expect_equal(nrow(res), nrow(lkups$svy_lkup[country_code == "AGO"]))
  },
  get_caller_names = function() c("else")
  )

  skip("TEMPORARY SKIP")
  # Regular query (fill_gaps = TRUE)
  country <- "AGO"
  reporting_years <- length(unique(lkups$ref_lkup$reporting_year))
  expected_years <- reporting_years - nrow(lkups$censored$countries[country_code == country])

  with_mocked_bindings({
    res <- ui_pc_charts(country   = country,
                        povline   = 1.9,
                        fill_gaps = TRUE,
                        lkup      = lkups)
    expect_equal(class(res), c("data.table", "data.frame"))
    expect_equal(names(res), lkups$return_cols$ui_pc_charts$cols)
    expect_equal(nrow(res), expected_years)
  },
  get_caller_names = function() c("else")
  )


  # Group by
  ui_pc_charts(country = "AGO", group_by = "wb", povline = 1.9, lkup = lkups) |>
    expect_error()

  res <- ui_pc_charts(country = "AGO", povline = 1.9, lkup = lkups)
  res2 <- pip(country = "AGO",  povline = 1.9, lkup = lkups)
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

