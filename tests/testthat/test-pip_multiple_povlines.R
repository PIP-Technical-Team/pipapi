# Tests depend on PIPAPI_DATA_ROOT_FOLDER_LOCAL. Skip if not found.
library(collapse)
library(data.table)
data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")

skip_if(data_dir == "")

latest_version <-
  available_versions(data_dir) |>
  max()

lkups <- create_versioned_lkups(data_dir,
                                vintage_pattern = latest_version)
lkup <- lkups$versions_paths[[lkups$latest_release]]


# Multiple poverty lines ------------

test_that("Regular microdata one country", {

  ct   <- "AGO"
  pl1  <- 2.15
  pl2  <- 3.65
  year <- 2000

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(poverty_line)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(poverty_line)

  expect_equal(singles , appended)

})

test_that("Microdata one country, mult years", {

  ct   <- "AGO"
  pl1  <- 2.15
  pl2  <- 3.65
  year <- c(2000, 2008, 2018)

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(poverty_line, reporting_year)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(poverty_line, reporting_year)

  expect_equal(singles , appended)

})



test_that("Group data - one country", {

  ct   <- "ARE"
  pl1  <- 2.15
  pl2  <- 3.65
  year <- 2013

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(poverty_line)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(poverty_line)

  expect_equal(singles , appended)

})

test_that("Group one country national, mult years", {

  ct   <- "ARE"
  pl1  <- 2.15
  pl2  <- 3.65
  year <- c(2000, 2018)

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(poverty_line, reporting_year)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(poverty_line, reporting_year)

  expect_equal(singles , appended)

})

test_that("Group one country urb/rur, one year", {

  ct   <- "CHN"
  pl1  <- 2.15
  pl2  <- 3.65
  year <- c(1981)

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(poverty_line, reporting_year, reporting_level)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(poverty_line, reporting_year, reporting_level)

  expect_equal(singles , appended)

})

test_that("Group one country urb/rur, one year", {

  ct   <- "CHN"
  pl1  <- 2.15
  pl2  <- 3.65
  year <- c(2010)

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(poverty_line, reporting_year, reporting_level)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(poverty_line, reporting_year, reporting_level)

  expect_equal(singles , appended)

})


test_that("Group one country urb/rur, multi year", {

  ct   <- "CHN"
  pl1  <- 2.15
  pl2  <- 3.65
  year <- c(1984, 1987, 1990, 1993, 1996)

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(poverty_line, reporting_year, reporting_level)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(poverty_line, reporting_year, reporting_level)

  expect_equal(singles , appended)

})

test_that("Group one country urb/rur, All year", {

  ct   <- "CHN"
  pl1  <- 2.15
  pl2  <- 3.65
  year <- "all"

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(poverty_line, reporting_year, reporting_level)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(poverty_line, reporting_year, reporting_level)

  expect_equal(singles , appended)

})


test_that("mult countries, multi year", {

  ct   <- c("CHN", "PRY")
  pl1  <- 2.15
  pl2  <- 3.65
  year <- "all"

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(country_code, poverty_line, reporting_year, reporting_level)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(country_code, poverty_line, reporting_year, reporting_level)

  expect_equal(singles , appended)

})


test_that("all countries, all years", {

  ct   <- "all"
  pl1  <- 2.15
  pl2  <- 3.65
  year <- "all"

  out1 <- pip(country = ct ,year = year, povline = pl1, lkup = lkup)
  out2 <- pip(country = ct ,year = year, povline = pl2, lkup = lkup)
  singles <-
    rowbind(out2, out1) |>
    roworder(country_code, poverty_line, reporting_year, reporting_level)


  appended <- pip(
    country = ct ,
    year = year,
    povline = c(pl1, pl2),
    lkup = lkup
  ) |>
    roworder(country_code, poverty_line, reporting_year, reporting_level)

  expect_equal(singles , appended)

})


