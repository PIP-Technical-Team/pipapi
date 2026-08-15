test_that("ui_version_id returns every PPP year for the latest release", {
  versions <- c(
    "20250814_2017_01_02_PROD",
    "20260922_2021_01_02_PROD",
    "20260922_2017_01_02_INT"
  )

  expect_identical(
    ui_version_id(versions),
    list(
      ppp_2017 = "20260922_2017",
      ppp_2021 = "20260922_2021"
    )
  )
})

test_that("ui_version_id scales to new PPP years and removes duplicates", {
  versions <- c(
    "20260922_2024_01_02_PROD",
    "20260922_2011_01_02_PROD",
    "20260922_2021_03_01_TEST",
    "20260922_2017_01_02_PROD",
    "20260922_2021_01_02_PROD"
  )

  expect_identical(
    ui_version_id(versions),
    list(
      ppp_2011 = "20260922_2011",
      ppp_2017 = "20260922_2017",
      ppp_2021 = "20260922_2021",
      ppp_2024 = "20260922_2024"
    )
  )
})

test_that("ui_version_id deterministically orders PPP years for a tied release", {
  versions <- c(
    "20251231_2021_01_02_PROD",
    "20260101_2021_01_02_PROD",
    "20260101_2017_01_02_PROD",
    "20251231_2017_01_02_PROD"
  )

  result <- ui_version_id(versions)

  expect_identical(names(result), c("ppp_2017", "ppp_2021"))
  expect_identical(unname(unlist(result)), c("20260101_2017", "20260101_2021"))
})

test_that("ui_version_id rejects empty and malformed version vectors", {
  expect_error(ui_version_id(character()), "non-empty character vector")
  expect_error(ui_version_id(c("20260922_2021_01_02_PROD", NA_character_)), "missing values")
  expect_error(ui_version_id("not-a-version"), "8-digit release date")
  expect_error(ui_version_id("20260230_2021_01_02_PROD"), "valid dates")
  expect_error(ui_version_id(20260922), "non-empty character vector")
})
