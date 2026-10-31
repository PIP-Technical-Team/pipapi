dirs_names <- c("00000001",
                "20210401",
                "20210920_2011_02_02_PROD",
                "20220317_2011_02_02_INT",
                "20220317_2017_01_01_INT",
                "20220408",
                "20220408_2011_02_02_PROD")


test_that("pattern list is created correctly", {

  vp <- get_vintage_pattern_regex()

  expect_equal(object = vp,
               expected =  list(
                 vintage_pattern = "\\d{8}_\\d{4}_\\d{2}_\\d{2}_(PROD|TEST|INT)$",
                 prod_regex = "PROD$",
                 int_regex = "INT$",
                 test_regex = "TEST$")
               )


  vp <- get_vintage_pattern_regex(vintage_pattern = "\\.*",
                                  int_regex = "foo$")

  expect_equal(object = vp,
               expected =  list(
                 vintage_pattern = "\\.*",
                 prod_regex = "PROD$",
                 int_regex = "foo$",
                 test_regex = "TEST$")
               )


})


test_that("create vintanger pattern call is working fine", {

  # test NULL
  vintage_pattern <- NULL
  cvp <- create_vintage_pattern_call(vintage_pattern)
  vp  <- get_vintage_pattern_regex()
  expect_equal(cvp, vp)

  # Test list
  vintage_pattern <- list("r.*", "", "^hjkhj\\.d")
  cvp <- create_vintage_pattern_call(vintage_pattern)
  expect_equal(cvp, list(vintage_pattern = "r.*",
                         prod_regex = "",
                         int_regex = "^hjkhj\\.d",
                         test_regex = "TEST$"))


  # Test unnamed character vector
  vintage_pattern <- c("r.*", "", "^hjkhj\\.d")
  cvp <- create_vintage_pattern_call(vintage_pattern)
  expect_equal(cvp, list(vintage_pattern = "r.*",
                         prod_regex = "",
                         int_regex = "^hjkhj\\.d",
                         test_regex = "TEST$"))


  # test named chacter vector
  vintage_pattern <- c(vintage_pattern = "r.*", test_regex = "", int_regex =  "^hjkhj\\.d")
  cvp <- create_vintage_pattern_call(vintage_pattern)
  expect_equal(cvp, list(vintage_pattern = "r.*",
                         prod_regex = "PROD$",
                         int_regex = "^hjkhj\\.d",
                         test_regex = ""))

})




vintage_patterns <- get_vintage_pattern_regex()

test_that("id_valid_dirs correctly identifies valid directories", {

  out <- id_valid_dirs(dirs_names = dirs_names,
                       vintage_pattern = vintage_patterns$vintage_pattern)

  expect_equal(out, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
})


test_that("sort_versions correctly orders available versions", {
  versions <- c("20210920_2011_02_02_PROD",
                "20220317_2011_02_02_INT",
                "20220317_2017_01_01_INT",
                "20220408_2011_02_02_PROD")
  expected_sorted_versions <- c("20220408_2011_02_02_PROD",
                                "20210920_2011_02_02_PROD",
                                "20220317_2017_01_01_INT",
                                "20220317_2011_02_02_INT")

  out <- sort_versions(versions = versions,
                       prod_regex = vintage_patterns$prod_regex,
                       int_regex  = vintage_patterns$int_regex,
                       test_regex = vintage_patterns$test_regex)

  expect_equal(out, expected_sorted_versions)
})

# test_that("all objects are correctly passed and used", {
#   root <- rprojroot::is_r_package
#   tmp <- lintr::lint(filename = root$find_file("R/create_lkups.R"),
#                      linters = lintr::object_usage_linter())
#
#   expect_equal(length(tmp), 0)
# })


# ifel_isnull() --------------------------------------------------------------

test_that("ifel_isnull: returns y when x is NULL", {
  expect_equal(pipapi:::ifel_isnull(NULL, "default"), "default")
})

test_that("ifel_isnull: returns x when x is not NULL", {
  expect_equal(pipapi:::ifel_isnull("value", "default"), "value")
})

test_that("ifel_isnull: works with numeric types", {
  expect_equal(pipapi:::ifel_isnull(NULL, 42L), 42L)
  expect_equal(pipapi:::ifel_isnull(0, 42L), 0)
})


# use_new_lineup_version() ---------------------------------------------------

test_that("use_new_lineup_version: returns FALSE for dates before threshold", {
  expect_false(use_new_lineup_version("20250401_2021_01_02_PROD"))
  expect_false(use_new_lineup_version("20250501_2021_01_02_PROD")) # threshold is >
})

test_that("use_new_lineup_version: returns TRUE for dates after threshold", {
  expect_true(use_new_lineup_version("20250502_2021_01_02_PROD"))
  expect_true(use_new_lineup_version("20250930_2021_01_02_PROD"))
  expect_true(use_new_lineup_version("20260101_2021_01_02_PROD"))
})

test_that("use_new_lineup_version: is vectorised", {
  x <- c("20250401_2021_01_02_PROD", "20250930_2021_01_02_PROD")
  result <- use_new_lineup_version(x)
  expect_equal(result, c(FALSE, TRUE))
})

test_that("use_new_lineup_version: TEST_VINTAGE triggers new path", {
  expect_true(use_new_lineup_version(TEST_VINTAGE))
})


# id_valid_dirs() edge cases -------------------------------------------------

test_that("id_valid_dirs: all-invalid names returns all FALSE", {
  out <- id_valid_dirs(dirs_names = c("foo", "bar", "123"),
                       vintage_pattern = vintage_patterns$vintage_pattern)
  expect_true(all(!out))
})

test_that("id_valid_dirs: empty input returns empty logical", {
  out <- id_valid_dirs(dirs_names = character(0),
                       vintage_pattern = vintage_patterns$vintage_pattern)
  expect_equal(length(out), 0L)
  expect_type(out, "logical")
})

test_that("id_valid_dirs: TEST suffix is valid", {
  out <- id_valid_dirs(dirs_names = "20220317_2011_02_02_TEST",
                       vintage_pattern = vintage_patterns$vintage_pattern)
  expect_true(out)
})


# sort_versions() edge cases -------------------------------------------------

test_that("sort_versions: PROD versions sorted newest-first", {
  versions <- c("20210101_2011_01_01_PROD", "20220101_2011_01_01_PROD")
  out <- sort_versions(versions,
                       prod_regex = vintage_patterns$prod_regex,
                       int_regex  = vintage_patterns$int_regex,
                       test_regex = vintage_patterns$test_regex)
  expect_equal(out[1], "20220101_2011_01_01_PROD")
})

test_that("sort_versions: PROD before INT before TEST", {
  versions <- c("20220101_2011_01_01_INT",
                "20220101_2011_01_01_TEST",
                "20220101_2011_01_01_PROD")
  out <- sort_versions(versions,
                       prod_regex = vintage_patterns$prod_regex,
                       int_regex  = vintage_patterns$int_regex,
                       test_regex = vintage_patterns$test_regex)
  expect_equal(out[1], "20220101_2011_01_01_PROD")
  expect_equal(out[2], "20220101_2011_01_01_INT")
  expect_equal(out[3], "20220101_2011_01_01_TEST")
})

test_that("sort_versions: empty input returns empty character", {
  out <- sort_versions(character(0),
                       prod_regex = vintage_patterns$prod_regex,
                       int_regex  = vintage_patterns$int_regex,
                       test_regex = vintage_patterns$test_regex)
  expect_equal(length(out), 0L)
})


# create_return_cols() -------------------------------------------------------

test_that("create_return_cols: returns a named list", {
  result <- create_return_cols(
    pip     = list(cols = c("headcount", "poverty_gap")),
    pip_grp = list(cols = c("region_code"))
  )
  expect_type(result, "list")
  expect_named(result, c("pip", "pip_grp"))
})

test_that("create_return_cols: preserves inner structure", {
  cols <- c("headcount", "mean")
  result <- create_return_cols(pip = list(cols = cols))
  expect_equal(result$pip$cols, cols)
})

test_that("create_return_cols: empty call returns empty list", {
  result <- create_return_cols()
  expect_equal(length(result), 0L)
})


# escape_vintage_regex() -----------------------------------------------------

test_that("escape_vintage_regex: escapes special regex characters", {
  expect_equal(escape_vintage_regex("2026.01_PROD"), "2026\\.01_PROD")
  expect_equal(escape_vintage_regex("a+b*c?d(e)f[g]{h}^i$j|k"), "a\\+b\\*c\\?d\\(e\\)f\\[g\\]\\{h\\}\\^i\\$j\\|k")
  expect_equal(escape_vintage_regex(character(0)), character(0))
  expect_equal(escape_vintage_regex(NULL), character(0))
})


# build_vintage_pattern() ----------------------------------------------------

test_that("build_vintage_pattern: constructs anchored exact-match regex", {
  pattern <- build_vintage_pattern(c("20250930_2021_01_02_PROD", "20260922_2021_01_02_PROD"))
  expect_equal(pattern, "^(20250930_2021_01_02_PROD|20260922_2021_01_02_PROD)$")
  expect_true(grepl(pattern, "20250930_2021_01_02_PROD"))
  expect_true(grepl(pattern, "20260922_2021_01_02_PROD"))
  expect_false(grepl(pattern, "20220609_2011_02_02_PROD"))
  expect_false(grepl(pattern, "prefix_20250930_2021_01_02_PROD"))
})

test_that("build_vintage_pattern: deduplicates, trims, and ignores blanks", {
  pattern <- build_vintage_pattern(c(" vintage_A ", "vintage_A", "", NA, "vintage_B"))
  expect_equal(pattern, "^(vintage_A|vintage_B)$")
})

test_that("build_vintage_pattern: aborts on empty or invalid input", {
  expect_error(build_vintage_pattern(character(0)), class = "rlang_error")
  expect_error(build_vintage_pattern(NULL), class = "rlang_error")
  expect_error(build_vintage_pattern(123L), class = "rlang_error")
  expect_error(build_vintage_pattern(c("", "   ", NA)), class = "rlang_error")
})


# read_pairing_vintages() ---------------------------------------------------

test_that("read_pairing_vintages: reads and filters synthetic pairing YAML", {
  temp_dir <- withr::local_tempdir()
  pairing_path <- file.path(temp_dir, "pairing.yml")
  synthetic_pairing <- list(
    pairing_file_version = 1L,
    bindings = list(
      list(
        release = "20220609",
        schema = "pre-lineup",
        folders = list("20220609_2011_02_02_PROD"),
        pipapi_version = "1.0.0",
        pipapi_sha = "f081d28fc9ed9fbc90660bcecf324e3164d77d19"
      ),
      list(
        release = "20220909",
        schema = "pre-lineup",
        folders = list("20220909_2011_02_02_PROD", "20220909_2017_01_02_PROD"),
        pipapi_version = "1.3.7.9006",
        pipapi_sha = "089c3fc877e003259694fb9b6aef8626a317bb53"
      ),
      list(
        release = "20250930",
        schema = "new-lineup",
        folders = list("20250930_2017_01_02_PROD", "20250930_2021_01_02_PROD"),
        pipapi_version = "1.5.15",
        pipapi_sha = "280af151d05902a550d5dea18cd4bf1a3d013239"
      )
    )
  )
  yaml::write_yaml(synthetic_pairing, pairing_path)

  # Filter by schema and version
  vintages_new <- read_pairing_vintages(pairing_path, schema = "new-lineup", pipapi_version = "1.5.15")
  expect_equal(vintages_new, c("20250930_2017_01_02_PROD", "20250930_2021_01_02_PROD"))

  # Filter by sha (case-insensitive)
  vintages_pre <- read_pairing_vintages(pairing_path, pipapi_sha = "089C3FC877E003259694FB9B6AEF8626A317BB53")
  expect_equal(vintages_pre, c("20220909_2011_02_02_PROD", "20220909_2017_01_02_PROD"))

  # All pre-lineup
  vintages_all_pre <- read_pairing_vintages(pairing_path, schema = "pre-lineup")
  expect_equal(sort(vintages_all_pre), c("20220609_2011_02_02_PROD", "20220909_2011_02_02_PROD", "20220909_2017_01_02_PROD"))

  # Error conditions
  expect_error(read_pairing_vintages(pairing_path), class = "rlang_error")
  expect_error(read_pairing_vintages("nonexistent.yml", schema = "new-lineup"), class = "rlang_error")
  expect_error(read_pairing_vintages(NA_character_, schema = "new-lineup"), class = "rlang_error")
  expect_error(read_pairing_vintages(pairing_path, schema = "unknown-schema"), class = "rlang_error")

  # Malformed YAML versions and empty bindings
  bad_ver_path <- file.path(temp_dir, "bad_ver.yml")
  yaml::write_yaml(list(pairing_file_version = 2L, bindings = synthetic_pairing$bindings), bad_ver_path)
  expect_error(read_pairing_vintages(bad_ver_path, schema = "new-lineup"), class = "rlang_error")

  frac_ver_path <- file.path(temp_dir, "frac_ver.yml")
  yaml::write_yaml(list(pairing_file_version = 1.2, bindings = synthetic_pairing$bindings), frac_ver_path)
  expect_error(read_pairing_vintages(frac_ver_path, schema = "new-lineup"), class = "rlang_error")

  empty_bind_path <- file.path(temp_dir, "empty_bind.yml")
  yaml::write_yaml(list(pairing_file_version = 1L, bindings = list()), empty_bind_path)
  expect_error(read_pairing_vintages(empty_bind_path, schema = "new-lineup"), class = "rlang_error")

  empty_folders_path <- file.path(temp_dir, "empty_folders.yml")
  yaml::write_yaml(
    list(
      pairing_file_version = 1L,
      bindings = list(list(release = "20250930", schema = "new-lineup", folders = list("", "  ")))
    ),
    empty_folders_path
  )
  expect_error(read_pairing_vintages(empty_folders_path, schema = "new-lineup"), class = "rlang_error")
})


# extract_data_dirs() with pairing filters -----------------------------------

test_that("extract_data_dirs: filters directories strictly with vintage_folders", {
  temp_dir <- withr::local_tempdir()
  dirs <- c("20220909_2011_02_02_PROD",
            "20220909_2017_01_02_PROD",
            "20250930_2021_01_02_PROD",
            "not_a_vintage")
  for (d in dirs) {
    dir.create(file.path(temp_dir, d))
  }

  vp <- get_vintage_pattern_regex()
  target_folders <- c("20250930_2021_01_02_PROD")

  res <- extract_data_dirs(temp_dir, vintage_pattern = vp, vintage_folders = target_folders)
  expect_equal(names(res), "20250930_2021_01_02_PROD")
  expect_equal(length(res), 1L)

  # With build_vintage_pattern
  vp_custom <- create_vintage_pattern_call(build_vintage_pattern(c("20220909_2011_02_02_PROD", "20220909_2017_01_02_PROD")))
  res_pattern <- extract_data_dirs(temp_dir, vintage_pattern = vp_custom)
  expect_equal(sort(names(res_pattern)), c("20220909_2011_02_02_PROD", "20220909_2017_01_02_PROD"))
})


# create_lkups() / resolve_use_new_lineup() schema configuration override ----

test_that("resolve_use_new_lineup: explicit schema overrides date heuristic", {
  # When schema = "pre-lineup", returns FALSE even for 2026 date
  expect_false(resolve_use_new_lineup("20260922_2021_01_02_PROD", schema = "pre-lineup"))

  # When schema = "new-lineup", returns TRUE even for 2022 date
  expect_true(resolve_use_new_lineup("20220909_2017_01_02_PROD", schema = "new-lineup"))

  # When schema = NULL, falls back to use_new_lineup_version()
  expect_false(resolve_use_new_lineup("20220909_2017_01_02_PROD", schema = NULL))
  expect_true(resolve_use_new_lineup("20260922_2021_01_02_PROD", schema = NULL))

  # Invalid schema aborts
  expect_error(
    resolve_use_new_lineup("20260922_2021_01_02_PROD", schema = "invalid-schema"),
    class = "rlang_error"
  )
})


# create_versioned_lkups() fault tolerance & isolation -----------------------

test_that("create_versioned_lkups: skips corrupt vintage with warning and succeeds", {
  temp_dir <- withr::local_tempdir()
  good_dir <- file.path(temp_dir, "20250930_2021_01_02_PROD")
  bad_dir <- file.path(temp_dir, "20250930_2017_01_02_PROD")
  dir.create(good_dir)
  dir.create(bad_dir)

  # Mock create_lkups so good_dir succeeds and bad_dir fails
  mock_create_lkups <- function(data_dir, versions, schema = NULL) {
    if (grepl("2017_01_02", data_dir)) {
      stop("Corrupted fst file in estimations/prod_ref_estimation.fst")
    }
    list(dummy_table = "ok", use_new_lineup_version = TRUE)
  }

  testthat::local_mocked_bindings(create_lkups = mock_create_lkups)
  expect_warning(
    res <- create_versioned_lkups(
      data_dir = temp_dir,
      vintage_folders = c("20250930_2021_01_02_PROD", "20250930_2017_01_02_PROD"),
      schema = "new-lineup"
    ),
    "Failed to load vintage folder"
  )

  # Valid vintage is retained
  expect_equal(res$versions, "20250930_2021_01_02_PROD")
  expect_equal(names(res$versions_paths), "20250930_2021_01_02_PROD")
  expect_equal(res$latest_release, "20250930_2021_01_02_PROD")
  expect_identical(res$versions, names(res$versions_paths))
})

test_that("create_versioned_lkups: warns when primary latest_release vintage fails and falls back", {
  temp_dir <- withr::local_tempdir()
  newer_bad_dir <- file.path(temp_dir, "20260922_2021_01_02_PROD")
  older_good_dir <- file.path(temp_dir, "20250930_2021_01_02_PROD")
  dir.create(newer_bad_dir)
  dir.create(older_good_dir)

  mock_create_lkups_newest_fails <- function(data_dir, versions, schema = NULL) {
    if (grepl("20260922", data_dir)) {
      stop("Newest vintage corrupted")
    }
    list(dummy_table = "ok", use_new_lineup_version = TRUE)
  }

  testthat::local_mocked_bindings(create_lkups = mock_create_lkups_newest_fails)
  warnings <- testthat::capture_warnings(
    res <- create_versioned_lkups(
      data_dir = temp_dir,
      vintage_folders = c("20260922_2021_01_02_PROD", "20250930_2021_01_02_PROD"),
      schema = "new-lineup"
    )
  )
  expect_true(any(grepl("Failed to load vintage folder", warnings)))
  expect_true(any(grepl("Primary vintage.*failed to load", warnings)))
  expect_equal(res$latest_release, "20250930_2021_01_02_PROD")
})

test_that("create_versioned_lkups: aborts when all candidate vintages fail", {
  temp_dir <- withr::local_tempdir()
  bad_dir1 <- file.path(temp_dir, "20250930_2021_01_02_PROD")
  bad_dir2 <- file.path(temp_dir, "20250930_2017_01_02_PROD")
  dir.create(bad_dir1)
  dir.create(bad_dir2)

  mock_create_lkups_all_fail <- function(data_dir, versions, schema = NULL) {
    stop("Disk read error")
  }

  testthat::local_mocked_bindings(create_lkups = mock_create_lkups_all_fail)
  expect_warning(
    expect_error(
      create_versioned_lkups(data_dir = temp_dir),
      class = "rlang_error"
    ),
    "Failed to load vintage folder"
  )
})

test_that("create_versioned_lkups: aborts when no directories match filter", {
  temp_dir <- withr::local_tempdir()
  dir.create(file.path(temp_dir, "empty_dir"))

  expect_error(
    create_versioned_lkups(temp_dir, vintage_pattern = "^nonexistent$"),
    class = "rlang_error"
  )
})

