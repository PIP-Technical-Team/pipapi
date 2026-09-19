# These tests source the core so the isolated library need not be reinstalled.
core_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
core_env <- new.env(parent = asNamespace("pipapi"))
for (file in c("cache-v2.R", "pip.R", "pip_agg.R", "ui_country_profile.R",
               "ui_poverty_indicators.R", "ui_home_page.R")) {
  sys.source(file.path(core_root, "R", file), envir = core_env)
}

core_fixture <- function() {
  source <- tempfile("cache-v2-source-")
  root <- tempfile("cache-v2-store-")
  dir.create(source)
  dir.create(root)
  version <- "20260922_2021_01_02_PROD"
  for (d in c("_aux", "estimations", "survey_data", "lineup_data")) {
    dir.create(file.path(source, d))
    writeBin(charToRaw(paste("fixture", d)), file.path(source, d, "input.bin"))
  }
  writeLines("2026-09-22T00:00:00Z", file.path(source, "data_update_timestamp.txt"))
  manifest <- core_env$cache_v2_manifest(source, version)
  core_env$.cache_v2_state$config <- list(root = root, manifest = manifest,
    build = list(fingerprint = "fixture-build"), planned_keys = character(),
    intermediate_mode = "write", required = TRUE, runtime_max_size = 1024^2)
  lkup <- core_env$cache_v2_attach(list(svy_lkup = data.table::data.table(
    country_code = c("AGO", "IDN"), display_cp = c(1, 0))), version)
  list(source = source, root = root, manifest = manifest, lkup = lkup, version = version)
}

test_that("effective defaults, types and permitted case forms have stable identities", {
  f <- core_fixture()
  id <- function(args = list(), operation = "pip", lkup = f$lkup, representation = NULL) {
    core_env$cache_v2_identity(operation, args, lkup, representation)
  }
  expect_identical(id()$key, id(list(country = "all", year = "all",
    povline = 1.90, welfare_type = "all", reporting_level = "all", group_by = "none"))$key)
  expect_identical(id(list(year = 2000L, povline = 3L))$key,
    id(list(povline = 3, year = "2000"))$key)
  expect_identical(id(list(country = "IDN", povline = 3))$key,
    id(list(povline = 3, country = "idn"))$key)
  expect_false(identical(id(list(country = c("AGO", "IDN")))$key,
    id(list(country = c("IDN", "AGO")))$key))
  expect_false(identical(id(list(povline = c(3, 4)))$key, id(list(povline = c(4, 3)))$key))
  expect_identical(id(list(povline = 3))$descriptor$parameters$povline_cents$value, list(300))
  expect_false(id(list(povline = 3.001))$cacheable)
  expect_false(identical(id(list(povline = 3.001))$key, id(list(povline = 3.002))$key))
  expect_error(id(list(unused = TRUE)), "Unknown")
  expect_error(core_env$cache_v2_effective_args("ui_pc_charts", list(group_by = "wb"), f$lkup), "arg")
  expect_error(core_env$cache_v2_effective_args("ui_hp_stacked", list(country = "ALL"), f$lkup), "Unknown")
  expect_identical(core_env$cache_v2_effective_args("ui_cp_key_indicators", list(), f$lkup)$povline, NULL)
  expect_identical(core_env$cache_v2_effective_args("ui_pc_charts", list(), f$lkup)$welfare_type, "all")
})

test_that("all provenance, operation and representation inputs separate keys", {
  f <- core_fixture()
  key <- core_env$cache_v2_identity("pip", list(), f$lkup)$key
  for (args in list(list(censor = TRUE), list(fill_gaps = TRUE), list(ppp = 12),
                    list(popshare = .1), list(povline = 3))) {
    expect_false(identical(key, core_env$cache_v2_identity("pip", args, f$lkup)$key))
  }
  cp <- core_env$cache_v2_cp_lookup(f$lkup)
  expect_identical(nrow(f$lkup$svy_lkup), 2L)
  expect_identical(nrow(cp$svy_lkup), 1L)
  expect_false(identical(key, core_env$cache_v2_identity("pip", list(), cp)$key))
  expect_false(identical(key, core_env$cache_v2_identity("pip_agg", list(), f$lkup)$key))
  a <- core_env$cache_v2_identity("cp-charts", list(povline = 3), f$lkup, list(serializer = "one"))
  b <- core_env$cache_v2_identity("cp-charts", list(povline = 3), f$lkup, list(serializer = "two"))
  expect_false(identical(a$key, b$key))
  cfg <- core_env$.cache_v2_state$config
  cfg$manifest$versions[[f$version]]$fingerprint <- "new-source"
  core_env$.cache_v2_state$config <- cfg
  changed <- core_env$cache_v2_attach(f$lkup, f$version)
  expect_false(identical(key, core_env$cache_v2_identity("pip", list(), changed)$key))
  expect_error(core_env$cache_v2_identity("pip", list(), f$lkup), "provenance")
  cfg$build$fingerprint <- "new-build"
  source_key <- core_env$cache_v2_identity("pip", list(), changed)$key
  core_env$.cache_v2_state$config <- cfg
  changed2 <- core_env$cache_v2_attach(f$lkup, f$version)
  expect_false(identical(core_env$cache_v2_identity("pip", list(), changed2)$key, source_key))
  other <- "20260922_2017_01_02_PROD"
  cfg$manifest$versions[[other]] <- cfg$manifest$versions[[f$version]]
  core_env$.cache_v2_state$config <- cfg
  other_lkup <- core_env$cache_v2_attach(f$lkup, other)
  expect_false(identical(core_env$cache_v2_identity("pip", list(), changed2)$key,
    core_env$cache_v2_identity("pip", list(), other_lkup)$key))
})

test_that("canonical UTF8 fixture sorts fields but never vector contents", {
  canonical <- core_env$.cache_v2_json(list(z = c("b", "a"),
    a = list(flag = TRUE, number = 3, text = "caf\u00e9")))
  expect_identical(canonical, '{"a":{"flag":true,"number":3,"text":"caf\u00e9"},"z":["b","a"]}')
  expect_identical(core_env$.cache_v2_sha("abc"),
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  expect_identical(core_env$.cache_v2_sha(canonical),
    digest::digest(charToRaw(enc2utf8(canonical)), algo = "sha256", serialize = FALSE))
})

test_that("manifest ignores copy time, covers source content and excludes caches", {
  f <- core_fixture()
  core_env$.cache_v2_state$config <- NULL
  file <- file.path(f$source, "survey_data", "input.bin")
  Sys.setFileTime(file, Sys.time() - 60)
  copied <- core_env$cache_v2_manifest(f$source, f$version, f$manifest)
  expect_identical(copied$fingerprint, f$manifest$fingerprint)
  expect_true(core_env$cache_v2_assert_inputs(f$manifest, full = TRUE))
  expect_true(core_env$cache_v2_assert_inputs(f$manifest, full = FALSE))
  writeLines("ignored", file.path(f$source, "cache.duckdb"))
  expect_identical(core_env$cache_v2_manifest(f$source, f$version)$fingerprint, f$manifest$fingerprint)
  writeBin(charToRaw("changed distribution"), file)
  expect_false(identical(core_env$cache_v2_manifest(f$source, f$version)$fingerprint, f$manifest$fingerprint))
  expect_error(core_env$cache_v2_assert_inputs(f$manifest), "inputs changed")
  unlink(file)
  expect_error(core_env$cache_v2_assert_inputs(f$manifest), "inputs changed")
})

test_that("store is immutable, checksummed, repairable and returns owned copies", {
  f <- core_fixture()
  id <- core_env$cache_v2_identity("pip", list(), f$lkup)
  expect_identical(core_env$cache_v2_get(id)$state, "missing")
  value <- data.table::data.table(x = 1:3)
  core_env$cache_v2_put(id, value)
  sha <- core_env$.cache_v2_file_sha(id$path)
  time <- file.info(id$path)$mtime
  first <- core_env$cache_v2_get(id)
  first$value[, x := 0L]
  expect_identical(core_env$cache_v2_get(id)$value$x, 1:3)
  core_env$cache_v2_put(id, data.table::data.table(x = 999))
  expect_identical(core_env$.cache_v2_file_sha(id$path), sha)
  expect_identical(file.info(id$path)$mtime, time)
  writeBin(as.raw(0), id$path)
  expect_identical(core_env$cache_v2_get(id)$state, "corrupt")
  core_env$cache_v2_put(id, value)
  expect_identical(core_env$cache_v2_get(id)$value$x, 1:3)
  unlink(paste0(id$path, ".meta.json"))
  expect_identical(core_env$cache_v2_get(id)$state, "corrupt")
  core_env$cache_v2_put(id, value)
  unlink(id$path)
  expect_identical(core_env$cache_v2_get(id)$state, "corrupt")
  core_env$cache_v2_put(id, value)
  expect_true(core_env$cache_v2_get(id)$hit)
  response <- core_env$cache_v2_identity("cp-charts", list(), f$lkup, list(schema = 1L))
  bytes <- charToRaw('{"value":null,"list":[2,1]}')
  core_env$cache_v2_put(response, bytes, "application/json")
  expect_identical(core_env$cache_v2_get(response)$value, bytes)
  expect_identical(core_env$cache_v2_get(response)$metadata$content_type, "application/json")
})

test_that("live mode bypasses reads and writes and non-cent lines are not cached", {
  f <- core_fixture()
  id <- core_env$cache_v2_identity("pip", list(), f$lkup)
  core_env$cache_v2_put(id, 1)
  withr::local_options(pipapi.query_live_data = TRUE)
  expect_false(core_env$cache_v2_get(id)$hit)
  expect_false(core_env$cache_v2_put(id, 2))
  options(pipapi.query_live_data = FALSE)
  expect_identical(core_env$cache_v2_get(id)$value, 1)
  three <- core_env$cache_v2_identity("pip", list(povline = 3.001), f$lkup)
  expect_false(core_env$cache_v2_put(three, 1))
  expect_false(file.exists(three$path))
})

test_that("disk and rename failures cannot publish completion and locks release", {
  f <- core_fixture()
  id <- core_env$cache_v2_identity("pip", list(), f$lkup)
  withr::local_options(pipapi.cache_v2_disk_usage = function(path) list(available = 0))
  expect_error(core_env$cache_v2_put(id, 1), "disk space")
  expect_false(core_env$cache_v2_get(id)$hit)
  options(pipapi.cache_v2_disk_usage = NULL)
  options(pipapi.cache_v2_rename = function(from, to) FALSE)
  withr::defer(options(pipapi.cache_v2_rename = NULL))
  expect_error(core_env$cache_v2_put(id, 1), "rename failed")
  expect_false(core_env$cache_v2_get(id)$hit)
  options(pipapi.cache_v2_rename = NULL)
  core_env$cache_v2_with_lock(id, core_env$cache_v2_put(id, 1))
  expect_true(core_env$cache_v2_get(id)$hit)
  expect_length(ls(core_env$.cache_v2_state$locks), 0L)
  interrupted <- core_env$cache_v2_identity("pip", list(povline = 4), f$lkup)
  options(pipapi.cache_v2_rename = function(from, to) {
    if (grepl("meta.json$", to)) return(FALSE)
    file.rename(from, to)
  })
  expect_error(core_env$cache_v2_put(interrupted, 2), "rename failed")
  expect_identical(core_env$cache_v2_get(interrupted)$state, "corrupt")
  options(pipapi.cache_v2_rename = NULL)
  core_env$cache_v2_put(interrupted, 2)
  expect_true(core_env$cache_v2_get(interrupted)$hit)
  for (error in list(simpleError("failure"), structure("failure", class = "try-error"),
                     list(error = "failure"), list(ok = FALSE))) {
    expect_error(core_env$cache_v2_put(interrupted, error), "error result")
  }
})

test_that("content type metadata is required and corruption repairs independently", {
  f <- core_fixture()
  id <- core_env$cache_v2_identity("cp-charts", list(), f$lkup, list(schema = 1L))
  expect_error(core_env$cache_v2_put(id, charToRaw("{}")), "content type")
  core_env$cache_v2_put(id, charToRaw("{}"), "application/json")
  meta <- jsonlite::fromJSON(paste0(id$path, ".meta.json"), simplifyVector = FALSE)
  meta$content_type <- NULL
  writeLines(core_env$.cache_v2_json(meta), paste0(id$path, ".meta.json"))
  expect_identical(core_env$cache_v2_get(id)$state, "corrupt")
  core_env$cache_v2_put(id, charToRaw("{}"), "application/json")
  expect_true(core_env$cache_v2_get(id)$hit)
})

test_that("source and cache relocation do not change keys or independent PPP inputs", {
  f <- core_fixture()
  source2 <- tempfile("relocated-source-")
  dir.create(source2)
  file.copy(list.files(f$source, full.names = TRUE), source2, recursive = TRUE, copy.date = FALSE)
  moved <- core_env$cache_v2_manifest(source2, f$version, f$manifest)
  expect_identical(moved$fingerprint, f$manifest$fingerprint)
  first <- core_env$cache_v2_identity("pip", list(), f$lkup)
  core_env$.cache_v2_state$config$root <- tempfile("relocated-cache-")
  core_env$.cache_v2_state$config$manifest <- moved
  expect_identical(core_env$cache_v2_identity("pip", list(), f$lkup)$key, first$key)
  source_parent <- tempfile("multi-ppp-")
  dir.create(source_parent)
  other <- "20260922_2017_01_02_PROD"
  for (v in c(f$version, other)) {
    dir.create(file.path(source_parent, v))
    file.copy(list.files(f$source, full.names = TRUE), file.path(source_parent, v), recursive = TRUE)
  }
  before <- core_env$cache_v2_manifest(source_parent, c(f$version, other))
  writeLines("new CP auxiliary", file.path(source_parent, f$version, "_aux", "input.bin"))
  after <- core_env$cache_v2_manifest(source_parent, c(f$version, other), before)
  expect_identical(before$versions[[other]]$fingerprint, after$versions[[other]]$fingerprint)
  expect_false(identical(before$versions[[f$version]]$fingerprint, after$versions[[f$version]]$fingerprint))
  writeLines("source, not a cache", file.path(source_parent, other, "_aux", "cache_ids.fst"))
  names <- names(core_env$cache_v2_manifest(source_parent, other)$versions[[other]]$files)
  expect_true("_aux/cache_ids.fst" %in% names)
})

test_that("source guard taints provenance before publication", {
  f <- core_fixture()
  id <- core_env$cache_v2_identity("pip", list(), f$lkup)
  file <- file.path(f$source, "survey_data", "input.bin")
  writeLines("changed input", file)
  expect_error(core_env$cache_v2_put(id, 1), "stats changed")
  expect_false(file.exists(id$path))
  expect_error(core_env$cache_v2_get(id), "tainted")
})

test_that("planned entries stay pinned while runtime entries are bounded", {
  f <- core_fixture()
  pinned <- core_env$cache_v2_identity("pip", list(), f$lkup)
  core_env$.cache_v2_state$config$planned_keys <- pinned$key
  pinned <- core_env$cache_v2_identity("pip", list(), f$lkup)
  expect_false(grepl("/runtime/", pinned$path, fixed = TRUE))
  core_env$cache_v2_put(pinned, 1)
  core_env$.cache_v2_state$config$runtime_max_size <- 1
  runtime <- core_env$cache_v2_identity("pip", list(povline = 4), f$lkup)
  expect_error(core_env$cache_v2_put(runtime, 1), "runtime cache limit")
  expect_true(core_env$cache_v2_get(pinned)$hit)
  core_env$.cache_v2_state$config$runtime_max_size <- 1024^2
  core_env$cache_v2_put(runtime, 1)
  next_id <- core_env$cache_v2_identity("pip", list(povline = 5), f$lkup)
  total <- sum(file.size(c(runtime$path, paste0(runtime$path, ".meta.json"))))
  core_env$.cache_v2_state$config$runtime_max_size <- total + 100
  core_env$cache_v2_put(next_id, 1)
  expect_false(core_env$cache_v2_get(runtime)$hit)
  expect_true(core_env$cache_v2_get(next_id)$hit)
  expect_true(core_env$cache_v2_get(pinned)$hit)
})

test_that("wrapper shares defaults, restores context and does not hash transient state", {
  f <- core_fixture()
  counter <- new.env()
  counter$n <- 0L
  fun <- function(country = "ALL", year = "ALL", povline = 1.9,
                  welfare_type = c("all", "consumption", "income"), lkup,
                  lkup_hash = stop("Legacy hash must not be forced")) {
    counter$n <- counter$n + 1L
    data.table::data.table(povline = povline)
  }
  wrapped <- core_env$.cache_v2_wrap("pip", fun)
  withr::defer(rm(list = "pip", envir = core_env$.cache_v2_state$originals))
  expect_identical(wrapped(lkup = f$lkup)$povline, 1.9)
  expect_identical(wrapped(country = "all", welfare_type = "all", lkup = f$lkup)$povline, 1.9)
  expect_identical(counter$n, 1L)
  expect_null(core_env$cache_v2_context())
  expect_identical(wrapped(povline = 3.001, lkup = f$lkup)$povline, 3.001)
  expect_identical(wrapped(povline = 3.002, lkup = f$lkup)$povline, 3.002)
  expect_identical(counter$n, 3L)
  before <- new.env(parent = emptyenv()); before$fun <- fun
  after <- new.env(parent = emptyenv()); after$fun <- wrapped
  expect_identical(core_env$.cache_v2_code(before), core_env$.cache_v2_code(after))
  before$fun <- function() "edited helper"
  expect_false(identical(core_env$.cache_v2_code(before), core_env$.cache_v2_code(after)))
  before$fun <- fun
  before$defaults <- after$defaults <- list(censor = FALSE, years = c(2000, 2001))
  expect_identical(core_env$.cache_v2_code(before), core_env$.cache_v2_code(after))
  before$defaults$censor <- TRUE
  expect_false(identical(core_env$.cache_v2_code(before), core_env$.cache_v2_code(after)))
  before$defaults <- after$defaults
  before$runtime <- new.env(); before$runtime$counter <- 123
  after$runtime <- new.env(); after$runtime$counter <- 456
  expect_identical(core_env$.cache_v2_code(before), core_env$.cache_v2_code(after))
  before$table <- data.table::data.table(x = 1:3)
  after$table <- data.table::copy(before$table)
  expect_identical(core_env$.cache_v2_code(before), core_env$.cache_v2_code(after))
  after$table[1L, x := 999L]
  expect_identical(core_env$.cache_v2_code(before), core_env$.cache_v2_code(after))
})

test_that("configuration verifies actual build and both opt-in switches", {
  f <- core_fixture()
  withr::local_envvar(c(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE"))
  expect_error(core_env$.cache_v2_assert_wrappers(), "wrappers are not installed")
  assert_wrappers <- core_env$.cache_v2_assert_wrappers
  core_env$.cache_v2_assert_wrappers <- function() invisible(TRUE)
  withr::defer(core_env$.cache_v2_assert_wrappers <- assert_wrappers)
  build <- core_env$cache_v2_build()
  expect_match(build$fingerprint, "^[0-9a-f]{64}$")
  expect_identical(build$fingerprint, core_env$cache_v2_build()$fingerprint)
  expect_identical(build$schema, 2L)
  expect_true(all(c("pipapi", "wbpip", "jsonlite", "qs2", "filelock") %in% names(build$packages)))
  expect_silent(core_env$cache_v2_configure(f$root, f$manifest, build,
                                            intermediate_mode = "read_only",
                                            compute_cache = TRUE))
  build$fingerprint <- "not-the-installed-build"
  expect_error(core_env$cache_v2_configure(f$root, f$manifest, build), "actual installed build")
  Sys.setenv(PIPAPI_APPLY_CACHING = "FALSE")
  expect_error(core_env$cache_v2_configure(f$root, f$manifest, build), "requires")
})

test_that("portable release contract reports all relevant differences", {
  expected <- list(
    schema = 1L,
    r_version = paste(R.version$major,
                      strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][[1L]],
                      sep = "."),
    packages = list(
      pipapi = list(version = "1.5.14", remote_sha = paste(rep("a", 40), collapse = "")),
      wbpip = list(version = "0.1.6", remote_sha = paste(rep("b", 40), collapse = "")),
      plumber = list(version = "1.3.3", remote_sha = NULL),
      jsonlite = list(version = "2.0.0", remote_sha = NULL)
    )
  )
  expected$fingerprint <- core_env$.cache_v2_sha(core_env$.cache_v2_json(
    expected[c("schema", "r_version", "packages")]
  ))
  actual <- expected
  actual$r_version <- "9.9"
  actual$packages$pipapi$remote_sha <- paste(rep("c", 40), collapse = "")
  actual$packages$jsonlite$version <- "99.0.0"
  expected$fingerprint <- core_env$.cache_v2_sha(core_env$.cache_v2_json(
    expected[c("schema", "r_version", "packages")]
  ))
  actual$fingerprint <- core_env$.cache_v2_sha(core_env$.cache_v2_json(
    actual[c("schema", "r_version", "packages")]
  ))
  expect_error(
    core_env$.cache_v2_assert_release(expected, actual),
    paste0("R version: cache=.*server=9.9.*pipapi commit: cache=",
           paste(rep("a", 40), collapse = ""), ", server=",
           paste(rep("c", 40), collapse = ""), ".*jsonlite version"),
    fixed = FALSE
  )
  invalid <- expected
  invalid$fingerprint <- paste(rep("0", 64), collapse = "")
  expect_error(core_env$.cache_v2_assert_release(invalid, expected),
               "fingerprint is invalid")
})

test_that("interprocess locks prevent duplicate publication and enforce timeout", {
  skip_if_not_installed("callr")
  f <- core_fixture()
  id <- core_env$cache_v2_identity("pip", list(), f$lkup)
  cfg <- core_env$.cache_v2_state$config
  counter <- file.path(f$root, "computations.txt")
  worker <- function(source, cfg, identity, counter) {
    e <- new.env(parent = baseenv())
    sys.source(source, e)
    e$.cache_v2_state$config <- cfg
    e$cache_v2_with_lock(identity, {
      hit <- e$cache_v2_get(identity)
      if (!hit$hit) {
        cat("computed\n", file = counter, append = TRUE)
        Sys.sleep(.2)
        e$cache_v2_put(identity, 42)
      }
    })
    e$cache_v2_get(identity)$value
  }
  arguments <- list(file.path(core_root, "R", "cache-v2.R"), cfg, id, counter)
  a <- callr::r_bg(worker, args = arguments, libpath = .libPaths())
  b <- callr::r_bg(worker, args = arguments, libpath = .libPaths())
  withr::defer({ if (a$is_alive()) a$kill(); if (b$is_alive()) b$kill() })
  a$wait(30000); b$wait(30000)
  expect_identical(a$get_result(), 42)
  expect_identical(b$get_result(), 42)
  expect_length(readLines(counter), 1L)
  core_env$cache_v2_with_lock(id, {
    expect_error(callr::r(function(source, cfg, id) {
      e <- new.env(parent = baseenv()); sys.source(source, e)
      e$.cache_v2_state$config <- cfg
      options(pipapi.cache_v2_lock_timeout = 25)
      e$cache_v2_with_lock(id, TRUE)
    }, args = list(file.path(core_root, "R", "cache-v2.R"), cfg, id), libpath = .libPaths()), "Timed out")
  })
})
