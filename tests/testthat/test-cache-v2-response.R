response_fixture <- function(.env = parent.frame()) {
  withr::local_envvar(c(PIPAPI_CACHE_V2 = "TRUE", PIPAPI_APPLY_CACHING = "TRUE",
                       PLUMBER_REQ_TIMEOUT = "0"), .local_envir = .env)
  withr::local_options(list(pipapi.query_live_data = FALSE), .local_envir = .env)
  local_mocked_bindings(cache_v2_build = function() list(fingerprint = "fixture-build"),
                       .package = "pipapi", .env = .env)
  state <- pipapi:::.cache_v2_state
  previous <- state$config
  previous_stats <- state$stats
  withr::defer(state$config <- previous, envir = .env)
  withr::defer(state$stats <- previous_stats, envir = .env)
  root <- tempfile("response-cache-")
  withr::defer(unlink(root, recursive = TRUE), envir = .env)
  versions <- c("20260922_2021_01_02_PROD", "20260922_2017_01_02_PROD",
                "20260430_2021_01_02_PROD")
  sources <- file.path(root, "sources")
  for (version in versions) {
    for (directory in c("_aux", "estimations", "survey_data", "lineup_data")) {
      dir.create(file.path(sources, version, directory), recursive = TRUE)
      writeLines("fixture", file.path(sources, version, directory, "fixture.txt"))
    }
  }
  manifest <- cache_v2_manifest(sources, versions)
  cache_v2_configure(file.path(root, "cache"), manifest, cache_v2_build())
  chr <- function(values) list(type = "character", values = values)
  controls <- list(country = chr(c("ALL", "IDN", "AGO", "ZZZ")), year = chr(c("ALL", "2000")),
    version = chr(versions), povline = list(type = "numeric", values = list(min = 0, max = 100)),
    fill_gaps = list(type = "logical"), welfare_type = chr(c("all", "consumption", "income")),
    reporting_level = chr(c("all", "national", "rural", "urban")),
    group_by = chr(c("none", "wb")), format = chr(c("json", "csv")))
  lkup <- list(query_controls = controls,
    pl_lkup = data.frame(poverty_line = 3, is_default = TRUE),
    svy_lkup = data.table::data.table(country_code = c("IDN", "AGO", "ZZZ"),
      display_cp = c(1, 1, 0), region_code = c("EAP", "SSA", "EAP")),
    cache_data_id = list(hash_ui_cp = "cp", hash_pip = "pip", hash_pip_grp = "agg"))
  lkups <- list(versions = versions, latest_release = versions[1],
    versions_paths = setNames(lapply(versions, function(v) cache_v2_attach(lkup, v)), versions))
  list(root = root, lkups = lkups, lkup = lkups$versions_paths[[1]], version = versions[1])
}

response_ui_mocks <- function(.env = parent.frame()) {
  counters <- new.env(parent = emptyenv())
  counters$ui <- 0L
  counters$serialize <- 0L
  counters$mode <- "ok"
  counters$values <- list(
    ui_hp_stacked = data.frame(region_code = c("WLD", "EAP"), pop_in_poverty = c(12, NA_real_)),
    ui_pc_charts = data.frame(country_code = c("IDN", "AGO"), reporting_pop = c(1.25, NA_real_)),
    ui_pc_regional = data.frame(region_code = c("WLD", "EAP"), reporting_pop = c(1.25, NA_real_)),
    ui_cp_charts = list(IDN = list(pov_charts = list(data.frame(year = c(2000, 2001), headcount = c(NA_real_, .2))), units = 1e6)),
    ui_cp_key_indicators = list(list(headcount = data.frame(country_code = "IDN", headcount = NA_real_), gini = NULL)))
  replacements <- lapply(names(counters$values), function(operation) {
    fun <- get(operation, envir = asNamespace("pipapi"))
    original <- attr(fun, "cache_v2_original", exact = TRUE)
    if (!is.null(original)) fun <- original
    scope <- new.env(parent = environment(fun))
    scope$counters <- counters
    scope$operation <- operation
    body(fun) <- quote({
      counters$ui <- counters$ui + 1L
      if (counters$mode == "error") stop("fixture UI failure")
      if (counters$mode == "timeout") return(list(ok = FALSE, error = "Request exceeded timeout of 1 seconds", elapsed = 1))
      if (counters$mode == "invalid") return(list(error = "fixture invalid result"))
      counters$values[[operation]]
    })
    environment(fun) <- scope
    fun
  })
  names(replacements) <- names(counters$values)
  do.call(local_mocked_bindings, c(replacements, list(.package = "pipapi", .env = .env)))
  withr::local_options(list(pipapi.cache_v2_response_hook = function(event, identity, seconds) {
    if (event == "serialize") counters$serialize <- counters$serialize + 1L
  }), .local_envir = .env)
  counters
}

response_app <- function(fixture) {
  env <- new.env(parent = asNamespace("pipapi"))
  env$lkups <- fixture$lkups
  env$pr_hook <- plumber::pr_hook
  env$forward <- plumber::forward
  plumber::Plumber$new(system.file("plumber/v1/endpoints.R", package = "pipapi"), envir = env)
}

response_http <- function(app, endpoint, params = list()) {
  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- "GET"
  req$PATH_INFO <- paste0("/api/v1/", endpoint)
  req$QUERY_STRING <- paste(vapply(names(params), function(name) {
    paste0(name, "=", urltools::url_encode(paste(params[[name]], collapse = ",")))
  }, character(1)), collapse = "&")
  req$rook.input <- list(read = function() raw(), rewind = function() NULL,
                         read_lines = function() character())
  app$call(req)
}

response_bytes <- function(body) {
  if (is.raw(body)) body else charToRaw(enc2utf8(as.character(body)))
}

test_that("all six templates return original serialized bytes and skip work on hits", {
  fixture <- response_fixture()
  counters <- response_ui_mocks()
  app <- response_app(fixture)
  templates <- list(
    list(endpoint = "hp-stacked", args = list()),
    list(endpoint = "pc-charts", args = list(country = "all", year = "all", fill_gaps = FALSE)),
    list(endpoint = "pc-charts", args = list(country = "all", year = "all", fill_gaps = TRUE)),
    list(endpoint = "pc-regional-aggregates", args = list(country = "all", year = "all")),
    list(endpoint = "cp-charts", args = list(country = "IDN")),
    list(endpoint = "cp-key-indicators", args = list(country = "IDN")))
  keys <- character()
  for (template in templates) {
    params <- c(template$args, list(povline = 3, version = fixture$version))
    request <- cache_v2_priority_request(template$endpoint, params, fixture$lkup)
    baseline <- withr::with_envvar(c(PIPAPI_CACHE_V2 = "FALSE"), response_http(app, template$endpoint, params))
    cold <- response_http(app, template$endpoint, params)
    ui <- counters$ui
    encodes <- counters$serialize
    # The builder sends the package's effective URL parameters, not its input.
    hit <- response_http(app, template$endpoint, request$params)
    expect_equal(cold$status, 200)
    expect_equal(hit$status, baseline$status)
    expect_identical(cold$body, response_bytes(baseline$body))
    expect_identical(hit$body, cold$body)
    expect_identical(hit$headers[["Content-Type"]], baseline$headers[["Content-Type"]])
    expect_equal(sum(tolower(names(hit$headers)) == "etag"), 1)
    expect_equal(sum(tolower(names(cold$headers)) == "content-type"), 1)
    expect_identical(cold$headers[["X-Pipapi-Cache"]], "MISS")
    expect_identical(hit$headers[["X-Pipapi-Cache"]], "HIT")
    expect_identical(hit$headers[["X-Pipapi-Cache-Key"]], request$identity$key)
    expect_identical(hit$headers[["ETag"]], paste0('"', digest::digest(hit$body, algo = "sha256", serialize = FALSE), '"'))
    expect_false(identical(hit$headers[["X-Request-ID"]], cold$headers[["X-Request-ID"]]))
    expect_identical(counters$ui, ui)
    expect_identical(counters$serialize, encodes)
    expect_identical(cache_v2_get(request$identity)$value, cold$body)
    keys <- c(keys, request$identity$key)
  }
  expect_length(unique(keys), 6)
  expect_identical(counters$serialize, 6L)
})

test_that("errors, invalid arguments and status-200 timeouts are not cached", {
  fixture <- response_fixture()
  counters <- response_ui_mocks()
  app <- response_app(fixture)
  params <- list(country = "IDN", povline = 3, version = fixture$version)
  request <- cache_v2_priority_request("cp-charts", params, fixture$lkup)
  for (mode in c("error", "timeout", "invalid")) {
    counters$mode <- mode
    result <- response_http(app, "cp-charts", params)
    expect_equal(result$status, if (mode == "error") 500 else 200)
    expect_false(cache_v2_get(request$identity)$hit)
  }
  counters$mode <- "ok"
  response_http(app, "cp-charts", params)
  ui <- counters$ui
  for (extra in list(list(format = "json"), list(year = "ALL"), list(fill_gaps = FALSE))) {
    expect_equal(response_http(app, "cp-charts", c(params, extra))$status, 500)
    expect_error(cache_v2_priority_request("cp-charts", c(params, extra), fixture$lkup))
  }
  expect_equal(response_http(app, "cp-charts", modifyList(params, list(povline = -1)))$status, 404)
  expect_error(cache_v2_priority_request("cp-charts", modifyList(params, list(povline = -1)), fixture$lkup))
  expect_identical(counters$ui, ui)
  expect_error(cache_v2_priority_request("pc-charts", c(params, list(group_by = "wb")), fixture$lkup))
})

test_that("effective keys preserve route defaults, version, representation and order", {
  fixture <- response_fixture()
  params <- list(country = "idn", povline = "3.000", version = fixture$version)
  a <- cache_v2_priority_request("cp-charts", params, fixture$lkup)
  b <- cache_v2_priority_request("/api/v1/cp-charts", modifyList(params, list(country = "IDN", povline = 3)), fixture$lkup)
  expect_identical(a$identity$key, b$identity$key)
  expect_identical(a$params$country, "IDN")
  expect_identical(a$params$version, fixture$version)
  expect_identical(a$args$pop_units, 1e6)
  expect_false("pop_units" %in% names(a$params))
  keys <- vapply(fixture$lkups$versions, function(version) {
    cache_v2_priority_request("cp-charts", modifyList(params, list(version = version)), fixture$lkups$versions_paths[[version]])$identity$key
  }, character(1))
  expect_length(unique(keys), 3)
  other_rep <- cache_v2_identity("cp-charts", a$args, fixture$lkup, list(serializer = "other"))
  expect_false(identical(a$identity$key, other_rep$key))
  expect_error(cache_v2_priority_request("cp-charts", params, fixture$lkups$versions_paths[[2]]), "full data version")
  expect_error(cache_v2_priority_dependencies(a, fixture$lkups$versions_paths[[2]]), "full data version")
  ordered <- cache_v2_priority_request("hp-stacked", list(povline = c(3, 4), version = fixture$version), fixture$lkup)
  reversed <- cache_v2_priority_request("hp-stacked", list(povline = c(4, 3), version = fixture$version), fixture$lkup)
  expect_false(identical(ordered$identity$key, reversed$identity$key))
  bare <- pipapi:::cache_v2_response_request("hp-stacked", list(version = fixture$version), fixture$lkup)
  query <- cache_v2_priority_request("hp-stacked", list(version = fixture$version), fixture$lkup)
  expect_equal(bare$args$povline, 1.9)
  expect_equal(query$args$povline, 3)
  expect_false(identical(bare$identity$key, query$identity$key))
})

test_that("CP shared prerequisites remain repairable independently of surviving seed responses", {
  fixture <- response_fixture()
  counters <- response_ui_mocks()
  app <- response_app(fixture)
  params <- list(country = "IDN", povline = 3, version = fixture$version)
  charts <- cache_v2_priority_request("cp-charts", params, fixture$lkup)
  indicators <- cache_v2_priority_request("cp-key-indicators", params, fixture$lkup)
  shared <- charts$dependencies[[1]]
  expect_identical(shared$identity$key, indicators$dependencies[[1]]$identity$key)
  expect_identical(shared$lookup_variant, "cp")
  expect_identical(shared$operation, "pip")
  expect_identical(shared$args$country, "ALL")
  expect_identical(shared$args$censor, FALSE)
  expect_identical(shared$args$welfare_type, "all")
  expect_identical(shared$args$group_by, "none")
  cache_v2_put(shared$identity, data.frame(country_code = c("IDN", "AGO")))
  response_http(app, "cp-charts", params)
  expect_true(cache_v2_get(charts$identity)$hit)
  unlink(c(shared$identity$path, paste0(shared$identity$path, ".meta.json")))
  jobs <- cache_v2_priority_dependencies(charts, fixture$lkup)
  expect_length(jobs, 1)
  expect_false(cache_v2_get(jobs[[1]]$identity)$hit)
  expect_true(cache_v2_get(charts$identity)$hit)
  ui <- counters$ui
  response_http(app, "cp-charts", modifyList(params, list(country = "AGO")))
  expect_identical(counters$ui, ui + 1L)
  expect_error(cache_v2_priority_request("cp-charts", modifyList(params, list(country = "ZZZ")), fixture$lkup), "seed country")
  pc <- cache_v2_priority_request("pc-charts", list(country = "ALL", year = "ALL", povline = 3, version = fixture$version), fixture$lkup)
  expect_identical(pc$dependencies[[1]]$lookup_variant, "full")
  expect_identical(pc$dependencies[[1]]$args$censor, TRUE)
  expect_false(identical(shared$identity$key, pc$dependencies[[1]]$identity$key))
  hp <- cache_v2_priority_request("hp-stacked", list(povline = 3, version = fixture$version), fixture$lkup)
  regional <- cache_v2_priority_request("pc-regional-aggregates", list(povline = 3, version = fixture$version), fixture$lkup)
  expect_identical(hp$dependencies[[1]]$args$group_by, "wb")
  expect_identical(hp$dependencies[[1]]$args$censor, FALSE)
  expect_identical(regional$dependencies[[1]]$args$censor, TRUE)
  expect_false(identical(hp$dependencies[[1]]$identity$key, regional$dependencies[[1]]$identity$key))
})

test_that("dependency jobs match the actual UI inner calls", {
  fixture <- response_fixture()
  captured <- new.env(parent = emptyenv())
  replacements <- lapply(c("pip", "pip_agg"), function(operation) {
    fun <- get(operation, envir = asNamespace("pipapi"))
    original <- attr(fun, "cache_v2_original", exact = TRUE)
    if (!is.null(original)) fun <- original
    scope <- new.env(parent = environment(fun))
    scope$captured <- captured
    scope$operation <- operation
    body(fun) <- quote({
      args <- as.list(environment())
      actual_lkup <- args$lkup
      args$lkup <- NULL
      captured$identity <- cache_v2_identity(operation, args, actual_lkup)
      stop("captured inner call")
    })
    environment(fun) <- scope
    fun
  })
  names(replacements) <- c("pip", "pip_agg")
  do.call(local_mocked_bindings, c(replacements, list(.package = "pipapi", .env = environment())))
  for (endpoint in c("hp-stacked", "pc-charts", "pc-regional-aggregates", "cp-charts", "cp-key-indicators")) {
    params <- list(povline = 3, version = fixture$version)
    if (startsWith(endpoint, "cp-")) params$country <- "IDN"
    if (endpoint == "pc-charts") params$welfare_type <- "income"
    request <- cache_v2_priority_request(endpoint, params, fixture$lkup)
    fun <- get(request$operation, envir = asNamespace("pipapi"))
    original <- attr(fun, "cache_v2_original", exact = TRUE)
    if (!is.null(original)) fun <- original
    expect_error(do.call(fun, c(request$args, list(lkup = fixture$lkup))), "captured inner call")
    expect_identical(captured$identity$key, request$dependencies[[1]]$identity$key)
  }
})

test_that("serializer failures and non-200 results cannot publish artifacts", {
  fixture <- response_fixture()
  response_ui_mocks()
  params <- list(povline = 3, version = fixture$version)
  request <- cache_v2_priority_request("hp-stacked", params, fixture$lkup)
  req <- new.env(parent = emptyenv())
  req$argsQuery <- params
  res <- plumber:::PlumberResponse$new(function(val, req, res, errorHandler) stop("serializer failed"))
  expect_error(pipapi:::cache_v2_response(req, res, "hp-stacked", fixture$lkup), "serializer failed")
  expect_false(cache_v2_get(request$identity)$hit)
  res <- plumber:::PlumberResponse$new(plumber::serializer_json())
  res$status <- 503L
  out <- pipapi:::cache_v2_response(req, res, "hp-stacked", fixture$lkup)
  expect_s3_class(out, "data.frame")
  expect_false(cache_v2_get(request$identity)$hit)
})
