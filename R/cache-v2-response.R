# Priority response planning and the exact-byte HTTP cache boundary.

.cache_v2_route_fingerprint <- function(endpoint, path = system.file(
    "plumber/v1/endpoints.R", package = "pipapi")) {
  if (!nzchar(path) || !file.exists(path)) stop("Priority route source is missing.")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  boundary <- grep("^# Endpoints definition", lines)
  if (length(boundary) != 1L) stop("Priority route filter boundary is missing.")
  routes <- grep("^#\\* @get /api/v1/", lines)
  route <- routes[lines[routes] == paste0("#* @get /api/v1/", endpoint)]
  if (length(route) != 1L) stop("Priority route declaration is missing: ", endpoint)
  sections <- grep("^### [a-zA-Z]", lines)
  start <- max(sections[sections < route])
  next_section <- sections[sections > route]
  end <- if (length(next_section)) min(next_section) - 1L else length(lines)
  block <- lines[start:end]
  annotations <- grep("^#\\* @(get|serializer|formatter)\\b", block,
                      value = TRUE)
  # The parameter name and Plumber type affect parsing; prose after them does
  # not. Editing the OpenAPI description must not redeploy a response cache.
  params <- grep("^#\\* @param +[^ ]+", block, value = TRUE)
  params <- sub("^#\\* @param +([^ ]+).*$", "\\1", params)
  normalized <- function(source) {
    paste(deparse(parse(text = paste(source, collapse = "\n"), keep.source = FALSE),
                  width.cutoff = 500L), collapse = "\n")
  }
  handlers <- grep("^## Endpoints: Core endpoints", lines)
  if (length(handlers) != 1L || handlers <= boundary) {
    stop("Shared poverty-line route helper is missing.")
  }
  .cache_v2_sha(.cache_v2_json(list(schema = 3L,
    shared_filters = normalized(lines[seq_len(boundary - 1L)]),
    filter_annotations = grep("^#\\* @(filter|serializer)\\b",
                              lines[seq_len(boundary - 1L)], value = TRUE),
    shared_handler = normalized(lines[(boundary + 1L):(handlers - 1L)]),
    route = normalized(block), annotations = annotations, params = params)))
}

cache_v2_response_spec <- function(endpoint) {
  endpoint <- extract_endpoint(endpoint)
  operations <- c(
    "hp-stacked" = "ui_hp_stacked",
    "pc-charts" = "ui_pc_charts",
    "pc-regional-aggregates" = "ui_pc_regional",
    "cp-charts" = "ui_cp_charts",
    "cp-key-indicators" = "ui_cp_key_indicators"
  )
  if (length(endpoint) != 1L || !endpoint %in% names(operations)) {
    stop("Unsupported priority endpoint")
  }
  endpoint_dependencies <- list(
    `hp-stacked` = list(c("pipapi", "ui_hp_stacked"), c("pipapi", "pip_agg"),
                        c("wbpip", "prod_compute_pip_stats")),
    `pc-charts` = list(c("pipapi", "ui_pc_charts"), c("pipapi", "pip"),
                       c("pipapi", "pip_agg"), c("wbpip", "prod_compute_pip_stats")),
    `pc-regional-aggregates` = list(c("pipapi", "ui_pc_regional"),
                                    c("pipapi", "pip_agg"),
                                    c("wbpip", "prod_compute_pip_stats")),
    `cp-charts` = list(c("pipapi", "ui_cp_charts"), c("pipapi", "pip"),
                       c("wbpip", "prod_compute_pip_stats")),
    `cp-key-indicators` = list(c("pipapi", "ui_cp_key_indicators"), c("pipapi", "pip"),
                               c("wbpip", "prod_compute_pip_stats"))
  )
  serializer_dependencies <- list(c("plumber", "serializer_json"), c("jsonlite", "toJSON"))
  query_dependencies <- list(c("pipapi", "validate_query_parameters"),
    c("pipapi", "parse_parameters"), c("pipapi", "assign_required_params"),
    c("pipapi", "check_parameters_values"), c("pipapi", "cache_v2_effective_args"))
  dependencies <- c(endpoint_dependencies[[endpoint]], query_dependencies,
                    serializer_dependencies)
  fingerprint_cache <- .cache_v2_state$endpoint_fingerprints
  endpoint_fingerprint <- get0(endpoint, envir = fingerprint_cache,
                               inherits = FALSE)
  if (is.null(endpoint_fingerprint)) {
    closure <- .cache_v2_function_fingerprint(dependencies, schema = 3L)
    endpoint_fingerprint <- .cache_v2_sha(.cache_v2_json(list(
      schema = 3L, dependencies = closure$fingerprint,
      operation = unname(operations[[endpoint]]),
      jsonlite_abi = as.character(utils::packageVersion("jsonlite")),
      qs2_abi = as.character(utils::packageVersion("qs2")),
      route = .cache_v2_route_fingerprint(endpoint),
      request_boundary = .cache_v2_function_descriptor("pipapi", "cache_v2_response_request"),
      identity_boundary = .cache_v2_function_descriptor("pipapi", "cache_v2_identity"),
      response_boundary = .cache_v2_function_descriptor("pipapi", "cache_v2_response")
    )))
    assign(endpoint, endpoint_fingerprint, envir = fingerprint_cache)
  }
  list(
    endpoint = endpoint,
    operation = unname(operations[[endpoint]]),
    dependencies = dependencies,
    endpoint_fingerprint = endpoint_fingerprint,
    representation = list(
      serializer = "plumber-json", schema = 3L,
      na = if (endpoint %in% c("pc-charts", "cp-key-indicators")) "null" else "default",
      content_type = "application/json",
      endpoint_fingerprint = endpoint_fingerprint
    )
  )
}

# This entry point accepts parameters already processed by the route filters.
cache_v2_response_request <- function(endpoint, params, lkup) {
  spec <- cache_v2_response_spec(endpoint)
  version <- params$version
  if (length(version) != 1L || is.na(version) ||
      !identical(version, lkup$cache_v2$version)) {
    stop("Priority request requires the attached full data version")
  }
  args <- params
  args$version <- NULL
  if (spec$endpoint == "pc-charts") args$censor <- TRUE
  # Do not drop globally valid arguments unsupported by this UI handler.
  # In particular, format=json must still fail before a cache lookup.
  args <- cache_v2_effective_args(spec$operation, args, lkup)
  identity <- cache_v2_identity(spec$endpoint, args, lkup, spec$representation)
  url_params <- validate_query_parameters(list(argsQuery = args))
  url_params$version <- version
  list(identity = identity, endpoint = spec$endpoint, params = url_params,
       args = args, operation = spec$operation)
}

#' Plan a validated priority response
#'
#' Uses the same query parsing, defaults, and value checks as the HTTP filters.
#' The caller must supply an immutable full version and its attached lookup.
#' This plans query-bearing requests; bare HTTP requests retain their own defaults.
#' @param endpoint Priority endpoint slug or path.
#' @param params Named list of typed query parameters, including full version.
#' @param lkup Version lookup attached with cache_v2_attach().
#' @return A response identity, effective URL parameters, UI arguments, and jobs.
#' @export
cache_v2_priority_request <- function(endpoint, params, lkup) {
  spec <- cache_v2_response_spec(endpoint)
  if (!is.list(params) || is.null(names(params)) ||
      any(!nzchar(names(params))) || anyDuplicated(names(params))) {
    stop("Priority parameters must be a uniquely named list")
  }
  if (length(params$version) != 1L ||
      !identical(params$version, lkup$cache_v2$version)) {
    stop("Priority request requires the attached full data version")
  }
  # Round-trip through the actual parser, including its case and type rules.
  query <- lapply(params, function(x) paste(as.character(x), collapse = ","))
  req <- list(argsQuery = query, PATH_INFO = paste0("/api/v1/", spec$endpoint))
  req$argsQuery <- parse_parameters(validate_query_parameters(req))
  req$args <- req$argsQuery
  req <- assign_required_params(req, lkup$pl_lkup)
  valid <- check_parameters_values(req, lkup$query_controls)
  if (anyNA(valid) || any(!valid)) stop("Invalid priority query parameter values")
  if (!is.null(req$argsQuery$povline)) {
    req$argsQuery$povline <- unique(round(req$argsQuery$povline, 2))
    if (length(req$argsQuery$povline) > 10L) stop("Invalid number of poverty lines")
  }
  request <- cache_v2_response_request(spec$endpoint, req$argsQuery, lkup)
  if (spec$endpoint %in% c("cp-charts", "cp-key-indicators")) {
    cp <- cache_v2_cp_lookup(lkup)
    if (!all(request$args$country %in% cp$svy_lkup$country_code)) {
      stop("Priority seed country is not available in the Country Profiles lookup")
    }
  }
  request$dependencies <- cache_v2_priority_dependencies(request, lkup)
  request
}

#' Plan prerequisites even when a priority response already exists
#'
#' In particular, a surviving Country Profiles response or outer computation
#' does not establish that the shared all-country pip result still exists.
#' @param request A request returned by cache_v2_priority_request().
#' @param lkup The full version lookup used to plan the request.
#' @return A list of jobs with identity, operation, args, and lookup_variant.
#' @export
cache_v2_priority_dependencies <- function(request, lkup) {
  if (!identical(request$params$version, lkup$cache_v2$version)) {
    stop("Priority dependencies require the request's full data version")
  }
  endpoint <- cache_v2_response_spec(request$endpoint)$endpoint
  ui <- request$args
  args <- list(country = "ALL", year = "ALL", povline = ui$povline)
  variant <- "full"
  operation <- "pip"
  if (endpoint %in% c("cp-charts", "cp-key-indicators")) {
    variant <- "cp"
    lkup <- cache_v2_cp_lookup(lkup)
    args$fill_gaps <- FALSE
    args$reporting_level <- "all"
  } else if (endpoint == "pc-charts") {
    args$country <- ui$country
    args$year <- ui$year
    args$fill_gaps <- ui$fill_gaps
    args$group_by <- ui$group_by
    args$reporting_level <- ui$reporting_level
    args$censor <- TRUE
    # The current UI handler does not forward welfare_type to pip().
  } else if (endpoint == "hp-stacked") {
    operation <- "pip_agg"
    args$welfare_type <- "all"
    args$reporting_level <- "all"
    args$censor <- FALSE
    args$additional_ind <- FALSE
  } else {
    operation <- "pip_agg"
    args$country <- ui$country
    args$year <- ui$year
    args$group_by <- "wb"
    args$reporting_level <- "all"
    args$censor <- TRUE
  }
  args <- cache_v2_effective_args(operation, args, lkup)
  list(list(identity = cache_v2_identity(operation, args, lkup),
            operation = operation, args = args, lookup_variant = variant))
}

cache_v2_response_event <- function(req, event, identity, seconds = 0) {
  if (is.null(req$.cache_v2)) req$.cache_v2 <- list()
  req$.cache_v2[[event]] <- list(count = 1L, seconds = seconds)
  .cache_v2_count(paste0("response_", event), identity$descriptor$operation, seconds)
  hook <- getOption("pipapi.cache_v2_response_hook")
  if (is.function(hook)) hook(event = event, identity = identity, seconds = seconds)
  invisible(NULL)
}

cache_v2_response <- function(req, res, endpoint, lkup) {
  spec <- cache_v2_response_spec(endpoint)
  params <- req$argsQuery
  params$version <- NULL
  params$lkup <- lkup
  if (spec$endpoint == "pc-charts") params$censor <- TRUE
  compute <- function() {
    operation <- spec$operation
    original <- .cache_v2_state$originals[[operation]]
    if (!is.function(original)) original <- get(operation, envir = asNamespace("pipapi"))
    with_req_timeout(do.call(original, params))
  }
  if (!cache_v2_enabled() ||
      isTRUE(getOption("pipapi.query_live_data"))) return(compute())

  if (is.null(cache_v2_context(lkup))) return(compute())

  request <- cache_v2_response_request(endpoint, req$argsQuery, lkup)
  identity <- request$identity
  .cache_v2_guard(identity, inputs = TRUE)
  send <- function(bytes, content_type, state) {
    # Plumber's setHeader appends, so remove the generic ETag and the first
    # serializer's Content-Type before setting the exact representation headers.
    res$headers <- res$headers[!tolower(names(res$headers)) %in%
      c("etag", "content-type", "x-pipapi-cache", "x-pipapi-cache-key")]
    res$setHeader("ETag", paste0('"', digest::digest(bytes, algo = "sha256", serialize = FALSE), '"'))
    res$setHeader("X-Pipapi-Cache", state)
    res$setHeader("X-Pipapi-Cache-Key", identity$key)
    res$serializer <- plumber::serializer_content_type(content_type, base::identity)
    bytes
  }
  hit <- cache_v2_get(identity)
  if (isTRUE(hit$hit)) {
    cache_v2_response_event(req, "hit", identity)
    return(send(hit$value, hit$metadata$content_type, "HIT"))
  }
  cache_v2_with_lock(identity, {
    hit <- cache_v2_get(identity)
    if (isTRUE(hit$hit)) {
      cache_v2_response_event(req, "hit", identity)
      send(hit$value, hit$metadata$content_type, "HIT")
    } else {
      cache_v2_response_event(req, "miss", identity)
      start <- proc.time()[["elapsed"]]
      value <- compute()
      cache_v2_response_event(req, "ui", identity, proc.time()[["elapsed"]] - start)
      # Timeouts can have status 200. Never admit a failure object.
      valid <- identical(as.integer(res$status), 200L) &&
        !inherits(value, c("error", "condition", "try-error")) &&
        !(is.list(value) && (!is.null(value$error) || identical(value$ok, FALSE)))
      if (!valid) {
        res$setHeader("X-Pipapi-Cache", "BYPASS")
        value
      } else {
        start <- proc.time()[["elapsed"]]
        # Plumber assigns the annotated route serializer before this handler.
        # Serialize once, then replace it with a raw-byte identity serializer.
        serializer <- res$serializer
        if (!is.function(serializer)) stop("Priority route has no serializer")
        serialized <- serializer(value, req, res, function(req, res, err) stop(err))
        cache_v2_response_event(req, "serialize", identity, proc.time()[["elapsed"]] - start)
        bytes <- serialized$body
        if (is.character(bytes) && length(bytes) == 1L) bytes <- charToRaw(enc2utf8(bytes))
        if (!is.raw(bytes) || !identical(as.integer(serialized$status), 200L)) {
          stop("Priority serializer did not return a successful byte response")
        }
        headers <- serialized$headers
        content_name <- names(headers)[tolower(names(headers)) == "content-type"]
        content_type <- if (length(content_name)) headers[[content_name[[1L]]]] else NULL
        if (is.null(content_type)) {
          response_headers <- res$headers
          content_name <- names(response_headers)[tolower(names(response_headers)) == "content-type"]
          if (length(content_name)) content_type <- response_headers[[content_name[[1L]]]]
        }
        if (is.null(content_type)) stop("Priority serializer did not set Content-Type")
        cache_v2_put(identity, bytes, content_type = content_type)
        send(bytes, content_type, "MISS")
      }
    }
  })
}
