# ---- process-level thread caps (Step 3) -----------------------------
# Avoid oversubscribing on multi-core servers (common cause of hangs under load)
Sys.setenv(
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS      = "1",
  OMP_NUM_THREADS      = "1"
)

ncores <- parallel::detectCores(logical = FALSE)

data.table::setDTthreads(max(1L, ncores))
collapse::set_collapse(nthreads	= max(1L, ncores))
fst::threads_fst(max(1L, ncores))

# ---- build router --------------------------------------------------------
library(plumber)

endpoints_path <- system.file("plumber/v1/endpoints.R", package = "pipapi")
api_spec_path  <- system.file("plumber/v1/openapi.yaml", package = "pipapi")

pr <- plumber::pr(endpoints_path) |>

  # ---- Post-route: log route duration (handler time) ----
plumber::pr_hook("postroute", function(req, res) {
  if (!is.null(req$.start)) {
    dur <- .now() - req$.start
    cat(
      sprintf(
        '{"type":"route","id":"%s","method":"%s","path":"%s","status":%s,"dur_s":%.6f}\n',
        req$.id %||% "",
        req$.meth %||% "",
        req$.path %||% "",
        as.character(res$status %||% NA_integer_),
        dur
      ),
      file = stderr()
    )
  }
}) |>

  # ---- Pre-serialization: mark when we start serializing ----
plumber::pr_hook("preserialize", function(req, res) {
  req$.ser0 <- .now()
}) |>

  # ---- Post-serialization: log serialization duration ----
plumber::pr_hook("postserialize", function(req, res) {
  if (!is.null(req$.ser0) && !is.na(req$.ser0)) {
    ser_dur <- .now() - req$.ser0
    cat(
      sprintf(
        '{"type":"serialize","id":"%s","path":"%s","dur_s":%.6f}\n',
        req$.id %||% "",
        req$.path %||% "",
        ser_dur
      ),
      file = stderr()
    )
  }
}) |>

  # ---- Exit hook: when process shuts down ----
plumber::pr_hook("exit", function() {
  cat(
    sprintf(
      '{"type":"exit","uptime_s":%.2f}\n',
      proc.time()[["elapsed"]]
    ),
    file = stderr()
  )
}) |>

  # ---- Global error handler ----
plumber::pr_set_error(function(req, res, err) {
  method <- req$REQUEST_METHOD %||% ""
  path   <- req$PATH_INFO %||% ""
  rid    <- req$.id %||% "NA"

  cat(
    sprintf(
      '{"type":"error","id":"%s","method":"%s","path":"%s","msg":%s}\n',
      rid, method, path, jsonlite::toJSON(err$message, auto_unbox = TRUE)
    ),
    file = stderr()
  )

  res$status <- 500
  res$body <- jsonlite::toJSON(list(
    error      = "Internal Server Error",
    message    = err$message,
    path       = path,
    method     = method,
    request_id = rid
  ), auto_unbox = TRUE)

  res
}) |>

  # ---- API Spec (with dynamic version injection) ----
plumber::pr_set_api_spec(api = function(spec) {
  spec$info$version <- as.character(utils::packageVersion("pipapi"))
  spec
}) |>
  plumber::pr_set_api_spec(yaml::read_yaml(api_spec_path))

pr
