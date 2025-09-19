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

# ---- plumber setup ---------------------------------------------------
library(plumber)

endpoints_path <- system.file("plumber/v1/endpoints.R", package = "pipapi")
api_spec_path <- system.file("plumber/v1/openapi.yaml", package = "pipapi")

pr <- plumber::pr(endpoints_path) |>
  # (Step 4) simplified error handler
  plumber::pr_set_error(function(req, res, err) {
    rid <- tryCatch(req$.id, error = function(e) NA)
    method <- req$REQUEST_METHOD
    path   <- req$PATH_INFO

    # structured log to stderr (good for docker logs)
    cat(sprintf(
      '{"level":"error","id":"%s","method":"%s","path":"%s","msg":%s}\n',
      as.character(rid), method, path,
      jsonlite::toJSON(err$message, auto_unbox = TRUE)
    ), file = stderr())

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
  # Inject version into OpenAPI spec
  plumber::pr_set_api_spec(api = function(spec) {
    spec$info$version <- as.character(utils::packageVersion("pipapi"))
    spec
  }) |>
  plumber::pr_set_api_spec(yaml::read_yaml(api_spec_path))

pr
