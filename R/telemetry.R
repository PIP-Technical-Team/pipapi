# ---- tiny telemetry (ID + timings) --------------------------------------

# Monotonic wall time for duration (in seconds)
.now <- function() {
  proc.time()[["elapsed"]]
}

# Generate a request id AND return a decoded structure
# - id_raw: "milliseconds-since-epoch-random"
# - timestamp: POSIXct in UTC
# - random: integer
.req_id <- function() {
  ts_ms_num <- as.numeric(Sys.time()) * 1000
  ts_ms_chr <- format(ts_ms_num, scientific = FALSE, trim = TRUE)
  rnd       <- sample.int(1e9, 1)
  id_raw    <- paste0(ts_ms_chr, "-", rnd)

  list(
    id_raw    = id_raw,
    timestamp = as.POSIXct(as.numeric(ts_ms_chr) / 1000,
                           origin = "1970-01-01", tz = "UTC"),
    random    = rnd
  )
}
