library(plumber)
library(logger)
library(glue)

endpoints_path <- system.file("plumber/v1/endpoints.R", package = "pipapi")
convert_empty <- pipapi:::convert_empty

plumber::pr(endpoints_path) %>%
  # pre-route log
  plumber::pr_hook("preroute", function() {
    tictoc::tic() # Start timer for log info
  }) %>%
  # post-route log
  plumber::pr_hook("postroute", function(req, res) {
    end_route <- tictoc::toc(quiet = TRUE)
    log_info('{convert_empty(req$REMOTE_ADDR)} {convert_empty(req$REQUEST_METHOD)} {convert_empty(req$PATH_INFO)} {convert_empty(req$QUERY_STRING)}  {convert_empty(res$status)} {round(end_route$toc - end_route$tic, digits = getOption("digits", 6))}')
  }) %>%
  # pre-serialization log
  plumber::pr_hook("preserialize", function() {
    tictoc::tic()
  }) %>%
  # post-serialization log
  plumber::pr_hook("postserialize", function(req) {
    end_serial <- tictoc::toc(quiet = TRUE)
    log_info('{convert_empty(req$PATH_INFO)} {round(end_serial$toc - end_serial$tic, digits = getOption("digits", 6))}') }) %>%
  plumber::pr_hook("exit", function() {
    log_info('Bye bye: {proc.time()[["elapsed"]]}')
  })
