Sys.setenv("PIPAPI_APPLY_CACHING" = "TRUE")
Sys.setenv("PIPAPI_CACHE_MAX_SIZE" = "5368709120")

library(RestRserve)
library(pipapi)

app = Application$new()

pip_handler = function(.req, .res) {
  #out <- do.call(pipapi::pip, .req$parameters_query)
  lkups <- create_versioned_lkups(data_dir = "../../../../pip-fake-data/")
  lkup <-  lkups$versions_paths$`20211212_2011_01_01_PROD`
  out <- pip(country = "SSA", year = "all", povline = 1.986999, lkup = lkup)
 .res$set_body(jsonlite::toJSON(out))
}

app$add_get(path = "/pip", FUN = pip_handler)


test_handler <- function(.req, .res) {
  .res$set_body("Hello")
}

app$add_get(path = "/test", FUN = test_handler)

backend = BackendRserve$new()
backend$start(app, http_port = 8080)
