test_that("API loader returns the Plumber router created by the script", {
  path <- tempfile(fileext = ".R")
  writeLines("pr <- structure(list(marker = 'fixture'), class = 'Plumber')", path)
  env <- new.env(parent = baseenv())
  router <- pipapi:::.load_api_router(path, env)
  expect_s3_class(router, "Plumber")
  expect_identical(router$marker, "fixture")
  expect_identical(env$pr, router)
})

test_that("API loader rejects scripts without a Plumber router", {
  path <- tempfile(fileext = ".R")
  writeLines("value <- 'not a router'", path)
  expect_error(
    pipapi:::.load_api_router(path, new.env(parent = baseenv())),
    "did not create a Plumber router"
  )
})
