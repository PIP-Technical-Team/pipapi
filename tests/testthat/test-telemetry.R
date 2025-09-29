test_that(".now() returns a numeric scalar that increases over time", {
  t1 <- pipapi:::.now()
  Sys.sleep(0.01)
  t2 <- pipapi:::.now()
  expect_type(t1, "double")
  expect_length(t1, 1)
  expect_gt(t2, t1)
})

test_that("req_id() returns unique UUID strings", {
  id1 <- pipapi:::.req_id()
  id2 <- pipapi:::.req_id()
  expect_type(id1$id_raw, "character")
  expect_match(id1$id_raw, "^[0-9a-f\\-]+$")
  expect_false(id1$id_raw == id2$id_raw)
})
