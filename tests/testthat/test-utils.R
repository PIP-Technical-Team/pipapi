test_that("create_empty_response() works as expected", {
  res1 <- create_empty_response()
  res2 <- pipapi::empty_response
  expect_equal(res1, res2)

  skip("see issue #56")
  res3 <- pip("AGO", 2018, lkup = lkups, fill_gaps = FALSE)
  expect_true(all(names(res3) %in% names(res1)))

  skip("see issue #56")
  res4 <- pip("AGO", 2018, lkup = lkups, fill_gaps = TRUE)
  expect_true(all(names(res4) %in% names(res1)))
})
