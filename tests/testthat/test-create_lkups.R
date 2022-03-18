dirs_names <- c("00000001",
                "20210401",
                "20210920_2011_02_02_PROD",
                "20220317_2011_02_02_INT",
                "20220317_2017_01_01_INT",
                "20220408",
                "20220408_2011_02_02_PROD")

test_that("id_valid_dirs correctly identifies valid directories", {
  out <- id_valid_dirs(dirs_names = dirs_names,
                       vintage_pattern = get_vintage_pattern_regex())

  expect_equal(out, c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE))
})
