skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER") == "")
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"))
data_folder_root <- lkups$versions_paths[[lkups$latest_release]]$data_root

tables <- c("gdp", "pce", "pop", "cpi", "ppp")

test_that("get_aux_table() works", {
  dl <- lapply(tables, function(x) {
    get_aux_table(data_folder_root, table = x)
  })
  expect_equal(length(dl), length(tables))

  gdp_long <- pipapi::get_aux_table(data_folder_root, "gdp", long_format = TRUE)
  expect_equal(ncol(gdp_long), 4)
})

test_that("get_aux_table() returns an error", {
  expect_error(pipapi::get_aux_table(data_folder_root, "../survey_data/ARG_1980_EPH_D2_INC_GROUP.fst"),
               "Error opening fst file for reading, please check access rights and file availability")

  expect_error(pipapi::get_aux_table(data_folder_root, "decomposition", long_format = TRUE),
               "Please select one of cpi, ppp, gdp, pce and pop tables to get data in long format.")
})
