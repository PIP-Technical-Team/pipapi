skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
# lkup_path <- test_path("testdata", "lkup.rds")
# lkup      <- readRDS(lkup_path)

data_folder_root <- lkups$data_root


tables <- c("gdp", "pce", "pop", "cpi", "ppp")

test_that("get_aux_table() works", {
  dl <- lapply(tables, function(x) {
    get_aux_table(data_folder_root, table = x)
  })
  expect_equal(length(dl), length(tables))
})

test_that("get_aux_table() returns an error", {
  expect_error(pipapi::get_aux_table(data_folder_root, "../survey_data/ARG_1980_EPH_D2_INC_GROUP.fst"),
               "Error opening fst file for reading, please check access rights and file availability")
})
