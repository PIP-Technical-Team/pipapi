skip_if(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER') == "")
data_folder_root <- Sys.getenv('PIPAPI_DATA_ROOT_FOLDER')
tables <- c('gdp', 'pce', 'pop', 'cpi', 'ppp')

test_that("get_aux_table() works", {

  dl <- lapply(tables, function(x){
    get_aux_table(data_folder_root, table = x)
  })
  expect_equal(length(dl), length(tables))

})
