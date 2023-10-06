skip_if(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") == "")

data_dir <- fs::path(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkups <- create_versioned_lkups(data_dir = data_dir)
lkup <-  lkups$versions_paths[[lkups$latest_release]]

aux_files   <- readRDS(testthat::test_path("testdata", "mock_aux_files.rds"))
valid_years <- readRDS(testthat::test_path("testdata", "mock_valid_years.rds"))
aggs      <- aux_files$regions
off_gt <-  c("region") # c("region", "world")--
alt_agg <- aggs[["region_code"]][!aggs[["grouping_type"]] %in% off_gt]
ext_reg <- c("ALL", "WLD")
off_reg <- aggs[["region_code"]][aggs[["grouping_type"]] %in% off_gt]
off_reg_ext <- sort(unique(c(off_reg, ext_reg)))
user_gts <- unique(aggs[["grouping_type"]])
off_reg <- aggs[["region_code"]][aggs[["grouping_type"]] %in% off_gt]
md <- aux_files$missing_data

test_that("filter_md works when year is passed as a character", {
  ctr_alt_agg <- c("BEN", "BFA", "CAF", "CIV", "CMR", "COG", "CPV", "GAB", "GHA", "GIN",
                   "GMB", "GNB", "GNQ", "LBR", "MLI", "MRT", "NER", "NGA", "SEN", "SLE",
                   "TCD", "TGO")
  # single year
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = "1983")

  expect_equal(length(unique(out$year)), 1)
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))

  # 2 years
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = c("1983", "2000"))

  expect_equal(length(unique(out$year)), 2)
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))

  # All years
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = "ALL")

  expect_equal(length(unique(out$year)), length(unique(md$year)))
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))
})

test_that("filter_md works when year is passed as a numeric", {
  ctr_alt_agg <- c("BEN", "BFA", "CAF", "CIV", "CMR", "COG", "CPV", "GAB", "GHA", "GIN",
                   "GMB", "GNB", "GNQ", "LBR", "MLI", "MRT", "NER", "NGA", "SEN", "SLE",
                   "TCD", "TGO")
  # single year
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = 1983)

  expect_equal(length(unique(out$year)), 1)
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))

  # 2 years
  out <- filter_md(md = md,
                   ctr_alt_agg = ctr_alt_agg,
                   year = c(1983, 2000))

  expect_equal(length(unique(out$year)), 2)
  expect_true(all(unique(out$country_code) %in% ctr_alt_agg))
})

test_that("get_md_vars works as expected", {
  cl <- aux_files$country_list
  # returns expected object
  md           <- aux_files$missing_data
  ctr_alt_agg  <- cl$country_code[cl$region_code == "SSA"]
  year         <- 1981
  off_alt_agg  <- "both"
  user_off_reg <- "SSA"
  tmp <- get_md_vars(md           = md,
                     ctr_alt_agg  = ctr_alt_agg,
                     year         = year,
                     off_alt_agg  = off_alt_agg,
                     user_off_reg = user_off_reg)
  expect_equal(class(tmp), "list")
  expect_equal(names(tmp), c("md", "md_year", "md_off_reg", "grp_use"))
  expect_equal(class(tmp$md), c("data.table", "data.frame"))
  expect_equal(class(tmp$md_year), "numeric")
  expect_equal(class(tmp$md_off_reg), "character")
  expect_equal(class(tmp$grp_use), "character")

  # returns list with empty elements when nrow(md) == 0
  md0           <- md[0, ]
  ctr_alt_agg  <- cl$country_code[cl$region_code == "SSA"]
  year         <- 1981
  off_alt_agg  <- "both"
  user_off_reg <- "SSA"
  tmp <- get_md_vars(md           = md0,
                     ctr_alt_agg  = ctr_alt_agg,
                     year         = year,
                     off_alt_agg  = off_alt_agg,
                     user_off_reg = user_off_reg)
  expect_equal(class(tmp), "list")
  expect_equal(names(tmp), c("md", "md_year", "md_off_reg", "grp_use"))
  expect_equal(class(tmp$md), c("data.table", "data.frame"))
  expect_equal(nrow(tmp$md), 0)
  expect_equal(tmp$md_year, numeric(0))
  expect_equal(tmp$md_off_reg, character(0))
  expect_equal(tmp$grp_use, character(0))

  # returns correct results when off_alt_agg = "both"
  # IS THIS THE CORRECT CHECK? TO VALIDATE
  md           <- aux_files$missing_data
  ctr_alt_agg  <- cl$country_code[cl$region_code == "SSA"]
  year         <- 1981
  off_alt_agg  <- "both"
  user_off_reg <- "SSA"
  tmp <- get_md_vars(md           = md,
                     ctr_alt_agg  = ctr_alt_agg,
                     year         = year,
                     off_alt_agg  = off_alt_agg,
                     user_off_reg = user_off_reg)
  expect_equal(class(tmp), "list")
  expect_equal(names(tmp), c("md", "md_year", "md_off_reg", "grp_use"))
  expect_equal(class(tmp$md), c("data.table", "data.frame"))
  expect_equal(class(tmp$md_year), "numeric")
  expect_equal(class(tmp$md_off_reg), "character")
  expect_equal(tmp$grp_use, "alone")

  # returns correct results when off_alt_agg = "off"
  # IS THIS THE CORRECT CHECK? TO VALIDATE
  md           <- aux_files$missing_data
  ctr_alt_agg  <- cl$country_code[cl$region_code == "SSA"]
  year         <- 1981
  off_alt_agg  <- "off"
  user_off_reg <- "SSA"
  tmp <- get_md_vars(md           = md,
                     ctr_alt_agg  = ctr_alt_agg,
                     year         = year,
                     off_alt_agg  = off_alt_agg,
                     user_off_reg = user_off_reg)
  expect_equal(class(tmp), "list")
  expect_equal(names(tmp), c("md", "md_year", "md_off_reg", "grp_use"))
  expect_equal(class(tmp$md), c("data.table", "data.frame"))
  expect_equal(tmp$md_year, year) # WHY IS THIS VALUE DIFFERENT FROM WHEN off_alt_agg = "both"?
  expect_equal(tmp$md_off_reg, user_off_reg) # WHY IS THIS VALUE DIFFERENT FROM WHEN off_alt_agg = "both"?
  expect_equal(tmp$grp_use, "not")

  # returns correct results when off_alt_agg = "alt"
  # IS THIS THE CORRECT CHECK? TO VALIDATE
  md           <- aux_files$missing_data
  ctr_alt_agg  <- cl$country_code[cl$region_code == "SSA"]
  year         <- 1981
  off_alt_agg  <- "alt"
  user_off_reg <- "SSA"
  tmp <- get_md_vars(md           = md,
                     ctr_alt_agg  = ctr_alt_agg,
                     year         = year,
                     off_alt_agg  = off_alt_agg,
                     user_off_reg = user_off_reg)
  expect_equal(class(tmp), "list")
  expect_equal(names(tmp), c("md", "md_year", "md_off_reg", "grp_use"))
  expect_equal(class(tmp$md), c("data.table", "data.frame"))
  expect_equal(tmp$md_year, year) # WHY IS THIS VALUE DIFFERENT FROM WHEN off_alt_agg = "both"?
  expect_equal(tmp$md_off_reg, user_off_reg) # WHY IS THIS VALUE DIFFERENT FROM WHEN off_alt_agg = "both"?
  expect_equal(tmp$grp_use, "not")

  # returns correct results when mix of alternative and official with off_alt_agg = "alt"
  # IS THIS THE CORRECT CHECK? TO VALIDATE
  md           <- aux_files$missing_data
  md_region    <- "SSA"
  ctr_alt_agg  <- cl$country_code[cl$region_code == md_region]
  year         <- 1981
  off_alt_agg  <- "alt"
  user_off_reg <- "LAC"
  tmp <- get_md_vars(md           = md,
                     ctr_alt_agg  = ctr_alt_agg,
                     year         = year,
                     off_alt_agg  = off_alt_agg,
                     user_off_reg = user_off_reg)
  expect_equal(class(tmp), "list")
  expect_equal(names(tmp), c("md", "md_year", "md_off_reg", "grp_use"))
  expect_equal(class(tmp$md), c("data.table", "data.frame"))
  expect_equal(tmp$md_year, year) # WHY IS THIS VALUE DIFFERENT FROM WHEN off_alt_agg = "both"?
  expect_equal(tmp$md_off_reg, md_region) # WHY IS THIS FAILING?
  expect_equal(tmp$grp_use, "not")

  # returns correct results when mix of alternative and official with off_alt_agg = "both"
  # IS THIS THE CORRECT CHECK? TO VALIDATE
  md           <- aux_files$missing_data
  md_region    <- "SSA"
  ctr_alt_agg  <- cl$country_code[cl$region_code == md_region]
  year         <- 1981
  off_alt_agg  <- "both"
  user_off_reg <- "LAC"
  tmp <- get_md_vars(md           = md,
                     ctr_alt_agg  = ctr_alt_agg,
                     year         = year,
                     off_alt_agg  = off_alt_agg,
                     user_off_reg = user_off_reg)
  expect_equal(class(tmp), "list")
  expect_equal(names(tmp), c("md", "md_year", "md_off_reg", "grp_use"))
  expect_equal(class(tmp$md), c("data.table", "data.frame"))
  expect_equal(tmp$md_year, year) # WHY IS THIS VALUE DIFFERENT FROM WHEN off_alt_agg = "both"?
  expect_equal(tmp$md_off_reg, md_region) # WHY IS THIS FAILING?
  expect_equal(tmp$grp_use, "append")

  # returns correct results when mix of alternative and official with off_alt_agg = "off"
  # IS THIS THE CORRECT CHECK? TO VALIDATE
  md           <- aux_files$missing_data
  md_region    <- "SSA"
  ctr_alt_agg  <- cl$country_code[cl$region_code == md_region]
  year         <- 1981
  off_alt_agg  <- "both"
  user_off_reg <- "LAC"
  tmp <- get_md_vars(md           = md,
                     ctr_alt_agg  = ctr_alt_agg,
                     year         = year,
                     off_alt_agg  = off_alt_agg,
                     user_off_reg = user_off_reg)
  expect_equal(class(tmp), "list")
  expect_equal(names(tmp), c("md", "md_year", "md_off_reg", "grp_use"))
  expect_equal(class(tmp$md), c("data.table", "data.frame"))
  expect_equal(tmp$md_year, year) # WHY IS THIS VALUE DIFFERENT FROM WHEN off_alt_agg = "both"?
  expect_equal(tmp$md_off_reg, md_region) # WHY IS THIS FAILING?
  expect_equal(tmp$grp_use, "append")
})

test_that("get_grp_to_compute works as expected", {
  # Results are in the expected format
  user_off_reg <- off_reg_ext
  md_off_reg   <- unique(md[["region_code"]])
  year         <- valid_years$valid_survey_years
  md_year      <- unique(md[["year"]])
  tmp <- get_grp_to_compute(user_off_reg,
                            md_off_reg,
                            year,
                            md_year)
  expect_equal(class(tmp), c("data.table", "data.frame"))
  expect_equal(names(tmp), c("region_code", "reporting_year"))

  # Results are correct
  user_off_reg <- "SSA"
  md_off_reg   <- unique(md[["region_code"]])
  year         <- 1981
  md_year      <- unique(md[["year"]])
  tmp <- get_grp_to_compute(user_off_reg,
                            md_off_reg,
                            year,
                            md_year)
  expect_false(all(unique(tmp$reporting_year) %in% year))
  expect_false(all(unique(tmp$region_code) %in% user_off_reg))
})

test_that("get_impl_ctrs works as expected", {
  ctrs <- aux_files$countries
  # returns all survey countries when "world" is part of user_gt
  user_gt      <- user_gts
  user_gt_code <- get_user_x_code(user_gt)
  user_aggs     <- off_reg_ext
  tmp <- get_impl_ctrs(user_gt = user_gt,
                       user_gt_code = user_gt_code,
                       user_aggs = user_aggs,
                       ctrs = ctrs)
  expect_equal(tmp, ctrs$country_code)

  # returns SSA survey countries when user_gt = "africa_split
  user_gt <- "africa_split"
  user_gt_code <- get_user_x_code(user_gt)
  user_aggs <- alt_agg
  tmp <- get_impl_ctrs(user_gt = user_gt,
                       user_gt_code = user_gt_code,
                       user_aggs = user_aggs,
                       ctrs = ctrs)
  expect_equal(tmp, ctrs$country_code[ctrs$region_code == "SSA"])

  # returns all survey countries for a specific region when user_aggs = specific region
  user_gt      <- user_gts
  user_gt_code <- get_user_x_code(user_gt)
  user_aggs     <- "LAC"
  tmp <- get_impl_ctrs(user_gt = user_gt,
                       user_gt_code = user_gt_code,
                       user_aggs = user_aggs,
                       ctrs = ctrs)
  expect_equal(tmp, ctrs$country_code[ctrs$region_code %in% user_aggs])

  # returns all survey countries for multiple regions when user_aggs = specific regions
  user_gt      <- user_gts
  user_gt_code <- get_user_x_code(user_gt)
  user_aggs     <- c("LAC", "MNA")
  tmp <- get_impl_ctrs(user_gt = user_gt,
                       user_gt_code = user_gt_code,
                       user_aggs = user_aggs,
                       ctrs = ctrs)
  expect_equal(tmp, ctrs$country_code[ctrs$region_code %in% user_aggs])

  # returns character(0) when user_gt is empty
  user_gt <- character(0)
  user_gt_code <- get_user_x_code(user_gt)
  user_aggs <- alt_agg
  tmp <- get_impl_ctrs(user_gt = user_gt,
                       user_gt_code = user_gt_code,
                       user_aggs = user_aggs,
                       ctrs = ctrs)
  expect_equal(tmp, character(0))


})

test_that("get_ctr_alt_agg works as expected", {
  cl <- aux_files$country_list
  # returns all countries when "world" is part of user_alt_gt
  user_alt_gt      <- user_gts[!user_gts %in% off_gt]
  user_alt_gt_code <- get_user_x_code(user_alt_gt)
  user_alt_agg     <- alt_agg
  tmp <- get_ctr_alt_agg(user_alt_gt = user_alt_gt,
                         user_alt_gt_code = user_alt_gt_code,
                         user_alt_agg = user_alt_agg,
                         cl = cl)
  expect_equal(tmp, cl$country_code)

  # returns SSA countries when user_alt_gt = "africa_split
  user_alt_gt <- "africa_split"
  user_alt_gt_code <- get_user_x_code(user_alt_gt)
  user_alt_agg <- alt_agg
  tmp <- get_ctr_alt_agg(user_alt_gt = user_alt_gt,
                         user_alt_gt_code = user_alt_gt_code,
                         user_alt_agg = user_alt_agg,
                         cl = cl)
  expect_equal(tmp, cl$country_code[cl$region_code == "SSA"])

  # returns character(0) when user_alt_gt is empty
  user_alt_gt <- character(0)
  user_alt_gt_code <- get_user_x_code(user_alt_gt)
  user_alt_agg <- alt_agg
  tmp <- get_ctr_alt_agg(user_alt_gt = user_alt_gt,
                         user_alt_gt_code = user_alt_gt_code,
                         user_alt_agg = user_alt_agg,
                         cl = cl)
  expect_equal(tmp, character(0))
})

test_that("get_user_x_code works as expected", {
  # results are correct for non-empty strings
  x <- "test_value"
  tmp <- get_user_x_code(x)
  expect_equal(tmp, paste0(x, "_code"))
  # results are correct for non-empty strings of length > 1
  x <- c("test_value1", "test_value2")
  tmp <- get_user_x_code(x)
  expect_equal(tmp, paste0(x, "_code"))
  # results are correct for empty strings
  x <- ""
  tmp <- get_user_x_code(x)
  expect_equal(tmp, character(0))
  # results are correct for empty strings of length > 1
  x <- c("", "")
  tmp <- get_user_x_code(x)
  expect_equal(tmp, character(0))
  # results are correct for character(0)
  x <- character(0)
  tmp <- get_user_x_code(x)
  expect_equal(tmp, character(0))
})

test_that("get_user_alt_gt works as expected", {
  # returns character(0) if user_gt = character(0)
  tmp <- get_user_alt_gt(user_gt = character(0), off_gt = off_gt)
  expect_equal(tmp, character(0))
  # returns alternative grouping types when user_gt is not empty
  tmp <- get_user_alt_gt(user_gt = user_gts, off_gt = off_gt)
  expect_true(all(!tmp %in% off_gt))
})

test_that("select_user_aggs works as expected", {
  # All official regions are returned when user selects "ALL" or "WLD"
  tmp <- select_user_aggs(country = "ALL",
                          off_reg_ext = off_reg_ext,
                          aggs = aggs)
  expect_equal(tmp, off_reg_ext)

  tmp <- select_user_aggs(country = "WLD",
                          off_reg_ext = off_reg_ext,
                          aggs = aggs)
  expect_equal(tmp, off_reg_ext)

  # If user selects specific regions, these regions are being returned
  region = "AFE"
  tmp <- select_user_aggs(country = region,
                          off_reg_ext = off_reg_ext,
                          aggs = aggs)
  expect_equal(tmp, region)

  region = c("AFE", "AFW", "LAC")
  tmp <- select_user_aggs(country = region,
                          off_reg_ext = off_reg_ext,
                          aggs = aggs)
  expect_equal(tmp, region)

  # Empty vector is being returned is user selects a country code
  country = "COL"
  tmp <- select_user_aggs(country = country,
                          off_reg_ext = off_reg_ext,
                          aggs = aggs)
  expect_equal(tmp, character(0))

  country = c("COL", "FRA")
  tmp <- select_user_aggs(country = country,
                          off_reg_ext = off_reg_ext,
                          aggs = aggs)
  expect_equal(tmp, character(0))

  # When mixing countries and regions, countries are being ignored
  country = c("COL", "FRA")
  region  = "LAC"
  tmp <- select_user_aggs(country = c(country, region),
                          off_reg_ext = off_reg_ext,
                          aggs = aggs)
  expect_equal(tmp, region)
})

test_that("select_off_alt_agg works as expected", {
  # Returns correct results ("alt"?) when user_gt = character(0)
  # user_gt will be character(0) when user_aggs = character(0)
  # QUESTION: IS THIS TEST CORRECT? SHOULD "alt" be returned in such cases?
  tmp <- select_off_alt_agg(user_gt = character(0), off_gt = off_gt)
  expect_equal(tmp, "alt")

  # Returns "both" when mixed user_gt values are being passed
  tmp <- select_off_alt_agg(user_gt = user_gts, off_gt = off_gt)
  expect_equal(tmp, "both")

  # Returns "off" when only official user_gt values are being passed
  tmp <- select_off_alt_agg(user_gt = off_gt, off_gt = off_gt)
  expect_equal(tmp, "off")

  # Returns "alt" when only alternative user_gt values are being passed
  tmp <- select_off_alt_agg(user_gt = user_gts[!user_gts %in% off_gt], off_gt = off_gt)
  expect_equal(tmp, "alt")
})

test_that("create_vector_countries output the expected object", {
  skip("TEMPORARY SKIP")
  country <- "ALL"
  year = "2010"
  out <- create_countries_vctr(country = country,
                               year    = year,
                               valid_years = valid_years,
                               aux_files = aux_files)
  expect_true(is.list(out))
  expect_equal(sort(names(out)),
               sort(c("user_off_reg",
                      "user_alt_agg",
                      "est_ctrs",
                      "md_off_reg",
                      "md_year",
                      "grp_use",
                      "md",
                      "user_alt_gt_code"))
  )
})

test_that("create_vector_countries works for countries selection", {
  skip("TEMPORARY SKIP")
  country <- "ALL"
  year = "ALL"
  out <- create_countries_vctr(country = country,
                               year    = year,
                               valid_years = valid_years,
                               aux_files = aux_files)

  # Selects all countries with survey data when country="ALL"
  expect_equal(sort(out$est_ctrs), sort(aux_files$countries$country_code))
})

test_that("create_vector_countries works for regions selection", {
  skip("TEMPORARY SKIP")
  country <- "ALL"
  year = "2010"
  out <- create_countries_vctr(country = country,
                               year    = year,
                               valid_years = valid_years,
                               aux_files = aux_files)

  # Returns official and non-official regions when country="ALL"
  expect_equal(out$user_off_reg,
               aux_files$regions$region_code[aux_files$regions$grouping_type == "region"])
  # List of all aggregates is correct (no duplicates for instance)
  expect_equal(out$user_alt_agg,
               aux_files$regions$region_code[!aux_files$regions$grouping_type %in% c("region", "world")])

  # Correctly selects alternative regions
  country <- c("AFE")
  year = "ALL"
  out <- create_countries_vctr(country = country,
                               year    = year,
                               valid_years = valid_years,
                               aux_files = aux_files)

  # Returns no official region
  expect_true(is.null(out$user_off_reg))
  # Returns selected alternative region
  expect_equal(out$user_alt_agg, country)
  # Selects all SSA countries for estimation: ARE THOSE TESTS CORRECT?
  expect_equal(out$est_ctrs, aux_files$countries$country_code[aux_files$countries$region_code == "SSA"])
  # Selects correct missing data region
  expect_equal(out$md_off_reg, "SSA")
  # Selects years with missing data - NEED TO CHECK THE LOGIC
  # expect_equal(out$md_year, valid_years$valid_survey_years)
})
