#' #' Create countries vectors
#' #'
#' #' This functions selects the correct countries to be used in the aggregates
#' #' selected by the user, either official or alternative aggregates.
#' #'
#' #'
#' #' @inheritParams pip
#' #' @param valid_years list: Valid years information provided through lkup object
#' #' @param aux_files list: List of auxiliary tables provided through lkup object
#' #'
#' #' @return a list of vectors with countries and regions code to be used in
#' #'   `pip()` and `pip_grp()`
#' create_countries_vctr <- function(country,
#'                                   year,
#'                                   valid_years,
#'                                   aux_files) {
#'
#'   # init Return list ------------
#'   lret <-
#'     list(
#'       user_off_reg      = NULL, # Off regs requested by user
#'       user_alt_agg      = NULL, # Alt aggs requested by user
#'       user_aggs         = NULL, # all aggregates requested by user
#'       user_ctrs         = NULL, # countries requested by user
#'       impl_ctrs         = NULL, # implicit countries requested by user
#'       est_ctrs          = NULL, # All countries needed for estimations
#'       alt_agg           = NULL, # Alt aggregates available
#'       off_reg           = NULL, # Official regions available
#'       all_agg           = NULL, # All aggs available, including WLD and all
#'       off_alt_agg       = NULL, # what to do with alt and reg
#'       user_gt           = NULL, # gt implicitly selected by user
#'       user_alt_gt       = NULL, # Alternative gt implicitly selected by user
#'       ctr_alt_agg       = NULL, # ctrs in alt aggs based on gt by user
#'       ctr_off_reg       = NULL, # Survey countries in official regions
#'       md_ctrs           = NULL, # missing data countries
#'       fg_ctrs           = NULL, # fill gaps countries (with svy)
#'       md_off_reg        = NULL, # off regs to input to MD
#'       md_year           = NULL, # years to input to MD countries
#'       grp_use           = NULL,
#'       md                = NULL, # missing data  and pop table
#'       user_alt_gt_code  = NULL # code of alt grouping data
#'     )
#'
#'   # modify if year is "ALL" --------------
#'
#'   if (any(c("ALL", "MRV") %in% toupper(year))) {
#'     year <- valid_years$valid_survey_years
#'   }
#'
#'   #   ___________________________________________________________________
#'   #   Organize main country and region data                       ####
#'
#'   ## Split between regions and countries ----------
#'
#'   ### Regions available ----------
#'   aggs      <- aux_files$regions  ## all aggregates
#'
#'   region_code <- country_code <-
#'     grouping_type <- NULL
#'
#'   ## Official grouping type
#'   off_gt <-  c("region", "world")
#'
#'   # Official valid region codes
#'   off_reg <- aggs[["region_code"]][aggs[["grouping_type"]] %in% off_gt]
#'
#'   #  Aggregates selected by user
#'   if ("ALL" %in% country) {
#'
#'     user_aggs <- unique(aggs[["region_code"]])
#'
#'   } else {
#'
#'     user_aggs <- unique(aggs[["region_code"]][aggs[["region_code"]] %in% country])
#'   }
#'
#'   ## all and WLD to off_reg
#'   off_reg <- unique(c("ALL", off_reg))
#'
#'   # Alternative  aggregates code
#'   alt_agg <- aggs[["region_code"]][!aggs[["grouping_type"]] %in% off_gt]
#'
#'   # All aggregates available including WLD and all
#'   all_agg <- c(off_reg, alt_agg)
#'
#'   # Official aggregates requested by user
#'   user_off_reg <- off_reg[off_reg %in% user_aggs]
#'
#'
#'   # Alternative aggregates requested by user
#'   user_alt_agg <- alt_agg[alt_agg %in% user_aggs]
#'
#'
#'   ### countries Available -----
#'   ctrs      <- aux_files$countries
#'
#'   # Countries selected by user
#'   user_ctrs <- unique(ctrs[["country_code"]][ctrs[["country_code"]]  %in% country])
#'
#'   ### Get grouping type -------
#'   user_gt <- unique(aggs[["grouping_type"]][aggs[["region_code"]] %in% user_aggs])
#'
#'
#'   if (!is_empty(user_gt) && all(user_gt %in% off_gt)) {
#'     off_alt_agg <- "off"
#'   } else if (any(off_gt %in% user_gt)) {
#'     off_alt_agg <- "both"
#'   } else {
#'     off_alt_agg <- "alt"
#'   }
#'
#'
#'
#'   ### Estimates for official aggregates
#'
#'   # Organize Countries in aggregate --------
#'
#'   #Find out all the countries that belong to
#'   #ALTERNATIVE aggregates requested by the user
#'
#'   # Get countries that belong to aggregates requested by the user that are NOT
#'   # official but alternative aggregates. We need to find out missing data
#'   # estimates only for those countries. For instance, if the user requested LAC
#'   # and AFE, we don't care about the the countries with missing data in the LAC
#'   # because their estimates are done implicitly. We DO care about the estimates
#'   # of the missing countries in AFE because we need the explicit SSA estimates.
#'
#'   cl           <- aux_files$country_list
#'
#'   if (!is_empty(user_gt)) {
#'     user_alt_gt  <- user_gt[!user_gt %in% off_gt]
#'     user_gt_code <- paste0(user_gt, "_code")
#'   } else {
#'     user_alt_gt  <- character()
#'     user_gt_code <- character()
#'   }
#'
#'   ## ALL Countries in alternative aggregates  ----
#'   if (!is_empty(user_alt_gt)) {
#'     user_alt_gt_code <- paste0(user_alt_gt, "_code")
#'
#'     ctr_alt_agg      <- user_alt_gt_code |>
#'       # Create filter for data.table
#'       paste0(" %in% user_alt_agg", collapse =  " | ") |>
#'       # parse the filter as unevaluated expression
#'       {\(.) parse(text = .) }() |>
#'       # filter and get country codes
#'       {\(.) cl[eval(.), country_code] }()
#'
#'   } else {
#'     user_alt_gt_code <- character()
#'     ctr_alt_agg      <- character()
#'   }
#'   # add to return list
#'
#'
#'
#'   ## ctr_off_reg Survey countries in official regions --------
#'   if (!is_empty(user_off_reg)) {
#'     ctr_off_reg <- ctrs[["country_code"]][ctrs[["region_code"]] %in% user_off_reg]
#'   } else {
#'     ctr_off_reg <- character()
#'   }
#'
#'
#'   ## Implicit SURVEY countries on both, official and alternative ----------
#'   if (!is_empty(user_gt)) {
#'     impl_ctrs   <- user_gt_code |>
#'       # Create filter for data.table
#'       paste0(" %in% user_aggs", collapse =  " | ") |>
#'       # parse the filter as unevaluated expression
#'       {\(.) parse(text = .) }() |>
#'       # filter and get country codes
#'       {\(.) ctrs[eval(.), country_code] }()
#'
#'   } else {
#'     impl_ctrs <- character()
#'   }
#'
#'   # add to return list
#'
#'
#'   ## All countries needed for estimations --------------
#'   est_ctrs <- unique(c(user_ctrs, impl_ctrs))
#'
#'
#'   # Early Return ---------
#'   # if (off_alt_agg == "off") {
#'   #
#'   #   return(lret)
#'   # }
#'
#'
#'   ## Countries with  missing data ----
#'   md <- filter_md(md = aux_files$missing_data,
#'                   ctr_alt_agg = ctr_alt_agg,
#'                   year = year)
#'
#'   # Get countries for which we want to input
#'   yes_md <- nrow(md) > 0
#'   if (yes_md) {
#'     md_off_reg <- unique(md[["region_code"]])
#'     md_year    <- unique(md[["year"]])
#'
#'     if (off_alt_agg == "both") {
#'       # filter md_off_reg and md_year based on what have already been
#'       # estimated
#'
#'       grp_computed <-
#'         expand.grid(region_code      = user_off_reg,
#'                     reporting_year   = as.numeric(year), # Hot fix
#'                     stringsAsFactors = FALSE) |>
#'         data.table::as.data.table()
#'
#'       grp_to_compute <-
#'         expand.grid(region_code      = md_off_reg,
#'                     reporting_year   = md_year,
#'                     stringsAsFactors = FALSE)  |>
#'         data.table::as.data.table()
#'
#'       grp_to_compute <-
#'         grp_to_compute[!grp_computed,
#'                        on = c("region_code", "reporting_year")]
#'
#'       # filter region code and year to calculate
#'       md_off_reg <- unique(grp_to_compute[["region_code"]])
#'       md_year    <- unique(grp_to_compute[["year"]])
#'
#'       if (length(md_off_reg) > 0) {
#'         # If length of `md_off_reg` is still positive, we need to append the
#'         # results previously calculated
#'         grp_use <- "append"
#'       } else {
#'         # If length of `md_off_reg` is zero, we don't need to do any additional
#'         # calculation, so the previously done calculation is used alone
#'         grp_use <- "alone"
#'       }
#'
#'     } else {
#'       # If there is no previous calculation, there is nothing to use from
#'       # before.
#'       grp_use <- "not"
#'     }
#'
#'     md_ctrs <- unique(md[["country_code"]]) # missing data countries
#'     # fg_ctrs <- ctr_alt_agg[!ctr_alt_agg %in% md_ctrs] # survey countries
#'
#'   } else { # if yes_md == FALSE
#'     md_ctrs    <- character() # missing data countries
#'     # fg_ctrs    <- ctr_alt_agg  # survey countries
#'     md_off_reg <- character()
#'     md_year    <- numeric()
#'     grp_use    <- character()
#'     md         <- character()
#'   }
#'
#'   fg_ctrs <- est_ctrs # survey countries
#'
#' #   _____________________________________________________________________
#' #   Return                                                           ####
#'
#'   # Add to return list
#'   fillin_list(lret)
#'
#'   return(lret)
#'
#' }
#'
#' #' Helper function to filter missing data table
#' #'
#' #' @param md data.frame: Table of countries with missing data
#' #' @param ctr_alt_agg character: Countries from alternate aggregates
#' #' @param year character: year
#' #'
#' #' @return data.table
#' filter_md <- function(md, ctr_alt_agg, year) {
#'   # Filter countries
#'   md <-  md[md[["country_code"]] %in% ctr_alt_agg, ]
#'     numeric_years <- suppressWarnings(as.numeric(year))
#'     numeric_years <- numeric_years[!is.na(numeric_years)]
#'
#'   # Filter years
#'     if (length(numeric_years) > 0) {
#'       md <- md[md[["year"]] %in% numeric_years, ]
#'     }
#'     return(md)
#' }
#'
#'
