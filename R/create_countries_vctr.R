#' Create countries vectors
#'
#' This functions selects the correct countries to be used in the aggregates
#' selected by the user, either official or alternative aggregates.
#'
#'
#' @inheritParams pip
#' @param valid_years list: Valid years information provided through lkup object
#' @param aux_files list: List of auxiliary tables provided through lkup object
#'
#' @return a list of vectors with countries and regions code to be used in
#'   `pip()` and `pip_grp()`
create_countries_vctr <- function(country,
                                  year,
                                  valid_years,
                                  aux_files) {

  # STEP 1: Setup ----
  ## init Return list ----
  lret <-
    list(
      user_off_reg      = NULL, # Off regs requested by user
      user_alt_agg      = NULL, # Alt aggs requested by user
      est_ctrs          = NULL, # All countries needed for estimations
      md_off_reg        = NULL, # off regs needed to input to missing data
      md_year           = NULL, # years to input to missing data countries
      grp_use           = NULL, # Instruction for the next function call
      md                = NULL, # missing data  and pop table
      user_alt_gt_code  = NULL#,  # code of alt grouping data
      # user_aggs         = NULL, # all aggregates requested by user
      # user_ctrs         = NULL, # countries requested by user
      # impl_ctrs         = NULL, # implicit countries requested by user
      # alt_agg           = NULL, # Alt aggregates available
      # off_reg           = NULL, # Official regions available
      # all_agg           = NULL, # All aggs available, including WLD and ALL
      # off_alt_agg       = NULL, # what to do with alt and reg
      # user_gt           = NULL, # gt implicitly selected by user
      # user_alt_gt       = NULL, # Alternative gt implicitly selected by user
      # ctr_alt_agg       = NULL, # ctrs in alt aggs based on gt by user
      # ctr_off_reg       = NULL, # Survey countries in official regions
      # md_ctrs           = NULL  # missing data countries
    )

  ## Adjust year ----
  if (any(c("ALL", "MRV") %in% toupper(year))) {
    year <- valid_years$valid_survey_years
  }
  # STEP 2: Identify regions ----
  ## All available aggregates ----
  aggs      <- aux_files$regions  ## all aggregates
  ## Official grouping type ----
  off_gt <-  c("region") # c("region", "world")
  ## Official valid region codes ----
  off_reg <- aggs[["region_code"]][aggs[["grouping_type"]] %in% off_gt]
  ## Extended list of regions ----
  ext_reg <- c("ALL", "WLD")
  off_reg_ext <- sort(unique(c(off_reg, ext_reg)))
  ## Alternative  aggregates code ----
  alt_agg <- aggs[["region_code"]][!aggs[["grouping_type"]] %in% off_gt]
  ## All aggregates available including WLD and all ----
  all_agg <- c(off_reg_ext, alt_agg)
  ##  Aggregates selected by user ----
  user_aggs <- select_user_aggs(country = country,
                                off_reg = off_reg,
                                aggs = aggs)
  ## Official aggregates requested by user ----
  user_off_reg <- off_reg_ext[off_reg_ext %in% user_aggs]
  ## Alternative aggregates requested by user ----
  user_alt_agg <- alt_agg[alt_agg %in% user_aggs]
  ## Get grouping type ----
  user_gt <- unique(aggs[["grouping_type"]][aggs[["region_code"]] %in% user_aggs])
  ## Select off_alt_agg ----
  off_alt_agg <- select_off_alt_agg(user_gt = user_gt, off_gt = off_gt)

  # STEP 3: Identify countries ----
  ## Available countries -----
  ctrs      <- aux_files$countries
  ## Countries selected by user ----
  user_ctrs <- unique(ctrs[["country_code"]][ctrs[["country_code"]]  %in% country])
  ## Countries need to compute alternative aggregates ----
  user_alt_gt      <- get_user_alt_gt(user_gt)
  user_gt_code     <- get_user_x_code(user_gt)
  user_alt_gt_code <- get_user_x_code(user_alt_gt)
  ctr_alt_agg      <- get_ctr_alt_agg(user_alt_gt      = user_alt_gt,
                                      user_alt_gt_code = user_alt_gt_code,
                                      user_alt_agg = user_alt_agg,
                                      cl               = aux_files$country_list )
  ## Implicit SURVEY countries in both, official and alternative ----
  impl_ctrs <- get_impl_ctrs(user_gt = user_gt,
                             user_gt_code = user_gt_code,
                             user_aggs = user_aggs,
                             ctrs = ctrs)
  ## All countries needed for estimations -----
  est_ctrs <- unique(c(user_ctrs, impl_ctrs))

  # STEP 4: Create variable to help compute stats for countries with missing data ----
  ## Countries with  missing data ----
  md_vars <- get_md_vars(md           = md,
                         ctr_alt_agg  = ctr_alt_agg,
                         year         = year,
                         off_alt_agg  = off_alt_agg,
                         user_off_reg = user_off_reg)
  list2env(md_vars, envir = environment())
  # Add to return list
  fillin_list(lret)

  return(lret)

}

#' Helper function to filter missing data table
#'
#' @param md data.frame: Table of countries with missing data
#' @param ctr_alt_agg character: Countries from alternate aggregates
#' @param year character: year
#'
#' @return data.table
filter_md <- function(md, ctr_alt_agg, year) {
  # Filter countries
  md <-  md[md[["country_code"]] %in% ctr_alt_agg, ]
  numeric_years <- suppressWarnings(as.numeric(year))
  numeric_years <- numeric_years[!is.na(numeric_years)]

  # Filter years
  if (length(numeric_years) > 0) {
    md <- md[md[["year"]] %in% numeric_years, ]
  }
  return(md)
}

#' Helper function to identify how Official and Alternative regions should be
#' handled
#'
#' @param user_gt character: Grouping type implicitly selected by user
#' @param off_gt character: Grouping type associated with Offical Regions
#'
#' @return character
#' @export
#'
select_off_alt_agg <- function(user_gt, off_gt) {
  if (!is_empty(user_gt) && all(user_gt %in% off_gt)) {
    out <- "off"
  } else if (any(off_gt %in% user_gt)) {
    out <- "both"
  } else {
    out <- "alt"
  }
  return(out)
}

#' Helper function to select correct Official Regions
#'
#' @param country character: User selected countries
#' @param off_reg character: Official region codes
#' @param aggs data.frame: Regions lookup table
#'
#' @return character
#' @export
#'
select_user_aggs <- function(country, off_reg, aggs) {
  if (any(c("ALL", "WLD") %in% country)) {
    # Select all official regions
    out <- off_reg

  } else {
    # Select only official regions that correspond to selected countries
    out <- unique(aggs[["region_code"]][aggs[["region_code"]] %in% country])
  }
  return(out)
}

#' Helper function to define user_alt_gt
#'
#' @param user_gt character: Grouping type needed by user
#'
#' @return character
#' @export
#'
get_user_alt_gt <- function(user_gt) {
  if (!is_empty(user_gt)) {
    out  <- user_gt[!user_gt %in% off_gt]
  } else {
    out  <- character()
  }
  return(out)
}

#' Helper function to define user_{var}_code
#'
#' @param user_gt character: Grouping type needed by user
#'
#' @return character
#' @export
#'
get_user_x_code <- function(x) {
  if (!is_empty(x)) {
    out <- paste0(x, "_code")
  } else {
    out <- character()
  }
  return(out)
}

#' Helper function to retrieve the required countries
#' needed to compute alternative aggregates requested by user
#' Get countries that belong to aggregates requested by the user that are NOT
#' official but alternative aggregates. We need to find out missing data
#' estimates only for those countries. For instance, if the user requested LAC
#' and AFE, we don't care about the the countries with missing data in the LAC
#' because their estimates are done implicitly. We DO care about the estimates
#' of the missing countries in AFE because we need the explicit SSA estimates.
#'
#' @param user_alt_gt character: Grouping type needed by user
#' @param user_alt_gt_code character: Grouping type code
#' @param user_alt_agg character: Alternate aggregates requested by user
#' @param cl data.frame: Countries lookup table
#'
#' @return character
#' @export
#'
get_ctr_alt_agg <- function(user_alt_gt,
                            user_alt_gt_code,
                            user_alt_agg,
                            cl) {
  if (!is_empty(user_alt_gt)) {

    out      <- user_alt_gt_code |>
      # Create filter for data.table
      paste0(" %in% user_alt_agg", collapse =  " | ") |>
      # parse the filter as unevaluated expression
      {\(.) parse(text = .) }() |>
      # filter and get country codes
      {\(.) cl[eval(.), country_code] }()
  } else {
    out      <- character()
  }
  return(out)
}

#' Helper function to retrieve the implicit country surveys present in both
#' alternative and official aggregates
#'
#' @param user_gt character: Grouping type
#' @param user_gt_code character: Grouping type code
#' @param user_aggs character: Aggregates selected by user
#' @param ctrs data.frame: Countries lookup table
#'
#' @return character
#' @export
#'
get_impl_ctrs <- function(user_gt,
                          user_gt_code,
                          user_aggs,
                          ctrs) {
  if (!is_empty(user_gt)) {
    out   <- user_gt_code |>
      # Create filter for data.table
      paste0(" %in% user_aggs", collapse =  " | ") |>
      # parse the filter as unevaluated expression
      {\(.) parse(text = .) }() |>
      # filter and get country codes
      {\(.) ctrs[eval(.), country_code] }()

  } else {
    out <- character()
  }
  return(out)
}

#' Helper function to retrieve the country/year pairs to be computed
#'
#' @param user_off_reg character: Official regions requested by user
#' @param md_off_reg character: Missing data for official regions
#' @param year character: Years
#' @param md_year character: Years with missing data
#'
#' @return data.frame
#' @export
#'
get_grp_to_compute <- function(user_off_reg,
                               md_off_reg,
                               year,
                               md_year) {
  grp_computed <-
    expand.grid(region_code      = user_off_reg,
                reporting_year   = as.numeric(year), # Hot fix
                stringsAsFactors = FALSE) |>
    data.table::as.data.table()

  grp_to_compute <-
    expand.grid(region_code      = md_off_reg,
                reporting_year   = md_year,
                stringsAsFactors = FALSE)  |>
    data.table::as.data.table()

  grp_to_compute <-
    grp_to_compute[!grp_computed,
                   on = c("region_code", "reporting_year")]

  return(grp_to_compute)
}


#' Helper function to retrieve variables needed to handle imputation of
#' missing data
#'
#' @param md data.frame: Table of country/year with missing data
#' @param ctr_alt_agg character: Countries in alternative aggregate
#' @param year character: Years
#' @param off_alt_agg character: Instruction about how to handle official and
#' alternate aggregates
#' @param user_off_reg: character: Official regions requested by user
#' @return list
#' @export
#'
get_md_vars <- function(md,
                        ctr_alt_agg,
                        year,
                        off_alt_agg,
                        user_off_reg) {
  md <- filter_md(md = md,
                  ctr_alt_agg = ctr_alt_agg,
                  year = year)
  ## Get countries for which we want to impute
  yes_md <- nrow(md) > 0
  if (yes_md) {
    md_off_reg <- unique(md[["region_code"]])
    md_year    <- unique(md[["year"]])

    if (off_alt_agg == "both") {
      # filter md_off_reg and md_year based on what have already been
      # estimated

      grp_to_compute <- get_grp_to_compute(user_off_reg = user_off_reg,
                                           md_off_reg   = md_off_reg,
                                           year         = year,
                                           md_year      = md_year)

      # filter region code and year to calculate
      md_off_reg <- unique(grp_to_compute[["region_code"]])
      md_year    <- unique(grp_to_compute[["year"]])

      if (length(md_off_reg) > 0) {
        # If length of `md_off_reg` is still positive, we need to append the
        # results previously calculated
        grp_use <- "append"
      } else {
        # If length of `md_off_reg` is zero, we don't need to do any additional
        # calculation, so the previously done calculation is used alone
        grp_use <- "alone"
      }

    } else {
      # If there is no previous calculation, there is nothing to use from
      # before.
      grp_use <- "not"
    }
    md_ctrs <- unique(md[["country_code"]]) # missing data countries

  } else { # if yes_md == FALSE
    md_ctrs    <- character() # missing data countries
    md_off_reg <- character()
    md_year    <- numeric()
    grp_use    <- character()
    md         <- character()
  }

  return(list(md         = md,
              md_year    = md_year,
              md_off_reg = md_off_reg,
              grp_use    = grp_use)
  )
}


