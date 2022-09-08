#' Create countries vectors
#'
#' This functions selects the correct countries to be used in the aggregates
#' selected by the user, either official or alternative aggregates.
#'
#'
#' @inheritParams pip
#'
#' @return a list of vectors with countries and regions code to be used in
#'   `pip()` and `pip_grp()`
create_countries_vctr <- function(country,
                                  year,
                                  lkup) {

  # init Return list ------------
  lret <-
    list(
      user_off_reg      = NULL, # Off regs requested by user
      user_alt_agg      = NULL, # Alt aggs requested by user
      user_aggs         = NULL, # all aggregates requested by user
      user_ctrs         = NULL, # countries requested by user
      impl_ctrs         = NULL, # implicit countries requested by user
      est_ctrs          = NULL, # All countries needed for estimations
      alt_agg           = NULL, # Alt aggregates available
      off_reg           = NULL, # Official regions available
      all_agg           = NULL, # All aggs available, including WLD and all
      off_alt_agg       = NULL, # what to do with alt and reg
      user_gt           = NULL, # gt implicitly selected by user
      user_alt_gt       = NULL, # Alternative gt implicitly selected by user
      ctr_alt_agg       = NULL, # ctrs in alt aggs based on gt by user
      ctr_off_reg       = NULL, # Survey countries in official regions
      md_ctrs           = NULL, # missing data countries
      fg_ctrs           = NULL, # fill gaps countries (with svy)
      md_off_reg        = NULL, # off regs to input to MD
      md_year           = NULL, # years to input to MD countries
      grp_use           = NULL,
      md                = NULL, # missing data  and pop table
      user_alt_gt_code  = NULL # code of alt grouping data
    )

  # modify if year is "all" --------------

  if ("all" %in% year) {
    year <- lkup$valid_years$valid_survey_years
  }

  #   ___________________________________________________________________
  #   Organize main country and region data                       ####

  ## Split between regions and countries ----------

  ### Regions available ----------
  aggs      <- lkup$aux_files$regions  ## all aggregates

  region_code <- country_code <-
    grouping_type <- NULL

  # Official valid region codes
  off_reg <- aggs[grouping_type == "region",
                  region_code]

  #  Aggregates selected by user
  if (any(c("all", "WLD") %in% country)) {
    user_aggs <- off_reg
  } else {
    user_aggs <- aggs[region_code %in% country,
                      unique(region_code)]
  }

  ## all and WLD to off_reg
  off_reg <- c("all", off_reg, "WLD")

  # Alternative  aggregates code
  alt_agg <- aggs[grouping_type != "region",
                  region_code]

  # All aggregates available including WLD and all
  all_agg <- c(off_reg, alt_agg)

  # Official aggregates requested by user
  user_off_reg <- off_reg[off_reg %in% user_aggs]


  # Alternative aggregates requested by user
  user_alt_agg <- alt_agg[alt_agg %in% user_aggs]


  ### countries Available -----
  ctrs      <- lkup$aux_files$countries

  # Countries selected by user
  user_ctrs <- ctrs[country_code  %in% country,
                    unique(country_code)]

  ### Get grouping type -------
  user_gt <- aggs[region_code %in% user_aggs,
                        unique(grouping_type)]


  if (!is_empty(user_gt) && all(user_gt %in% "region")) {
    off_alt_agg <- "off"
  } else if ("region" %in% user_gt) {
    off_alt_agg <- "both"
  } else {
    off_alt_agg <- "alt"
  }



  ### Estimates for official aggregates

  # Organize Countries in aggregate --------

  #Find out all the countries that belong to
  #ALTERNATIVE aggregates requested by the user

  # Get countries that belong to aggregates requested by the user that are NOT
  # official but alternative aggregates. We need to find out missing data
  # estimates only for those countries. For instance, if the user requested LAC
  # and AFE, we don't care about the the countries with missing data in the LAC
  # because their estimates are done implicitly. We DO care about the estimates
  # of the missing countries in AFE because we need the explicit SSA estimates.

  cl           <- lkup$aux_files$country_list

  if (!is_empty(user_gt)) {
    user_alt_gt  <- user_gt[user_gt != "region"]
    user_gt_code <- paste0(user_gt, "_code")
  } else {
    user_alt_gt  <- character()
    user_gt_code <- character()
  }

  ## ALL Countries in alternative aggregates  ----
  if (!is_empty(user_alt_gt)) {
    user_alt_gt_code <- paste0(user_alt_gt, "_code")

    ctr_alt_agg      <- user_alt_gt_code |>
      # Create filter for data.table
      paste0(" %in% user_alt_agg", collapse =  " | ") |>
      # parse the filter as unevaluated expression
      {\(.) parse(text = .) }() |>
      # filter and get country codes
      {\(.) cl[eval(.), country_code] }()

  } else {
    user_alt_gt_code <- character()
    ctr_alt_agg      <- character()
  }
  # add to return list



  ## ctr_off_reg Survey countries in official regions --------
  if (!is_empty(user_off_reg)) {
    ctr_off_reg <- ctrs[region_code %in% user_off_reg,
                        country_code]
  } else {
    ctr_off_reg <- character()
  }


  ## Implicit SURVEY countries on both, official and alternative ----------
  if (!is_empty(user_gt)) {
    impl_ctrs   <- user_gt_code |>
      # Create filter for data.table
      paste0(" %in% user_aggs", collapse =  " | ") |>
      # parse the filter as unevaluated expression
      {\(.) parse(text = .) }() |>
      # filter and get country codes
      {\(.) ctrs[eval(.), country_code] }()

  } else {
    impl_ctrs <- character()
  }

  # add to return list


  ## All countries needed for estimations --------------
  est_ctrs <- unique(c(user_ctrs, impl_ctrs))


  # Early Return ---------
  # if (off_alt_agg == "off") {
  #
  #   return(lret)
  # }


  ## Countries with  missing data ----

  md <- lkup$aux_files$missing_data

  ### Filter by year  -----
  md <-
    if (is.character(year)) {
      md[country_code %in% ctr_alt_agg]
    } else {
      nyear <- year # to avoid conflicts in variable names
      md[country_code %in% ctr_alt_agg & year %in% nyear]
    }

  # add to return list




  # Get countries for which we want to input
  yes_md <- nrow(md) > 0
  if (yes_md) {
    md_off_reg <- md[, unique(region_code)]
    md_year    <- md[, unique(year)]

    if (off_alt_agg == "both") {
      # filter md_off_reg and md_year based on what have already been
      # estimated

      grp_computed <-
        expand.grid(region_code      = user_off_reg,
                    reporting_year   = year,
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

      # filter region code and year to calculate
      md_off_reg <- grp_to_compute[, unique(region_code)]
      md_year    <- grp_to_compute[, unique(year)]

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

    md_ctrs <- md[, unique(country_code)] # missing data countries
    fg_ctrs <- ctr_alt_agg[!ctr_alt_agg %in% md_ctrs] # survey countries

  } else { # if yes_md == FALSE
    md_ctrs    <- character() # missing data countries
    fg_ctrs    <- ctr_alt_agg  # survey countries
    md_off_reg <- character()
    md_year    <- numeric()
    grp_use    <- character()
    md         <- character()
  }

#   _____________________________________________________________________
#   Return                                                           ####

  # Add to return list
  fillin_list(lret)

  return(lret)

}

