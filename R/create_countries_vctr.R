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
      alt_agg           = NULL, # Alt aggregates available
      off_reg           = NULL, # Official regions available
      off_alt_agg       = NULL, # what to do with alt and reg
      user_gt           = NULL, # gt implicitly selected by user
      user_alt_gt       = NULL, # Alternative gt implicitly selected by user
      ctr_alt_agg       = NULL, # ctrs in alt aggs based on gt by user
      md_ctrs           = NULL, # missing data countries
      fg_ctrs           = NULL, # fill gaps countries (with svy)
      md_off_reg        = NULL, # off regs to input to MD
      md_year           = NULL, # years to input to MD countries
      grp_use           = NULL,
      missing_data      = NULL, # MD and pop table
      gt_code           = NULL, # code of alt grouping data
      country_list      = NULL  #
    )

  #   ___________________________________________________________________
  #   Get Data Availability                                       ####
  ## Split between regions and countries ----------

  ### Regions available ----------
  aggs      <- lkup$aux_files$regions  ## all aggregates

  region_code <- country_code <-
    grouping_type <- NULL

  user_aggs <- aggs[region_code %in% country,
                    unique(region_code)]

  # Official valid region codes
  off_reg <- aggs[grouping_type == "region",
                  region_code]

  alt_agg <- aggs[grouping_type != "region",
                  region_code]

  off_reg <- c("all", off_reg, "WLD")



  # Official aggregates requested by user
  user_off_reg <- off_reg[off_reg %in% user_aggs]

  # Alternative aggregates requested by user
  user_alt_agg <- alt_agg[alt_agg %in% user_aggs]

  ### countries Available -----
  ctrs      <- lkup$aux_files$countries

  # Countries selected by user
  user_ctrs <- ctrs[country_code  %in% country,
                    unique(country_code)]

  ## Get grouping type -------
  grouping_type <- aggs[region_code %in% user_aggs,
                        unique(grouping_type)]

  if (all(grouping_type %in% "region")) {
    off_alt_agg <- "off"
  } else if ("region" %in% grouping_type) {
    off_alt_agg <- "both"
  } else {
    off_alt_agg <- "alt"
  }


  ## Estimates for official aggregates

  if (off_alt_agg == "both") {
    # Non-official grouping_type
    gt  <- grouping_type[grouping_type != "region"]

  } else {
    gt  <- grouping_type

  }

  # add return list
  ret_list(lret, user_off_reg)
  ret_list(lret, user_alt_agg)
  ret_list(lret, user_aggs)
  ret_list(lret, user_ctrs)
  ret_list(lret, alt_agg)
  ret_list(lret, off_reg)
  ret_list(lret, off_alt_agg)
  ret_list(lret, gt, "user_alt_gt")


  # Early Return ---------
  if (off_alt_agg == "off") {


    return(lret)
  }

  ## Countries in aggregate --------


  #Find out all the countries that belong to
  #ALTERNATIVE aggregates requested by the user

  # Get countries that belong to aggregates requested by the user that are NOT
  # official but alternative aggregates. We need to find out missing data
  # estimates only for those countries. For instance, if the user requested LAC
  # and AFE, we don't care about the the countries with missing data in the LAC
  # because their estimates are done implicitly. We DO care about the estimates
  # of the missing countries in AFE because we need the explicit SSA estimates.

  cl        <- lkup$aux_files$country_list
  gt_code   <- paste0(gt, "_code")

  ctr_alt_agg   <- gt_code |>
    # Create filter for data.table
    paste0(" %in% user_alt_agg", collapse =  " | ") |>
    # parse the filter as unevaluated expression
    {\(.) parse(text = .) }() |>
    # filter and get country codes
    {\(.) cl[eval(.), country_code] }()

  # add to return list
  ret_list(lret, ctr_alt_agg)

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
  ret_list(lret, md, "missing_data")



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
    md_ctrs    <- NULL # missing data countries
    fg_ctrs     <- ctr_alt_agg  # survey countries
    md_off_reg <- NULL
    md_year    <- NULL
  }

#   _____________________________________________________________________
#   Return                                                           ####

  # Add to return list
  ret_list(lret, md_ctrs)
  ret_list(lret, fg_ctrs)
  ret_list(lret, md_off_reg)
  ret_list(lret, md_year)
  ret_list(lret, grp_use)
  ret_list(lret, gt_code)
  ret_list(lret, cl, "country_list")

  return(lret)

}


#' Store return values
#'
#' @param lret list with return named objects predifined
#' @param x object to be added to lret
#' @param x_name character: name of object to be added to `lret`. It must
#' @param assign logical: whether to assign to parent frame. Default is TRUE
#'
#' @return invisible `lret` list
#' @examples
#' lf <- list(x = NULL,
#' y = 8,
#' w = "foo")
#'
#' z <- "ocho"
#' x <- 2
#' w <- "nueve"
#' (ret_list(lf, x))
#' (ret_list(lf, z, "y"))
#' (ret_list(lf, w, assign = FALSE))
#' lf
ret_list <- function(lret,
                     x ,
                     x_name = deparse(substitute(x)),
                     assign = TRUE) {

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {
    x_name %in% names(lret)
    is.list(lret)
    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (length(x) == 0 & !is.null(x)) {
    return(invisible(lret))
  }


#   ____________________________________________________________________
#   Computations                   ####

  # get name of original object
  nm <- deparse(substitute(lret))


  lret[[x_name]] <-  x
  if (assign == TRUE) {
    assign(nm, lret, envir = parent.frame())
  }


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(invisible(lret))

}

