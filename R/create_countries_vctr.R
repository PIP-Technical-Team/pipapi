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
#   ____________________________________________________________________________
#   Computations                                                            ####
  #   ___________________________________________________________________
  #   Get Data Availability                                       ####

  ## Regions available ----------
  aggs    <- lkup$aux_files$regions  ## all aggregates

  # Official valid region codes
  off_reg <- lkup$aux_files$regions[grouping_type == "region",
                                    region_code]
  off_reg <- c("all", off_reg, "WLD")

  # reg_av <- aggs$region_code
  region_code <- country_code <- NULL

  ## Get grouping type ----
  grouping_type <- aggs[region_code %in% country,
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
    gt <- grouping_type[grouping_type != "region"]

    # official regions selected by the user
    off_regs_user <- off_reg[off_reg %in% country]

    alt_agg <- country[!country %in% off_reg]


  } else {
    gt            <- grouping_type
    off_regs_user <- NULL
    alt_agg       <- country
  }

  ## Countries in aggregate --------

  cl        <- lkup$aux_files$country_list

  #Find out all the countries that belong to
  #ALTERNATIVE aggregates requested by the user

  # Get countries that belong to aggregates requested by the user that are NOT
  # official but alternative aggregates. We need to find out missing data
  # estimates only for those countries. For instance, if the user requested LAC
  # and AFE, we don't care about the the countries with missing data in the LAC
  # because their estimates are done implicitly. We DO care about the estimates
  # of the missing countries in AFE because we need the explicit SSA estimates.

  gt_code   <- paste0(gt, "_code")

  ctr_alt_agg   <- gt_code |>
    # Create filter for data.table
    paste0(" %in% alt_agg", collapse =  " | ") |>
    # parse the filter as unevaluated expression
    {\(.) parse(text = .) }() |>
    # filter and get country codes
    {\(.) cl[eval(.), country_code] }()



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


  # Get countries for which we want to input
  yes_md <- nrow(md) > 0
  if (yes_md) {
    rg_country <- md[, unique(region_code)]
    rg_year    <- md[, unique(year)]

    if (!is.null(off_ret)) {
      # filter rg_country and rg_year based on what have already been
      # estimated

      grp_computed <-
        unique(off_ret[, .(region_code, reporting_year)])

      grp_to_compute <-
        expand.grid(region_code      = rg_country,
                    reporting_year   = rg_year,
                    stringsAsFactors = FALSE)  |>
        data.table::as.data.table()

      grp_to_compute <-
        grp_to_compute[!grp_computed,
                       on = c("region_code", "reporting_year")]

      # filter region code and year to calculate
      rg_country <- grp_to_compute[, unique(region_code)]
      rg_year    <- grp_to_compute[, unique(year)]

      if (length(rg_country) > 0) {
        # If length of `rg_country` is still positive, we need to append the
        # results previously calculated
        grp_use <- "append"
      } else {
        # If length of `rg_country` is zero, we don't need to do any additional
        # calculation, so the previously done calculation is used alone
        grp_use <- "alone"
      }

    } else {
      # If there is no previous calculation, there is nothing to use from
      # before.
      grp_use <- "not"
    }

    md_ctrs <- md[, unique(country_code)] # missing data countries
    sv_ctr  <- ctr_alt_agg[which(!ctr_alt_agg %in% md_ctrs)] # survey countries

  } else {
    md_ctrs <- NULL # missing data countries
    sv_ctr  <- ctr_alt_agg  # survey countries
  }




#   ____________________________________________________________________________
#   Return                                                                  ####
  return(TRUE)

}
