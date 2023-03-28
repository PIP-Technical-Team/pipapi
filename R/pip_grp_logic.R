#' Logic for computing new aggregate
#'
#' @inheritParams pip
#' @return data.table
#' @examples
#' \dontrun{
#' # Create lkups
#' }
#' @export
pip_grp_logic <- function(country         = "ALL",
                           year            = "ALL",
                           povline         = 1.9,
                           group_by        = c("none", "wb"),
                           welfare_type    = c("all", "consumption", "income"),
                           reporting_level = c("all", "national"),
                           lkup,
                           censor          = TRUE,
                           lkup_hash       = lkup$cache_data_id$hash_pip_grp) {
  #   ________________________________________________________________________
  #   STEP 1: Set up                                                      ####

  welfare_type    <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  group_by        <- match.arg(group_by)

  # Custom aggregations only supported at the national level
  # subgroups aggregations only supported for "all" countries
  if (group_by != "none") {
    reporting_level <- "all"
    if (!all(country %in% c("ALL", lkup$query_controls$region$values))) {
      country <- "ALL"
    }
  } else {
    reporting_level <- "national"
  }

  #   ___________________________________________________________________
  # STEP 3: filter countries and years                             ####

  lcv <- # List with countries vectors
    create_countries_vctr(
      country         =  country,
      year            =  year,
      valid_years     =  lkup$valid_years,
      aux_files       =  lkup$aux_files
    )

  # use the same names as before to avoid inconsistencies
  alt_agg <- lcv$user_alt_agg
  gt_code <- lcv$user_alt_gt_code
  cl      <- lkup$aux_files$country_list

  # STEP 4: Start pip_grp_logic algorithm ----
  ## STEP 4.1: Official regions only selection ----
  ## This will trigger an early return as no additional imputations are needed
  if (all(lcv$off_alt_agg == "off")) { # Users only request the official regions
    ### Early return -----------
    res <-
      pip_grp(country         =  country,
              year            =  year,
              povline         =  povline,
              group_by        =  "wb",
              welfare_type    =  welfare_type,
              reporting_level =  reporting_level,
              lkup            =  lkup,
              censor          =  censor)
    return(res)

  } else {

    ## STEP 4.2: Compute fg_pip for ALL required countries ----
    ## This will then be re-used in various part of the function
    ## This is to avoid re-computing and re-loading the same data over and over
    fg_pip_master <- fg_pip(
      country         = c(lcv$md_off_reg, lcv$user_off_reg),
      year            = year,
      povline         = povline,
      popshare        = NULL,
      welfare_type    = welfare_type,
      reporting_level = reporting_level,
      ppp             = NULL,
      ref_lkup           = lkup[["ref_lkup"]],
      valid_regions      = lkup$query_controls$region$values,
      interpolation_list = lkup$interpolation_list
    )

    if (lcv$off_alt_agg == "both") {
      ### STEP 4.2.1 Estimates for official aggregates ----
      off_ret <-
        # pip_grp(country         =  lcv$user_off_reg,
        #         year            =  year,
        #         povline         =  povline,
        #         group_by        =  "wb",
        #         welfare_type    =  welfare_type,
        #         reporting_level =  reporting_level,
        #         lkup            =  lkup,
        #         censor          =  censor)
        pip_grp_helper(lcv_country     = lcv$ctr_off_reg,
                       country         = country,
                       year            = year,
                       povline         = povline,
                       reporting_level = reporting_level,
                       censor          = censor,
                       fg_pip          = fg_pip_master)
    } else {
      ### STEP 4.2.2 Alternate aggregates only ----
      ### Prepare necessary variables
      off_ret <- NULL
      alt_agg <- country
    }
  }

  #   ________________________________________________________
  #   computations                                      ####

  ## STEP 4.3 Compute needed regional aggregates -------
  ##  Alternate aggregates use the stats from corresponding official region
  ##  to impute values for missing countries. As a result, even if an official
  ##  region is not being requested by the user, we may need to compute its stats
  ##  to compute the alternate regions stats.
  ##  If we want to append previous calculations or there is no previous
  ## calculation of off regions but we still have to input to missing data
  ## countries, we estimate official region estimates for such countries

  if (lcv$grp_use %in% c("append", "not")) {

    grp <-
      # pip_grp(country         =  lcv$md_off_reg,
      #         year            =  lcv$md_year,
      #         povline         =  povline,
      #         group_by        =  "wb",
      #         welfare_type    =  welfare_type,
      #         reporting_level =  reporting_level,
      #         lkup            =  lkup,
      #         censor          =  censor)
      pip_grp_helper(lcv_country         = lcv$md_off_reg,
                     country             = country,
                     year                = lcv$md_year,
                     povline             = povline,
                     reporting_level     = reporting_level,
                     censor              = censor,
                     fg_pip              = fg_pip_master)

    if (lcv$grp_use == "append") {
      grp <- data.table::rbindlist(list(off_ret, grp))
    }

  } else {
    # If previous estimations are enough, we don't need to do any estimation.
    grp <- data.table::copy(off_ret)
  }

  names_grp <- names(grp)

  ### Prepare grp to be merge with pop_md
  grp[,
      c("reporting_pop", "pop_in_poverty") := NULL]


  ### Merge population with Missing data table ---------

  ### Merge with pop_md ------
  pop_md <- lcv$md
  data.table::setnames(pop_md, "year", "reporting_year")

  # This merge will remove those countries for which there is no official
  # aggregate because of lack of coverage in the region. Eg. There is not data
  # for SAS in 2000, so for countries like AFG 2000 we can't input estimates
  md_grp <- merge(pop_md, grp,
                  by = c("region_code", "reporting_year"))

  ### Merge other region codes -----------
  md_grp[,
         region_code := NULL]

  md_grp <- merge(md_grp, cl,
                  by = "country_code",
                  all.x = TRUE)


  ## Fill gaps estimates with countries with Survey  -----
  # fg <- fg_pip(
  #   country         = lcv$fg_ctrs,
  #   year            = year,
  #   povline         = povline,
  #   popshare        = NULL,
  #   welfare_type    = welfare_type,
  #   reporting_level = reporting_level,
  #   ppp             = NULL,
  #   ref_lkup           = lkup[["ref_lkup"]],
  #   valid_regions      = lkup$query_controls$region$values,
  #   interpolation_list = lkup$interpolation_list
  # )

  fg <- fg_pip_master[fg_pip_master$country_code %chin% lcv$fg_ctrs, ]

  if (!"ALL" %in% year) {
    fg <- fg[fg[["reporting_year"]] %in% as.numeric(year), ]
  }


  l_fg <- vector(mode = "list", length = length(gt_code))

  ### Split fg estimates by grouping type ============
  for (i in seq_along(gt_code)) {
    gt_var    <- gt_code[i]
    filter_fg <-
      paste0(gt_var, " %in% alt_agg") |>
      {\(.) parse(text = .) }()

    # Filter both datasets
    fdt        <- fg[eval(filter_fg)]
    mdt        <- md_grp[eval(filter_fg)]

    # Find common variables
    common_vars <- intersect(names(fdt), names(mdt))
    fdt         <- fdt[, ..common_vars]
    mdt         <- mdt[, ..common_vars]

    ## Append with countries with missing data -----
    l_fg[[i]] <- data.table::rbindlist(list(fdt, mdt),
                                       use.names = TRUE,
                                       fill = TRUE)
  }

  # Estimate poverty for aggregates
  stopifnot({
    length(l_fg) == length(gt_code)
  })

  ld <- vector(mode = "list", length = length(l_fg))
  for (i in seq_along(l_fg)) {
    x <- l_fg[[i]]
    y <- gt_code[i]

    ld[[i]] <- pip_aggregate(x, y)
  }
  de <- data.table::rbindlist(ld, use.names = TRUE)
  rm(ld)

  vars_to_keep <- names(off_ret)
  # de[, ]

  # Append official regions with Alt aggregates ---------

  if (!is.null(off_ret)) {
    ret <- data.table::rbindlist(list(de, off_ret),
                                 use.names = TRUE,
                                 fill = TRUE)

  } else {
    ret <- de
  }
  data.table::setcolorder(ret, names_grp)


  # Censor regional values -----------

  if (censor) {
    ret <- censor_rows(ret, lkup[["censored"]], type = "regions")
  }

  #   ____________________________________________________________________
  #   Return                                                         ####
  return(ret)

}


pip_grp_helper <- function(lcv_country,
                           country,
                           year,
                           povline,
                           reporting_level,
                           censor,
                           fg_pip,
                           group_by = "wb"){

  # Filter countries
  keep_countries <- fg_pip[["country_code"]] %chin% lcv_country |
    fg_pip[["wb_region_code"]] %chin% lcv_country
  out <- fg_pip[keep_countries, ]
  # Filter years
  if (!"ALL" %in% year) {
    out <- out[out[["reporting_year"]] %in% as.numeric(year), ]
  }

  # return empty dataframe if no metadata is found
  if (nrow(out) == 0) {
    return(pipapi::empty_response_grp)
  }

  # Handles aggregated distributions
  if (reporting_level %in% c("national", "all")) {
    out <- add_agg_stats(out)
  }

  # Handle potential (insignificant) difference in poverty_line values that
  # may mess-up the grouping
  out$poverty_line <- povline

  # Handle aggregations with sub-groups
  if (group_by != "none") {

    out <- pip_aggregate_by(
      df = out,
      group_lkup = lkup[["pop_region"]],
      country = country
    )

    # Censor regional values
    if (censor) {
      out <- censor_rows(out, lkup[["censored"]], type = "regions")
    }

  } else {
    # Handle simple aggregation
    out <- pip_aggregate(out)
  }

  out <- out[, c("region_name",
                 "region_code",
                 "reporting_year",
                 "reporting_pop",
                 "poverty_line",
                 "headcount",
                 "poverty_gap",
                 "poverty_severity",
                 "watts",
                 "mean",
                 "pop_in_poverty")]

  return(out)
}
