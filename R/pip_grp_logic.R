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
  #   Set up                                                      ####

  welfare_type    <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  group_by        <- match.arg(group_by)


  #   ______________________________________________________________
  #   Defenses                                                ####
  # check_inputs_pip_grp_logic(
  #   country         =  country,
  #   year            =  year,
  #   povline         =  povline,
  #   popshare        =  popshare,
  #   group_by        =  group_by,
  #   welfare_type    =  welfare_type,
  #   reporting_level =  reporting_level,
  #   lkup            =  lkup,
  #   censor          =  censor
  # )

  #   ___________________________________________________________________
  #   filter countries and years                                 ####

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

  if (all(lcv$off_alt_agg == "off")) {
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

  } else if (lcv$off_alt_agg == "both") {
  ## Estimates for official aggregates
    off_ret <-
      pip_grp(country         =  lcv$user_off_reg,
              year            =  year,
              povline         =  povline,
              group_by        =  "wb",
              welfare_type    =  welfare_type,
              reporting_level =  reporting_level,
              lkup            =  lkup,
              censor          =  censor)
  } else {
    off_ret <- NULL
    alt_agg <- country
  }

  #   ________________________________________________________
  #   computations                                      ####

  ## regional aggregates to be imputed -------

  # If we want to append previous calculations or there is no previous
  # calculation of off regions but we still have to input to missing data
  # countries, we estimate official region estimates for such countries

  if (lcv$grp_use %in% c("append", "not")) {

    grp <-
      pip_grp(country         =  lcv$md_off_reg,
              year            =  lcv$md_year,
              povline         =  povline,
              group_by        =  "wb",
              welfare_type    =  welfare_type,
              reporting_level =  reporting_level,
              lkup            =  lkup,
              censor          =  censor)

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
  fg <- fg_pip(
    country         = lcv$fg_ctrs,
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


