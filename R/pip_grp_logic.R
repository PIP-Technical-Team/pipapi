#' Logic for computing new aggregate
#'
#' @inheritParams pip
#' @return data.table
#' @examples
#' \dontrun{
#' # Create lkups
#' }
#' @export
pip_grp_logic <- function(country         = "all",
                          year            = "all",
                          povline         = 1.9,
                          popshare        = NULL,
                          group_by        = c("none", "wb"),
                          welfare_type    = c("all", "consumption", "income"),
                          reporting_level = c("all", "national"),
                          lkup,
                          debug           = FALSE,
                          censor          = TRUE) {
  #   ________________________________________________________________________
  #   Set up                                                      ####

  welfare_type    <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)
  group_by        <- match.arg(group_by)


  #   ______________________________________________________________
  #   Defenses                                                ####
  check_inputs_pip_grp_logic(
    country         =  country,
    year            =  year,
    povline         =  povline,
    popshare        =  popshare,
    group_by        =  group_by,
    welfare_type    =  welfare_type,
    reporting_level =  reporting_level,
    lkup            =  lkup,
    debug           =  debug,
    censor          =  censor
  )

  #   ___________________________________________________________________
  #   filter countries and years                                 ####

  lcv <- # List with countries vectors
  create_countries_vctr(
    country         =  country,
    year            =  year,
    lkup            =  lkup
  )


  if (all(lcv$off_alt_agg == "off")) {
  ### Early return -----------
    res <-
    pip_grp(country         =  country,
            year            =  year,
            povline         =  povline,
            popshare        =  popshare,
            group_by        =  "wb",
            welfare_type    =  welfare_type,
            reporting_level =  reporting_level,
            lkup            =  lkup,
            debug           =  debug,
            censor          =  censor)
    return(res)

  } else if (lcv$off_alt_agg == "both") {
  ## Estimates for official aggregates
    off_ret <-
      pip_grp(country         =  lcv$off_regs_user,
              year            =  year,
              povline         =  povline,
              popshare        =  popshare,
              group_by        =  "wb",
              welfare_type    =  welfare_type,
              reporting_level =  reporting_level,
              lkup            =  lkup,
              debug           =  debug,
              censor          =  censor)
  } else {
    gt      <- grouping_type
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
      pip_grp(country         =  lcv$off_regs_to_input,
              year            =  lcv$years_to_input,
              povline         =  povline,
              popshare        =  popshare,
              group_by        =  "wb",
              welfare_type    =  welfare_type,
              reporting_level =  reporting_level,
              lkup            =  lkup,
              debug           =  debug,
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
  pop_md <- lcv$missing_data
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
    popshare        = popshare,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    lkup            = lkup,
    ppp             = NULL,
    debug           = debug
  )

  gt_code <- lcv$gt_code
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

  #   ____________________________________________________________________
  #   Return                                                         ####
  return(ret)

}


#' Check parameters of pip_grp_logic
#'
#' @inheritParams pip_grp_logic
#' @return `invsible(TRUE)`
check_inputs_pip_grp_logic <- function(country,
                                       year,
                                       povline,
                                       popshare,
                                       group_by,
                                       welfare_type,
                                       reporting_level,
                                       lkup,
                                       debug,
                                       censor) {

# Year ---------
  if (is.character(year)) {
    if (!year %in% c("all", "WLD")) {
      msg     <- c(
        "{.file {year}} is an invalid value for {.field year}",
        "x" = "If {.field year} is character, it must be either
        {.file all} or {.file WLD}")
      cli::cli_abort(msg,class = "pipapi_error")
    }
  } else if (is.numeric(year)) {
    ref_years <- lkup$valid_years$valid_interpolated_years

    no_year <- year[!year %in%  ref_years]
    any_year <- any(year %in%  ref_years)

    if (!any_year) {
      msg     <- c(
        "{.file {as.character(year)}} {?is/are} an invalid value for
        {.field year}",
        "x" = "If {.field year} is numeric, at least one of its elements
        should belong to this list, \n {.file {ref_years}}")
      cli::cli_abort(msg,class = "pipapi_error")
    } else if (length(no_year) > 0) {
      msg     <- c(
        "{.file {as.character(no_year)}} {?is/are} outside the valid
        range for {.field year}",
        "x" = "If {.field year} is numeric, at least one of its elements
        should belong to this list, \n {.file {ref_years}}")
      cli::cli_warn(msg)
    }
  } else {
    msg     <- c(
      "{.field year} must be either character or numeric",
      "*" = "If {.strong character}, it must be either {.file all} or
      {.file WLD}",
      "*" = "If {.strong numeric}, at least one of its elements
        should belong to this list, \n {.file {ref_years}}"
    )
    cli::cli_abort(msg,class = "pipapi_error")

  }

# Country and regions availability ------------
  regs   <- lkup$aux_files$regions
  reg_av <- regs$region_code

  not_av <- country[!country %in% reg_av]

  if (length(not_av) > 0) {
    msg     <- c(
      "region{?s} {.file {not_av}} in paraneter {.field country}
      {?is/are} not available",
      "*" = "It must be one of the following, {.file {reg_av}}")
    cli::cli_abort(msg, class = "pipapi_error")
  }


# Return -------------
  return(invisible(TRUE))

}
