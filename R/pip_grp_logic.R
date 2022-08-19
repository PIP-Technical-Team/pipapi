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
  #   Get Data Availability                                       ####

  ## Regions available ----------
  regs   <- lkup$aux_files$regions
  # reg_av <- regs$region_code
  region_code <- country_code <- NULL

  ## Get grouping type ----
  grouping_type <- regs[region_code %in% country,
                        unique(grouping_type)]

  ### Early return -----------
  if (all(grouping_type %in% "region")) {
    pip_grp(country         =  country,
            year            =  year,
            povline         =  povline,
            popshare        =  popshare,
            group_by        =  group_by,
            welfare_type    =  welfare_type,
            reporting_level =  reporting_level,
            lkup            =  lkup,
            debug           =  debug,
            censor          =  censor)
  }

  ## Countries in aggregate --------

  cl        <- lkup$aux_files$country_list

  gt_code   <- paste0(grouping_type, "_code")
  filter_cl <- paste0(gt_code, " %in% country", collapse = " | ")
  filter_cl <- parse(text = filter_cl)

  ctr_agg   <- cl[eval(filter_cl), country_code]

  ## Countries with  missing data ----
  md <- lkup$aux_files$missing_data

  ### Filter by year  -----
  md <-
    if (is.character(year)) {
      md[country_code %in% ctr_agg]
    } else {
      nyear <- year
      md[country_code %in% ctr_agg & year %in% nyear]
    }

  ## Get regional aggregate for countries with missing data ----

  yes_md <- nrow(md) > 0
  if (yes_md) {
    rg_country <- md[, unique(region_code)]
    rg_year    <- md[, unique(year)]

    md_ctrs <- md[, unique(country_code)] # missing data countries
    sv_ctr  <- ctr_agg[which(!ctr_agg %in% md_ctrs)] # survey countries

  } else {
    md_ctrs <- NULL # missing data countries
    sv_ctr  <- ctr_agg  # survey countries
  }

  ## Get Fill gaps call ------

  #   ______________________________________________________________
  #   computations                                         ####

  ## Fill gaps estimates with countries with Survey -----
  fg <- fg_pip(
    country         = sv_ctr,
    year            = year,
    povline         = povline,
    popshare        = popshare,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    lkup            = lkup,
    ppp             = NULL,
    debug           = debug
  )









  #   ____________________________________________________________________
  #   Return                                                         ####
  return(TRUE)

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
    if (!any(year %in% ref_years)) {

      msg     <- c(
        "{.file {as.character(year)}} {?is/are} an invalid value for
        {.field year}",
        "x" = "If {.field year} is numeric, at least one of its elements
        should belong to this list, \n {.file {ref_years}}")
      cli::cli_abort(msg,class = "pipapi_error")
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

  if (!country %in% reg_av) {
    msg     <- c(
      "region {.field {country}} is not available",
      "*" = "It must be one of the following, {.file {reg_av}}")
    cli::cli_abort(msg, class = "pipapi_error")
  }


# Return -------------
  return(invisible(TRUE))

}
