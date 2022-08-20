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
    res <-
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
    return(res)
  }

  ## Countries in aggregate --------

  #Find out all the countries that belong to
  #the aggregates requested by the user

  cl        <- lkup$aux_files$country_list

  gt_code   <- paste0(grouping_type, "_code")
  filter_cl <- paste0(gt_code, " %in% country", collapse = " | ")
  filter_cl <- parse(text = filter_cl)

  ctr_agg   <- cl[eval(filter_cl), country_code]

  # Get countries that belong to aggregates requested by the user that are NOT
  # official but alternative aggregates. We need to find out missing data
  # estimates only for those countries. For instance, if the user requested LAC
  # and AFE, we don't care about the the countries with missing data in the LAC
  # because their estimates are done implicitly. We DO care about the estimates
  # of the missing countries in AFE because we need the explicit SSA estimates.


  ctr_alt_agg   <-
    # Get aggregates different to region
    grouping_type[grouping_type != "region"] |>
    # add "_code" suffix
    paste0("_code") |>
    # Create filter for data.table
    paste0(" %in% country", collapse =  " | ") |>
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
      nyear <- year
      md[country_code %in% ctr_alt_agg & year %in% nyear]
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

  #   ______________________________________________________________
  #   computations                                         ####

  ## regional aggregates to be imputed -------
  grp <-
    pip_grp(country       =  rg_country,
          year            =  rg_year,
          povline         =  povline,
          popshare        =  popshare,
          group_by        =  "wb",
          welfare_type    =  welfare_type,
          reporting_level =  reporting_level,
          lkup            =  lkup,
          debug           =  debug,
          censor          =  censor)

  ### Prepare grp to be merge with pop_md
  grp[,
      c("reporting_pop", "pop_in_poverty") := NULL]


  ### Merge population with Missing data table ---------
  pop   <- lkup$aux_files$pop
  pop   <-
    data.table::melt(pop,
                     id.vars         = c('country_code', 'data_level'),
                     variable.name   = "year",
                     value.name      = "reporting_pop",
                     variable.factor = FALSE,
                     value.factor    = FALSE)

  pop <- pop[data_level == "national"
             ][,
               data_level := NULL
               ][,
                 year := as.numeric(year)]

  # Filter pop with missing data countries
  pop_md <-
    pop[md,
      on = c("country_code", "year")]

  data.table::setnames(pop_md, "year", "reporting_year")



  ### Merge with pop_md ------

  pop_md_grp <- joyn::merge(pop_md, grp,
                            by = c("region_code", "reporting_year"),
                            match_type = "m:1",
                            reportvar = FALSE)


  ### Merge other region codes -----------
  pop_md_grp[,
             region_code := NULL]

  pop_md_grp <- joyn::merge(pop_md_grp, cl,
                            by = "country_code",
                            match_type = "m:1",
                            keep = "left",
                            reportvar = FALSE)


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

  l_fg <- vector(mode = "list", length = length(gt_code))

  ### Split fg estimates by grouping type ============

  for (i in seq_along(gt_code)) {
    gt_var    <- gt_code[i]
    filter_fg <-
      paste0(gt_var, " %in% country") |>
      {\(.) parse(text = .) }()

    dt        <- fg[eval(filter_fg)]

    l_fg[[i]] <- dt
  }


  ## Append with countries with missing data -----
  dt <- l_fg[[1]]
  dd <- data.table::rbindlist(list(dt, pop_md_grp),
                              use.names = TRUE,
                              fill = TRUE)



  # debugonce(pip_aggregate)
  de <- pip_aggregate(dd, gt_code[1])








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
