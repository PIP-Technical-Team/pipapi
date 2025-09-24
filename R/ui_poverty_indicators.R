#' Poverty Calculator Main chart
#'
#' Provides numbers that will populate the poverty calculator main chart.
#'
#' @inheritParams pip
#' @inheritParams ui_hp_countries
#' @return data.table
#' @export
ui_pc_charts <- function(country = c("AGO"),
                         year = "all",
                         povline = 1.9,
                         fill_gaps = FALSE,
                         group_by = "none",
                         welfare_type = c("all", "consumption", "income"),
                         reporting_level = c("all", "national", "rural", "urban"),
                         pop_units = 1e6,
                         censor  = TRUE,
                         lkup) {
  # Set returned columns
  return_cols           <- lkup$return_cols$ui_pc_charts$cols
  inequality_indicators <- lkup$return_cols$ui_pc_charts$inequality_indicators

  group_by         <- match.arg(group_by)
  welfare_type     <- match.arg(welfare_type)
  reporting_level  <- match.arg(reporting_level)

  out <- pip(
    country         = country,
    year            = year,
    povline         = povline,
    fill_gaps       = fill_gaps,
    group_by        = group_by,
    reporting_level = reporting_level,
    lkup            = lkup,
    censor          = censor
  )

  # Add pop_in_poverty and scale according to pop_units
  out[,
      `:=`(
        pop_in_poverty = reporting_pop * headcount / pop_units,
        reporting_pop  = reporting_pop / pop_units
      )]


  # handle different responses when fill_gaps = TRUE / FALSE
  # Return all columns when survey years are requested
  if (fill_gaps == FALSE) {
    # NOTE: this should be modified in the lkups or somewhere else...
    return_cols <- return_cols[return_cols != "estimate_type"]
    out <- out[, .SD, .SDcols = return_cols]

  } else {
    out <- get_vars(out, return_cols)
    # Set non-interpolated variables to NA if line-up years are requested
    out[, (inequality_indicators) := NA]
    out[, survey_comparability := NA]   # remove manually survey_comparability
    # remove nowcast from UI
    out <- out[estimate_type != "nowcast"]
  }
  return(out)
}

#' Poverty Calculator regional aggregates
#'
#' Provides numbers that will populate poverty calculator regional aggregates
#' for all years.
#'
#' @inheritParams ui_pc_charts
#' @return data.table
#' @export
ui_pc_regional <- function(country   = "ALL",
                           year      = "ALL",
                           povline   = 1.9,
                           pop_units = 1e6,
                           lkup) {

  # TEMPORARY UNTIL SELECTION MECHANISM IS BEING IMPROVED
  country <- toupper(country)
  if (is.character(year)) {
    year <- toupper(year)
  }

  out <- pip_agg(country         = country,
                 year            = year,
                 group_by        = "wb",
                 reporting_level = "national",
                 povline         = povline,
                 lkup            = lkup,
                 censor          = TRUE)

  # Add pop_in_poverty and scale according to pop_units
  out[,
      `:=`(
        pop_in_poverty = reporting_pop * headcount / pop_units,
        reporting_pop  = reporting_pop / pop_units
      )]

  # TEMP START: remove old aggregations --------------
  cl <- lkup$aux_files$country_list

  regs <- cl[, .(region_code, africa_split_code)] |>
    unlist()  |>  # convert to vector
    na_omit() |>
    unique()  |>
    unname()  |>
    c("WLD")  # add the world
  # TEMP END: remove old aggregations --------------

  out <- out[estimate_type == "actual" & region_code %in% regs]

  return(out)
}
