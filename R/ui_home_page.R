#' Home Page Main Chart
#'
#' Provides numbers that will populate the home page main chart.
#'
#' @param povline numeric: Poverty line
#' @param lkup list: A list of lkup tables
#' @param lkup_hash character: hash of pip
#'
#' @return data.table
#' @export
ui_hp_stacked <- function(povline = 1.9,
                          lkup,
                          lkup_hash = lkup$cache_data_id$hash_pip_grp) {

  ref_years <- sort(unique(lkup$ref_lkup$reporting_year))
  ref_years <- ref_years[!ref_years %in% c(1981:1989)]


  out <- pip_agg(country  = "ALL",
          year            = "ALL",
          povline         = povline,
          welfare_type    = "all",
          reporting_level = "all",
          lkup            = lkup,
          censor          = FALSE,
          lkup_hash       = lkup$cache_data_id$hash_pip_grp,
          additional_ind  = FALSE)

  regs <- lkup$aux_files$country_list[, region_code] |>
    funique() |>
    c("WLD")

  out <- out[region_code %in% regs & reporting_year %in% ref_years]

  out <- get_vars(out,
                  c("region_code", "reporting_year",
                    "poverty_line", "pop_in_poverty"))


  return(out)
}

#' Home Page Country Charts
#'
#' Provides numbers that will populate the home page country charts.
#'
#' @inheritParams pip
#' @param pop_units numeric: Units used to express population numbers (default
#'   to million)
#' @return data.table
#' @export
ui_hp_countries <- function(country = c("IDN", "CIV"),
                            povline = 1.9,
                            pop_units = 1e6,
                            lkup,
                            lkup_hash       = lkup$cache_data_id$hash_pip
                            ) {
  out <- pip(
    country = country,
    year = "ALL",
    povline = povline,
    lkup = lkup,
    fill_gaps = FALSE,
    reporting_level = "national"
  )

  # Add pop_in_poverty and scale according to pop_units
  out[,
      `:=`(
        pop_in_poverty = reporting_pop * headcount / pop_units,
        reporting_pop  = reporting_pop / pop_units
        )]

  out <- get_vars(out, c(
    "region_code", "country_code", "reporting_year",
    "poverty_line", "reporting_pop", "pop_in_poverty"
  ))

  return(out)
}
