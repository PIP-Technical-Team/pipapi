# utils-query.R
#
# Functions related to API query parameter validation and control objects.
#
# Functions:
#   create_query_controls()          - build the list of valid values for all API params
#   get_valid_aux_long_format_tables()- return tables supporting long_format=TRUE
#   .check_group_by()                - validate and normalise a group_by value


#' Create query controls
#'
#' Builds a named list of valid values (and types) for every API query
#' parameter. Used by the plumber middleware to validate incoming requests.
#'
#' @param svy_lkup data.table: Survey lookup table
#' @param ref_lkup data.table: Reference lookup table
#' @param aux_files list: Auxiliary data files (regions, country_list, etc.)
#' @param aux_tables character: Names of available auxiliary tables
#' @param versions character: Available data version strings
#'
#' @return Named list of parameter control objects, each with \code{$values}
#'   and \code{$type}.
#' @noRd
create_query_controls <- function(
  svy_lkup,
  ref_lkup,
  aux_files,
  aux_tables,
  versions
) {
  # Countries and regions
  countries <- unique(c(
    svy_lkup$country_code,
    ref_lkup$country_code
  ))

  regions <- unique(c(
    aux_files$regions$region_code
  ))

  country <- list(
    values = c(
      "ALL",
      sort(c(
        countries,
        regions
      ))
    ),
    type = "character"
  )

  region <- list(
    values = sort(c("ALL", regions)),
    type = "character"
  )
  # Year
  year <- list(
    values = c(
      "all",
      "MRV",
      sort(unique(c(
        svy_lkup$reporting_year,
        ref_lkup$reporting_year
      )))
    ),
    type = "character"
  )
  # Poverty line
  povline <- list(
    values = c(min = 0, max = 2700),
    type = "numeric"
  )
  # Popshare
  popshare <- list(
    values = c(min = 0, max = 1),
    type = "numeric"
  )

  # Boolean parameters
  fill_gaps <-
    aggregate <-
      long_format <-
        additional_ind <-
          exclude <-
            list(values = c(TRUE, FALSE), type = "logical")

  # Welfare type
  welfare_type <- list(
    values = c(
      "all",
      sort(unique(c(
        svy_lkup$welfare_type,
        ref_lkup$welfare_type
      )))
    ),
    type = "character"
  )
  # Reporting level
  reporting_level <- list(
    values = c(
      "all",
      sort(unique(c(
        svy_lkup$reporting_level,
        ref_lkup$reporting_level
      )))
    ),
    type = "character"
  )
  # PPPs
  ppp <- list(
    values = c(min = 0.05, max = 1000000),
    type = "numeric"
  )
  # Versions
  version <- list(
    values = versions,
    type = "character"
  )
  # Formats
  format <- list(values = c("json", "csv", "rds", "arrow"), type = "character")
  # Tables
  table <- list(values = aux_tables, type = "character")

  # type
  type <- list(values = c("both", "rg", "fg"), type = "character")

  pass <- list(values = Sys.getenv('PIP_CACHE_SERVER_KEY'), type = "character")
  # parameters
  parameter <-
    list(
      values = c(
        "country",
        "year",
        "povline",
        "popshare",
        "fill_gaps",
        "aggregate",
        "group_by",
        "welfare_type",
        "reporting_level",
        "ppp",
        "version",
        "format",
        "table",
        "long_format",
        "exclude",
        "type",
        "pass"
      ),
      type = "character"
    )

  # cum_welfare
  cum_welfare <- list(
    values = c(min = 0, max = 1),
    type = "numeric"
  )
  # cum_population
  cum_population <- list(
    values = c(min = 0, max = 1),
    type = "numeric"
  )
  # requested_mean
  requested_mean <- list(
    values = c(min = 0, max = 1e10),
    type = "numeric"
  )

  # mean
  mean <- list(
    values = c(min = 0, max = 1e10),
    type = "numeric"
  )

  # times_mean
  times_mean <- list(
    values = c(min = 0.01, max = 5),
    type = "numeric"
  )

  # lorenz
  lorenz <- list(values = c("lb", "lq"), type = "character")

  # n_bins
  n_bins <- list(
    values = c(min = 0, max = 1000),
    type = "numeric"
  )

  # Endpoint
  endpoint <-
    list(
      values = c("all", "aux", "pip", "pip-grp", "pip-info", "valid-params"),
      type = "character"
    )

  # group_by
  regs <- aux_files$country_list |>
    names() |>
    grep("_code$|_name$", x = _, value = TRUE, invert = TRUE) |>
    c("wb", "none", "vintage", "pcn") |>
    sort()

  group_by <- list(
    values = regs,
    type = "character"
  )

  # Create list of query controls
  query_controls <- list(
    country = country,
    region = region,
    year = year,
    povline = povline,
    popshare = popshare,
    fill_gaps = fill_gaps,
    aggregate = aggregate,
    long_format = long_format,
    exclude = exclude,
    additional_ind = additional_ind,
    group_by = group_by,
    welfare_type = welfare_type,
    reporting_level = reporting_level,
    ppp = ppp,
    version = version,
    format = format,
    table = table,
    parameter = parameter,
    cum_welfare = cum_welfare,
    cum_population = cum_population,
    requested_mean = requested_mean,
    mean = mean,
    times_mean = times_mean,
    lorenz = lorenz,
    n_bins = n_bins,
    endpoint = endpoint,
    type = type,
    pass = pass
  )

  return(query_controls)
}


#' Return auxiliary tables that support long_format=TRUE
#'
#' @return character vector of valid table names
#' @export
get_valid_aux_long_format_tables <- function() {
  c('cpi', 'ppp', 'gdp', 'pce', 'pop')
}


#' Validate and normalise a group_by argument
#'
#' Checks that the supplied \code{group_by} value is a single string that
#' exists in \code{lkup$query_controls$group_by$values}. Returns a canonical
#' form suitable for downstream code.
#'
#' @inheritParams pip
#'
#' @return A single character string (validated grouping key).
#'
#' @keywords internal
.check_group_by <- \(group_by, lkup) {
  # Defenses and early return -----------
  if (length(group_by) > 1) {
    cli::cli_abort("The `group_by` parameter can only take a single value.")
  }
  # vintage
  if (group_by %in% c("vintage", "pcn")) {
    return("regionpcn")
  }

  # special grouping
  if (group_by %in% c("none", "wb")) {
    return("wb")
  }

  # get regions -----------
  regs <- lkup$query_controls$group_by$values

  if (!tolower(group_by) %in% tolower(regs)) {
    cli::cli_abort(
      "The `group_by` parameter can only take the following values: {.field {regs}}."
    )
  }

  tolower(group_by)
}
