#' Compute PIP statistics
#'
#' Compute the main PIP poverty and inequality statistics.
#'
#' @param country character: Country ISO 3 codes
#' @param year integer: Reporting year
#' @param welfare_type character: Welfare type
#' @param reporting_level character: Geographical reporting level
#' @param lkup list: A list of lkup tables
#'
#' @return data.table
#' @export
get_files <- function(country = "all",
                year = "all",
                welfare_type = c("all", "consumption", "income"),
                reporting_level = c("all", "national", "rural", "urban"),
                lkup) {

  logger::log_info('root: {lkup$data_root}')

  welfare_type <- match.arg(welfare_type)
  reporting_level <- match.arg(reporting_level)

  metadata <- subset_lkup(
    country = country,
    year = year,
    welfare_type = welfare_type,
    reporting_level = reporting_level,
    lkup = lkup[["svy_lkup"]]
  )

  # return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(pipapi::empty_response)
  }

  out <- vector(mode = "integer", length = nrow(metadata))

  tictoc::tic("read_all")
  for (i in seq_along(out)) {
    tmp_metadata <- metadata[i, ]

    svy_data <- get_svy_data(
      tmp_metadata$cache_id,
      reporting_level = tmp_metadata$pop_data_level,
      path = tmp_metadata$path
    )

    svy_data <- nrow(svy_data[[1]])
    out[i] <- svy_data
  }
  # Logging
  end_read_all <- tictoc::toc(quiet = TRUE)
  logger::log_info('read_all: {round(end_read_all$toc - end_read_all$tic, digits = getOption("digits", 6))}')

  return(out)
}
