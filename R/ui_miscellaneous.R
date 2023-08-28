#' Data Sources Survey Metadata
#'
#' Provides survey metadata that will populate the Data Sources page.
#'
#' @inheritParams pip
#' @return data.table
#' @export
ui_svy_meta <- function(country = "all", lkup) {
  out <- readRDS(sprintf("%s/_aux/survey_metadata.rds", lkup$data_root))
  if (country == "all") {
    return(out)
  } else {
    out <- out[out$country_code == country, ]
    return(out)
  }
}



