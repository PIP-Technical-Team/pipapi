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


#' Frontend Version IDs
#'
#' Extracts the latest data release date and its available PPP years from raw
#' version identifiers. The result is suitable for direct JSON serialization
#' by frontend endpoints.
#'
#' @param versions Character vector of raw version identifiers in the form
#'   `yyyymmdd_uuuu` followed by optional metadata.
#'
#' @return A named list whose keys are `ppp_uuuu` and values are normalized
#'   `yyyymmdd_uuuu` version identifiers.
#' @export
ui_version_id <- function(versions) {
  if (!is.character(versions) || length(versions) == 0L || anyNA(versions)) {
    stop("`versions` must be a non-empty character vector without missing values.", call. = FALSE)
  }

  matches <- regmatches(
    versions,
    regexec("^([0-9]{8})_([0-9]{4})(?:_|$)", versions, perl = TRUE)
  )
  if (any(lengths(matches) != 3L)) {
    stop(
      "Each version must begin with an 8-digit release date and 4-digit PPP year.",
      call. = FALSE
    )
  }

  release_dates <- vapply(matches, `[[`, character(1), 2L)
  ppp_years <- vapply(matches, `[[`, character(1), 3L)
  parsed_dates <- as.Date(release_dates, format = "%Y%m%d")
  if (anyNA(parsed_dates) || any(format(parsed_dates, "%Y%m%d") != release_dates)) {
    stop("Version release dates must be valid dates in yyyymmdd format.", call. = FALSE)
  }

  latest_release <- max(release_dates)
  ppp_years <- sort(unique(ppp_years[release_dates == latest_release]))
  out <- as.list(paste(latest_release, ppp_years, sep = "_"))
  names(out) <- paste0("ppp_", ppp_years)
  out
}


