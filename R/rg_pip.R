#' Compute survey year stats
#'
#' Compute the main PIP poverty and inequality statistics for survey years.
#'
#' @inheritParams pip
#' @return data.frame
#' @keywords internal
rg_pip <- function(country,
                   year,
                   povline,
                   popshare,
                   welfare_type,
                   reporting_level,
                   ppp,
                   lkup) {
  # get values from lkup
  valid_regions <- lkup$query_controls$region$values
  svy_lkup      <- lkup$svy_lkup
  data_dir      <- lkup$data_root

  cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")

  metadata <- subset_lkup(
    country         = country,
    year            = year,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    lkup            = svy_lkup,
    valid_regions   = valid_regions,
    data_dir        = data_dir,
    povline         = povline,
    cache_file_path = cache_file_path,
    fill_gaps       = FALSE,
    popshare        = popshare
  )

  data_present_in_master <- metadata$data_present_in_master
  povline  <- metadata$povline
  metadata  <- metadata$lkup


  # Remove aggregate distribution if popshare is specified
  # TEMPORARY FIX UNTIL popshare is supported for aggregate distributions
  metadata <- filter_lkup(metadata = metadata,
                          popshare = popshare)

  # return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    return(list(main_data = pipapi::empty_response,
                data_in_cache = data_present_in_master))
  }

  # load data
  lt <- load_data_list(metadata)

  # parallelization
  # res <- get_pov_estimates(lt, povline = povline)

  # Regular lapply
  res <- lapply(lt, process_dt, povline = povline)

  res <- rbindlist(res, fill = TRUE)


  # clean data
  metadata[, file := basename(path)]

  out <- join(res,
              metadata,
              on = c("file", "reporting_level"),
              how = "full",
              validate = "m:1",
              verbose = 0)

  out[, `:=`(
    mean = survey_mean_ppp,
    median = survey_median_ppp,
    file = NULL
  )]

  setnames(out, "povline", "poverty_line")


  return(list(main_data = out, data_in_cache = data_present_in_master))
}



