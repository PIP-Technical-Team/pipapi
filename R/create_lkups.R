#' Create one list of lookups per data version
#'
#' @param data_dir character: Path to the main data directory
#'
#' @return list
#' @export
#'
create_versioned_lkups <- function(data_dir) {

  data_dirs <- extract_data_dirs(data_dir = data_dir)

  versions <- names(data_dirs)
  # versions[1] <- "latest_release"

  versions_paths <- purrr::map(data_dirs, create_lkups, versions = versions)
  names(versions_paths) <- versions

  return(list(versions = versions,
              versions_paths = versions_paths,
              latest_release = versions[1]))

}

#' Extract list of data sub-directories from main data directory
#'
#' @param data_dir character: Path to main data directory
#' @param version_length integer: Number of character of version ID
#'
#' @return character
#' @export
#'
extract_data_dirs <- function(data_dir, version_length = 8) {
  # List data directories under data_dir
  data_dirs <- list.dirs(data_dir, full.names = TRUE, recursive = FALSE)
  data_dirs <- data_dirs[grepl("^[0-9]{8}", sub(paste0(data_dir, "/"), "", data_dirs))]

  versions <- extract_versions(data_dirs = data_dirs,
                               version_length = version_length)

  names(data_dirs) <- versions

  data_dirs <- data_dirs[sort(names(data_dirs), decreasing = TRUE)]

  return(data_dirs)
}

#' Extract data versions from vector of data directory paths
#'
#' @param data_dirs character: List of data directory paths
#' @param version_length integer: Number of character of version ID
#'
#' @return character
#' @export
#'
extract_versions <- function(data_dirs,
                             version_length) {

  data_dirs_length <- nchar(data_dirs)

  versions <- purrr::map2_chr(data_dirs, data_dirs_length,
                              ~ substr(x = .x,
                                       start = .y - version_length + 1,
                                       stop = .y))

  return(versions)

}



#' Create look-up tables
#'
#' Create look-up tables that can be passed to [pip()].
#'
#' @param data_dir character: Path to PIP data root folder.
#' @param versions character: Available data versions
#' @return list
#' @export
create_lkups <- function(data_dir, versions) {

  # Get survey paths
  paths <- list.files(paste0(data_dir, "/survey_data"))
  paths_ids <- tools::file_path_sans_ext(paths)

  # TEMP FIX to add country and region name
  countries <-  fst::read_fst(sprintf("%s/_aux/countries.fst", data_dir),
                              as.data.table = TRUE)
  regions <-  fst::read_fst(sprintf("%s/_aux/regions.fst", data_dir),
                              as.data.table = TRUE)
  regions %>% data.table::setnames('region', 'region_name')
  # TEMP fix - END (see further code chunks below )

  # Clean svy_lkup
  svy_lkup <- fst::read_fst(sprintf("%s/estimations/prod_svy_estimation.fst", data_dir),
    as.data.table = TRUE
  )
  # TEMP cleaning - START
  svy_lkup <- svy_lkup[svy_lkup$cache_id %in% paths_ids, ]
  # TEMP cleaning - END
  svy_lkup$path <- sprintf(
    "%s/survey_data/%s.fst",
    data_dir, svy_lkup$cache_id
  )
  # TEMP fix to add country and region name
  svy_lkup <- merge(svy_lkup, countries[, c('country_code', 'country_name')],
                    by = 'country_code', all.x = TRUE)
  svy_lkup <- merge(svy_lkup, regions[, c('region_code', 'region_name')],
                    by = 'region_code', all.x = TRUE)
  # TEMP fix - END
  # Clean ref_lkup
  ref_lkup <- fst::read_fst(sprintf("%s/estimations/prod_ref_estimation.fst", data_dir),
    as.data.table = TRUE
  )
  # TEMP cleaning - START
  ref_lkup <- ref_lkup[ref_lkup$cache_id %in% paths_ids, ]
  # TEMP cleaning - END
  ref_lkup$path <- sprintf(
    "%s/survey_data/%s.fst",
    data_dir, ref_lkup$cache_id
  )
  # TEMP fix to add country and region name
  ref_lkup <- merge(ref_lkup, countries[, c('country_code', 'country_name')],
                    by = 'country_code', all.x = TRUE)
  ref_lkup <- merge(ref_lkup, regions[, c('region_code', 'region_name')],
                    by = 'region_code', all.x = TRUE)
  # TEMP fix - END
  # TEMP fix for ARG - START
  # There is a bug in the pipeline causing ARG to have duplicated rows
  ref_lkup <- ref_lkup[!(country_code == "ARG" & reporting_level == "national")]
  # TEMP fix for ARG - END

  # Add data interpolation ID (unique combination of survey files used for one
  # or more reporting years)
  ref_lkup <- ref_lkup[, data_interpolation_id := paste(cache_id, reporting_level, sep = "_")]
  ref_lkup <- ref_lkup[, data_interpolation_id := paste(unique(data_interpolation_id), collapse = "|"),
                       by = .(interpolation_id)]

  # Create interpolation list.
  # This is to facilitate interpolation computations
  unique_survey_files <- unique(ref_lkup$data_interpolation_id)
  interpolation_list <- vector(mode = "list", length = length(unique_survey_files))

  for (i in seq_along(interpolation_list)) {
    tmp_metadata <- ref_lkup[data_interpolation_id == unique_survey_files[i], ]
    cache_ids <- unique(tmp_metadata[["cache_id"]])
    reporting_level = unique(tmp_metadata[["reporting_level"]])
    paths <- unique(tmp_metadata$path)
    ctry_years <- unique(tmp_metadata[, c(
      "country_code", "reporting_year",
      "reporting_level", "interpolation_id"
    )])

    interpolation_list[[i]] <- list(
      tmp_metadata = tmp_metadata,
      cache_ids    = cache_ids,
      reporting_level = reporting_level,
      paths = paths,
      ctry_years = ctry_years
    )
  }

  names(interpolation_list) <- unique_survey_files

  # Load dist_stats
  dist_stats <- fst::read_fst(sprintf("%s/estimations/dist_stats.fst", data_dir),
                              as.data.table = TRUE)

  # Load pop_region
  pop_region <- fst::read_fst(sprintf("%s/_aux/pop_region.fst", data_dir),
    as.data.table = TRUE)

  # Load country profiles lkups
  cp_lkups <- readRDS(sprintf("%s/_aux/country_profiles.RDS", data_dir))

  # Load poverty lines table
  pl_lkup <- fst::read_fst(sprintf("%s/_aux/poverty_lines.fst", data_dir),
    as.data.table = TRUE)

  # Load list with censor tables
  censored <- readRDS(sprintf("%s/_aux/censored.RDS", data_dir))

  # Create pip return columns
  pip_cols <-
    c('region_name', 'region_code', 'country_name', 'country_code', 'reporting_year',
      'reporting_level', 'survey_acronym', 'survey_coverage',
      'survey_year', 'welfare_type', 'survey_comparability',
      'comparable_spell', 'poverty_line',
      'headcount', 'poverty_gap', 'poverty_severity', 'watts',
      'mean', 'median', 'mld', 'gini', 'polarization',
      'decile1', 'decile2', 'decile3', 'decile4', 'decile5',
      'decile6', 'decile7', 'decile8', 'decile9', 'decile10',
       # 'survey_mean_lcu', 'survey_mean_ppp', # Do we need these?
       # 'predicted_mean_ppp', # Do we need this?
      'cpi', #'cpi_data_level',
      'ppp', #'ppp_data_level',
      'reporting_pop', #'pop_data_level',
      'reporting_gdp', #'gdp_data_level',
      'reporting_pce', #'pce_data_level',
      'is_interpolated', # 'is_used_for_aggregation',
      'distribution_type',
      'estimation_type'
      # 'gd_type', 'path',
      # 'cache_id', 'survey_id', 'surveyid_year'
      # 'wb_region_code', 'interpolation_id'
    )

  # Create list of available auxiliary data tables
  aux_tables <- tools::file_path_sans_ext(list.files(paste0(data_dir, "/_aux"),
                                                     pattern = "\\.fst$"))
  aux_tables <- sort(aux_tables)


  # Create list of query controls
  query_controls <-
    create_query_controls(
      svy_lkup = svy_lkup,
      ref_lkup = ref_lkup,
      versions = versions)

  # Create list of lkups
  lkups <- list(
    svy_lkup = svy_lkup,
    ref_lkup = ref_lkup,
    dist_stats = dist_stats,
    pop_region = pop_region,
    cp_lkups = cp_lkups,
    pl_lkup = pl_lkup,
    censored = censored,
    pip_cols = pip_cols,
    query_controls = query_controls,
    data_root = data_dir,
    aux_tables = aux_tables,
    interpolation_list = interpolation_list
  )

  return(lkups)
}
