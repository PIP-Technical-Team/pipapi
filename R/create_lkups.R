#' Create one list of lookups per data version
#'
#' @param data_dir character: Path to the main data directory
#' @param vintage_pattern character: regex that identifies the name pattern of
#'   vintage folders
#' @return list
#' @export
#'
create_versioned_lkups <-
  function(data_dir,
           vintage_pattern = get_vintage_pattern_regex()) {

    data_dirs <- extract_data_dirs(data_dir = data_dir,
                                   vintage_pattern = vintage_pattern)

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
#' @inheritParams create_versioned_lkups
#'
#' @return character
#' @export
#'
extract_data_dirs <-
  function(data_dir,
           vintage_pattern
           ) {
  # List data directories under data_dir

  data_dirs  <- fs::dir_ls(data_dir, type = "directory")
  dirs_names <- basename(data_dirs)

  valid_dir <- id_valid_dirs(dirs_names      = dirs_names,
                             vintage_pattern = vintage_pattern)

  data_dirs  <- data_dirs[valid_dir]
  versions   <- dirs_names[valid_dir]

  names(data_dirs) <- versions


  # Sorting according to identity
  versions_prod <- versions[grepl("PROD$", versions)]
  versions_prod <- sort(versions_prod, decreasing = TRUE)

  versions_test <- versions[grepl("TEST$", versions)]
  versions_test <- sort(versions_test, decreasing = TRUE)

  versions_int  <- versions[grepl("INT$",  versions)]
  versions_int  <- sort(versions_int, decreasing = TRUE)

  # sort directories
  sorted_versions <- c(versions_prod, versions_int, versions_test)
  data_dirs <- data_dirs[sorted_versions]

  return(data_dirs)
}


#' Create look-up tables
#'
#' Create look-up tables that can be passed to [pip()].
#'
#' @param data_dir character: Path to PIP data root folder.
#' @param versions character: Available data versions
#' @return list
#'
#' @export

create_lkups <- function(data_dir, versions) {

  # Get survey paths
  paths <- list.files(fs::path(data_dir, "survey_data"))

  paths_ids <- tools::file_path_sans_ext(paths)

  # TEMP FIX to add country and region name
  cts_path <- fs::path(data_dir, "_aux/countries.fst")
  reg_path <- fs::path(data_dir, "_aux/regions.fst")

  countries <-  fst::read_fst(cts_path,as.data.table = TRUE)
  regions   <-  fst::read_fst(reg_path,as.data.table = TRUE)

  data.table::setnames(regions, 'region', 'region_name')

  # TEMP fix - END (see further code chunks below )

  # Clean svy_lkup
  svy_lkup_path <- fs::path(data_dir, "estimations/prod_svy_estimation.fst")
  svy_lkup      <- fst::read_fst(svy_lkup_path, as.data.table = TRUE)

  # TEMP cleaning - START
  svy_lkup <- svy_lkup[svy_lkup$cache_id %in% paths_ids, ]

  # TEMP cleaning - END
  svy_lkup$path <- fs::path(data_dir,"survey_data", svy_lkup$cache_id, ext = "fst")

  # TEMP fix to add country and region name
  svy_lkup <- merge(svy_lkup, countries[, c('country_code', 'country_name')],
                    by = 'country_code', all.x = TRUE)
  svy_lkup <- merge(svy_lkup, regions[, c('region_code', 'region_name')],
                    by = 'region_code', all.x = TRUE)
  # TEMP fix - END
  # Clean ref_lkup
  ref_lkup_path <- fs::path(data_dir, "estimations/prod_ref_estimation.fst")
  ref_lkup      <- fst::read_fst(ref_lkup_path, as.data.table = TRUE)


  # TEMP cleaning - START
  ref_lkup <- ref_lkup[ref_lkup$cache_id %in% paths_ids, ]

  # TEMP cleaning - END

  ref_lkup$path <-
    fs::path(data_dir, "survey_data", ref_lkup$cache_id, ext = "fst")


  # TEMP fix to add country and region name
  ref_lkup <- merge(ref_lkup, countries[, c('country_code', 'country_name')],
                    by = 'country_code', all.x = TRUE)
  ref_lkup <- merge(ref_lkup, regions[, c('region_code', 'region_name')],
                    by = 'region_code', all.x = TRUE)
  # TEMP fix - END

  # Add data interpolation ID (unique combination of survey files used for one
  # or more reporting years)
  ref_lkup <-
    ref_lkup[,
             data_interpolation_id := paste(cache_id,
                                            reporting_level,
                                            sep = "_")
             ]

  ref_lkup <-
    ref_lkup[,
             data_interpolation_id := paste(unique(data_interpolation_id),
                                            collapse = "|"),
             by = .(interpolation_id)]

  # Create interpolation list.
  # This is to facilitate interpolation computations
  unique_survey_files <- unique(ref_lkup$data_interpolation_id)
  interpolation_list  <- vector(mode = "list", length = length(unique_survey_files))

  for (i in seq_along(interpolation_list)) {

    tmp_metadata    <- ref_lkup[data_interpolation_id == unique_survey_files[i], ]
    cache_ids       <- unique(tmp_metadata[["cache_id"]])
    reporting_level <- unique(tmp_metadata[["reporting_level"]])
    paths           <- unique(tmp_metadata$path)
    ctry_years      <- unique(tmp_metadata[, c("country_code", "reporting_year",
                                               "reporting_level", "interpolation_id"
                                               )
                                           ])

    interpolation_list[[i]] <-
      list(tmp_metadata    = tmp_metadata,
           cache_ids       = cache_ids,
           reporting_level = reporting_level,
           paths           = paths,
           ctry_years      = ctry_years
           )
  }

  names(interpolation_list) <- unique_survey_files

  # Load dist_stats
  dist_stats_path <- fs::path(data_dir, "estimations/dist_stats.fst")
  dist_stats      <- fst::read_fst(dist_stats_path, as.data.table = TRUE)

  # Load pop_region
  pop_region_path <- fs::path(data_dir, "_aux/pop_region.fst")
  pop_region      <- fst::read_fst(pop_region_path,as.data.table = TRUE)

  # Load country profiles lkups
  cp_lkups_path   <- fs::path(data_dir, "_aux/country_profiles.RDS")
  cp_lkups        <- readRDS(cp_lkups_path)

  # Load poverty lines table
  pl_lkup_path    <- fs::path(data_dir, "_aux/poverty_lines.fst")
  pl_lkup         <- fst::read_fst(pl_lkup_path, as.data.table = TRUE)

  # Load list with censor tables
  censored_path   <- fs::path(data_dir, "_aux/censored.RDS")
  censored        <- readRDS(censored_path)

  # Create pip return columns
  pip_cols <-
    c(
      'region_name',
      'region_code',
      'country_name',
      'country_code',
      'reporting_year',
      'reporting_level',
      'survey_acronym',
      'survey_coverage',
      'survey_year',
      'welfare_type',
      'survey_comparability',
      'comparable_spell',
      'poverty_line',
      'headcount',
      'poverty_gap',
      'poverty_severity',
      'watts',
      'mean',
      'median',
      'mld',
      'gini',
      'polarization',
      'decile1',
      'decile2',
      'decile3',
      'decile4',
      'decile5',
      'decile6',
      'decile7',
      'decile8',
      'decile9',
      'decile10',
      # 'survey_mean_lcu', 'survey_mean_ppp', # Do we need these?
      # 'predicted_mean_ppp', # Do we need this?
      'cpi',
      #'cpi_data_level',
      'ppp',
      #'ppp_data_level',
      'reporting_pop',
      #'pop_data_level',
      'reporting_gdp',
      #'gdp_data_level',
      'reporting_pce',
      #'pce_data_level',
      'is_interpolated',
      # 'is_used_for_aggregation',
      'distribution_type',
      'estimation_type'
      # 'gd_type', 'path',
      # 'cache_id', 'survey_id', 'surveyid_year'
      # 'wb_region_code', 'interpolation_id'
    )

  # Create list of available auxiliary data tables
  aux_tables <- list.files(fs::path(data_dir, "_aux"),pattern = "\\.fst$")
  aux_tables <- tools::file_path_sans_ext(aux_tables)
  aux_tables <- sort(aux_tables)


  # Create list of query controls
  query_controls <-
    create_query_controls(
      svy_lkup = svy_lkup,
      ref_lkup = ref_lkup,
      versions = versions)

  # Create list of lkups
  lkups <- list(
    svy_lkup           = svy_lkup,
    ref_lkup           = ref_lkup,
    dist_stats         = dist_stats,
    pop_region         = pop_region,
    cp_lkups           = cp_lkups,
    pl_lkup            = pl_lkup,
    censored           = censored,
    pip_cols           = pip_cols,
    query_controls     = query_controls,
    data_root          = data_dir,
    aux_tables         = aux_tables,
    interpolation_list = interpolation_list
  )

  return(lkups)
}

#' Return regular expression needed for extracting data folders
#' Helper function to facilitate testing
#'
#' @return character

get_vintage_pattern_regex <- function() {
  "\\d{8}_\\d{4}_\\d{2}_\\d{2}_(PROD|TEST|INT)$"
}

#' Identify valid data directories
#' Helper function to facilitate testing
#'
#' @return logical
id_valid_dirs <- function(dirs_names,
                          vintage_pattern) {
  grepl(vintage_pattern, dirs_names)
}
