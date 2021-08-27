#' Create look-up tables
#'
#' Create look-up tables that can be passed to [pip()].
#'
#' @param data_dir character: Path to PIP data root folder.
#' @return list
#' @export
create_lkups <- function(data_dir) {

  # Get survey paths
  paths <- list.files(paste0(data_dir, "/survey_data"))
  paths_ids <- tools::file_path_sans_ext(paths)

  # Clean svy_lkup
  svy_lkup <- fst::read_fst(sprintf("%s/estimations/prod_svy_estimation.fst", data_dir),
    as.data.table = TRUE
  )
  # TEMP cleaning - START
  svy_lkup <- svy_lkup[svy_lkup$cache_id %in% paths_ids, ]
  # TEMP cleaning - END
  svy_lkup$path <- sprintf(
    "%ssurvey_data/%s.fst",
    data_dir, svy_lkup$cache_id
  )
  # Clean ref_lkup
  ref_lkup <- fst::read_fst(sprintf("%s/estimations/prod_ref_estimation.fst", data_dir),
    as.data.table = TRUE
  )
  # TEMP cleaning - START
  ref_lkup <- ref_lkup[ref_lkup$cache_id %in% paths_ids, ]
  # TEMP cleaning - END
  ref_lkup$path <- sprintf(
    "%ssurvey_data/%s.fst",
    data_dir, ref_lkup$cache_id
  )

  # Load pop_region
  pop_region <- fst::read_fst(sprintf("%s/_aux/pop_region.fst", data_dir),
    as.data.table = TRUE
  )

  # Load country profiles lkups
  cp_lkups <- readRDS(sprintf("%s/_aux/country_profiles.RDS", data_dir))

  # Load poverty lines table
  pl_lkup <- fst::read_fst(sprintf("%s/_aux/poverty_lines.fst", data_dir),
    as.data.table = TRUE
  )

  # Create pip return columns
  pip_cols <-
    c('region_code', 'country_code', 'reporting_year',
      'survey_acronym', 'survey_coverage', 'survey_year',
      'welfare_type', 'survey_comparability', 'comparable_spell',
      'poverty_line', 'headcount', 'poverty_gap', 'poverty_severity',
      'mean', 'median', 'mld', 'gini', 'polarization', 'watts',
      'decile1', 'decile2', 'decile3', 'decile4', 'decile5',
      'decile6', 'decile7', 'decile8', 'decile9', 'decile10',
      'survey_mean_lcu', 'survey_mean_ppp', # Do we need these?
      'predicted_mean_ppp', # Do we need this?
      'cpi', 'cpi_data_level',
      'ppp', 'ppp_data_level',
      'reporting_pop', 'pop_data_level',
      'reporting_gdp', 'gdp_data_level',
      'reporting_pce', 'pce_data_level',
      'is_interpolated', 'is_used_for_aggregation',
      'distribution_type', 'estimation_type'
      # 'gd_type', 'path',
      # 'cache_id', 'survey_id', 'surveyid_year'
      # 'wb_region_code', 'interpolation_id'
    )

  # Create list of query controls
  query_controls <-
    create_query_controls(
      data_dir,
      svy_lkup = svy_lkup,
      ref_lkup = ref_lkup)

  # Create list of lkups
  lkups <- list(
    svy_lkup = svy_lkup,
    ref_lkup = ref_lkup,
    pop_region = pop_region,
    cp_lkups = cp_lkups,
    pl_lkup = pl_lkup,
    pip_cols = pip_cols,
    query_controls = query_controls,
    data_root = data_dir
  )

  return(lkups)
}
