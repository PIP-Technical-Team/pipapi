#' Create one list of lookups per data version
#'
#' @param data_dir character: Path to the main data directory
#' @param vintage_pattern character: regex that identifies the name pattern of
#'   vintage folders
#' @return list
#' @export
create_versioned_lkups <-
  function(data_dir,
           vintage_pattern = NULL) {

    vintage_pattern <- create_vintage_pattern_call(vintage_pattern)


    data_dirs <- extract_data_dirs(data_dir = data_dir,
                                   vintage_pattern = vintage_pattern)

    versions <- names(data_dirs)
    # versions[1] <- "latest_release"

    versions_paths <- lapply(data_dirs, create_lkups, versions = versions)
    names(versions_paths) <- versions

    return(list(versions = versions,
                versions_paths = versions_paths,
                latest_release = versions[1]))

  }

#' Extract list of data sub-directories from main data directory
#'
#' @inheritParams create_versioned_lkups
#' @return character
#' @noRd
extract_data_dirs <-
  function(data_dir,
           vintage_pattern
           ) {
  # List data directories under data_dir

  data_dirs  <- fs::dir_ls(data_dir, type = "directory")
  dirs_names <- basename(data_dirs)

  valid_dir <- id_valid_dirs(dirs_names      = dirs_names,
                             vintage_pattern = vintage_pattern$vintage_pattern)

  data_dirs  <- data_dirs[valid_dir]
  versions   <- dirs_names[valid_dir]

  names(data_dirs) <- versions


  # Sorting according to identity
  sorted_versions <- sort_versions(versions = versions,
                                   prod_regex = vintage_pattern$prod_regex,
                                   int_regex  = vintage_pattern$int_regex,
                                   test_regex = vintage_pattern$test_regex)
  # sort directories
  data_dirs <- data_dirs[sorted_versions]

  return(data_dirs)
}


#' Create look-up tables
#'
#' Create look-up tables that can be passed to [pip()].
#'
#' @param data_dir character: Path to PIP data root folder.
#' @param versions character: Available data versions
#' @keywords internal
#' @return list
create_lkups <- function(data_dir, versions) {

  # Get survey paths ----
  paths <- list.files(fs::path(data_dir, "survey_data"))
  paths_ids <- tools::file_path_sans_ext(paths)

  # CREATE OBJECT: aux_files ----
  # Files with country and region information
  ## missing_data ----
  # Countries with Missing data
  msd_lkup_path    <- fs::path(data_dir, "_aux/missing_data.fst")
  missing_data     <- fst::read_fst(msd_lkup_path, as.data.table = TRUE)

  ## country_list ----
  cl_lkup_path    <- fs::path(data_dir, "_aux/country_list.fst")
  country_list     <- fst::read_fst(cl_lkup_path, as.data.table = TRUE)

  ## countries ----
  cts_path <- fs::path(data_dir, "_aux/countries.fst")
  countries <-  fst::read_fst(cts_path, as.data.table = TRUE)
  data.table::setnames(countries, 'region', 'region_name') # Why is this necessary?

  ## regions ----
  reg_path <- fs::path(data_dir, "_aux/regions.fst")
  regions   <-  fst::read_fst(reg_path, as.data.table = TRUE)

  ## pop ----
  # population
  pop_path    <- fs::path(data_dir, "_aux/pop.fst")
  pop         <- fst::read_fst(pop_path, as.data.table = TRUE)

  aux_files <- list(missing_data = missing_data,
                    country_list = country_list,
                    countries    = countries,
                    regions      = regions,
                    pop          = pop)

  # CREATE OBJECT: svy_lkup ----
  svy_lkup_path <- fs::path(data_dir, "estimations/prod_svy_estimation.fst")
  svy_lkup      <- fst::read_fst(svy_lkup_path, as.data.table = TRUE)

  ## TEMP cleaning - START ----
  svy_lkup <- svy_lkup[svy_lkup$cache_id %in% paths_ids, ]


  svy_lkup$path <- fs::path(data_dir,"survey_data", svy_lkup$cache_id, ext = "fst")

  ## TEMP: Ideally, region should come from one single place
  if ("region_code" %in% names(svy_lkup)) {
    svy_lkup[,
             region_code := NULL]
  }

  ## TEMP fix to add country and region name
  svy_lkup <- merge(svy_lkup, countries,
                    by = 'country_code',
                    all.x = TRUE)
  ## TEMP cleaning - END

  # CREATE OBJECT: ref_lkup ----
  ref_lkup_path <- fs::path(data_dir, "estimations/prod_ref_estimation.fst")
  ref_lkup      <- fst::read_fst(ref_lkup_path, as.data.table = TRUE)

  ## TEMP cleaning - START ----
  ref_lkup <- ref_lkup[ref_lkup$cache_id %in% paths_ids, ]

  # TEMP: Ideally, region should come from one single place
  if ("region_code" %in% names(ref_lkup)) {
    ref_lkup[,
             region_code := NULL]
  }

  # TEMP fix to add country and region name
  ref_lkup <-  merge(ref_lkup, countries,
                     by = 'country_code',
                     all.x = TRUE)
  ## TEMP cleaning - END

  # Add path to survey files
  ref_lkup$path <-
  fs::path(data_dir, "survey_data", ref_lkup$cache_id, ext = "fst")

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


  # CREATE OBJECT: interpolation_list ----
  # This is to facilitate interpolation computations
  unique_survey_files <- unique(ref_lkup$data_interpolation_id)
  interpolation_list  <- vector(mode = "list", length = length(unique_survey_files))

  for (i in seq_along(interpolation_list)) {

    tmp_metadata    <- ref_lkup[data_interpolation_id == unique_survey_files[i], ]
    cache_ids       <- unique(tmp_metadata[["cache_id"]])
    reporting_level <- unique(tmp_metadata[["reporting_level"]])
    paths           <- unique(tmp_metadata$path)
    ctry_years      <- unique(tmp_metadata[, c("region_code",
                                               "country_code",
                                               "reporting_year",
                                               "reporting_level",
                                               "interpolation_id"
    )
    ])

    interpolation_list[[i]] <-
      list(#tmp_metadata    = tmp_metadata,
        cache_ids       = cache_ids,
        reporting_level = reporting_level,
        paths           = paths,
        ctry_years      = ctry_years
      )
  }

  names(interpolation_list) <- unique_survey_files

  # CREATE OBJECT: dist_stats ----
  dist_stats_path <- fs::path(data_dir, "estimations/dist_stats.fst")
  dist_stats      <- fst::read_fst(dist_stats_path, as.data.table = TRUE)

  # CREATE OBJECT: pop_region ----
  pop_region_path <- fs::path(data_dir, "_aux/pop_region.fst")
  pop_region      <- fst::read_fst(pop_region_path,as.data.table = TRUE)

  # CREATE OBJECT: cp_lkups ----
  # country profiles lkups
  cp_lkups_path   <- fs::path(data_dir, "_aux/country_profiles.rds")
  cp_lkups        <- readRDS(cp_lkups_path)

  # CREATE OBJECT: pl_lkup ----
  # poverty lines table
  pl_lkup_path    <- fs::path(data_dir, "_aux/poverty_lines.fst")
  pl_lkup         <- fst::read_fst(pl_lkup_path, as.data.table = TRUE)

  # CREATE OBJECT: censored
  # list with censor tables
  censored_path   <- fs::path(data_dir, "_aux/censored.rds")
  censored        <- readRDS(censored_path)

  # CREATE OBJECT: return_cols ----
  return_cols <- create_return_cols(
    pip = list(
      cols = c( # Columns for pip call
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
        'cpi',
        'ppp',
        'reporting_pop',
        'reporting_gdp',
        'reporting_pce',
        'is_interpolated',
        'distribution_type',
        'estimation_type',
        'spl',
        'spr',
        'pg',
        'estimate_type'
      ),

      # This is not used anywhere anymore.
      dist_stats = c(
        "country_code",
        "reporting_year",
        "welfare_type",
        "reporting_level",
        "spl",
        'pg'
        #"spr"
      )
    ),
    pip_grp = list(
      cols = c( # Columns for pip_grp call
        "region_name",
        "region_code",
        "reporting_year",
        "reporting_pop",
        "poverty_line",
        "headcount",
        "poverty_gap",
        "poverty_severity",
        "watts",
        "mean",
        "pop_in_poverty",
        "spr",
        'pg',
        'estimate_type'
      ),
      weighted_average_cols = c(
        "headcount",
        "poverty_gap",
        "poverty_severity",
        "watts",
        "mean",
        "spr",
        'pg'
      )
    ),
    ui_pc_charts = list(
      cols = c(
        'country_code',
        'reporting_year',
        'welfare_type',
        'reporting_level',
        'median',
        'gini',
        'polarization',
        'mld',
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
        'region_code',
        'survey_coverage',
        'survey_comparability',
        'comparable_spell',
        'survey_year',
        'reporting_pop',
        'ppp',
        'cpi',
        'distribution_type',
        'is_interpolated',
        'poverty_line',
        'mean',
        'headcount',
        'poverty_gap',
        'poverty_severity',
        'watts',
        'pop_in_poverty',
        'spr',
        'pg',
        'estimate_type'
      ),
      inequality_indicators = c(
        'median',
        'gini',
        'polarization',
        'mld',
        'decile1',
        'decile2',
        'decile3',
        'decile4',
        'decile5',
        'decile6',
        'decile7',
        'decile8',
        'decile9',
        'decile10'
      )
    ),
    ag_average_poverty_stats = list(
      noneg_vars = c(
        "mean",
        "median",
        "headcount",
        "poverty_gap",
        "poverty_severity",
        "watts",
        "spr",
        'pg'
      ),
      zero_vars = c(
        "mean",
        "median",
        "watts"
      ),
      na_cols = c(
        "survey_mean_lcu",
        "ppp",
        "median",
        "survey_median_ppp"
      ),
      national_cols = c(
        "reporting_level",
        "gdp_data_level",
        "pce_data_level",
        "cpi_data_level",
        "ppp_data_level"
      )

    )
  )

  # CREATE OBJECT: aux_tables ----
  # Create list of available auxiliary data tables
  aux_tables <- list.files(fs::path(data_dir, "_aux"),pattern = "\\.fst$")
  aux_tables <- tools::file_path_sans_ext(aux_tables)
  aux_tables <- sort(aux_tables)

  # CREATE OBJECT: valid_years ----
  valid_years <- valid_years(data_dir)

  # CREATE OBJECT: query_controls ----
  # Create list of query controls
  query_controls <-
    create_query_controls(
      svy_lkup   = svy_lkup,
      ref_lkup   = ref_lkup,
      aux_files = aux_files,
      aux_tables = aux_tables,
      versions   = versions)

  # CREATE OBJECT: cache_data_id ----
  # The cache_data_id will be used to trigger cache invalidation
  # Using this cache_data_id as a function argument, cache can be pre-computed
  # and then deployed to the server

  ## Remove non-hash-able variables ----
  ## Some variables contain information that is specific to the local machine
  ## `pipapi` runs onto, such as path to data files, which will not be the same
  ## on my laptop, and on the PROD VM. These variables therefore need to be removed
  ## prior to the creation of the cache_data_id
  hash_svy_lkup <- svy_lkup
  hash_svy_lkup$path <- NULL

  hash_ref_lkup <- ref_lkup
  hash_ref_lkup$path <- NULL

  query_controls_hash <- query_controls
  query_controls_hash$version <- NULL

  ## Create cache_data_id for complete lkup ----
  hash_lkup <- list(hash_svy_lkup,
                    hash_ref_lkup,
                    dist_stats,
                    pop_region,
                    cp_lkups,
                    pl_lkup,
                    censored,
                    aux_files,
                    return_cols,
                    query_controls,
                    aux_tables,
                    valid_years
                    )
  hash_lkup <- rlang::hash(hash_lkup)

  ## Create cache_data_id for pip() ----
  hash_pip <- list(hash_svy_lkup,
                   hash_ref_lkup,
                   dist_stats,
                   pop_region,
                   censored,
                   aux_files,
                   return_cols$pip,
                   query_controls$region$values,
                   valid_years
                   )
  hash_pip <- rlang::hash(hash_pip)

  ## Create cache_data_id for pip_grp() ----
  ## Same data signature for pip_grp and pip_grp_logic
  hash_pip_grp <- list(hash_ref_lkup,
                       dist_stats,
                       pop_region,
                       censored,
                       aux_files,
                       return_cols$pip_grp,
                       query_controls$region$values,
                       valid_years
  )
  hash_pip_grp <- rlang::hash(hash_pip_grp)

  ## Create cache_data_id for ui_cp ----
  ## Same data signature for ui_cp_key_indicators, ui_cp_charts and ui_cp_download
  hash_ui_cp <- list(hash_svy_lkup,
                     dist_stats,
                     pop_region,
                     censored,
                     aux_files,
                     return_cols,
                     query_controls$region$values,
                     valid_years,
                     pl_lkup,
                     cp_lkups
  )
  hash_ui_cp <- rlang::hash(hash_ui_cp)


  ## Create cache_data_id list ----
  cache_data_id <- list(
    hash_lkup    = hash_lkup,
    hash_pip     = hash_pip,
    hash_pip_grp = hash_pip_grp,
    hash_ui_cp   = hash_ui_cp

  )


  # COERCE character to factors
  # svy_lkup <- coerce_chr_to_fct(svy_lkup)
  # dist_stats <- coerce_chr_to_fct(dist_stats)
  # ref_lkup <- coerce_chr_to_fct(ref_lkup)

  # Create list of lkups
  lkup <- list(
    svy_lkup           = svy_lkup,
    ref_lkup           = ref_lkup,
    dist_stats         = dist_stats,
    pop_region         = pop_region,
    cp_lkups           = cp_lkups,
    pl_lkup            = pl_lkup,
    censored           = censored,
    aux_files          = aux_files,
    return_cols        = return_cols,
    query_controls     = query_controls,
    data_root          = data_dir,
    aux_tables         = aux_tables,
    interpolation_list = interpolation_list,
    valid_years        = valid_years,
    cache_data_id      = cache_data_id
  )

  return(lkup)
}

#' Return regular expression needed for extracting data folders
#' Helper function to facilitate testing
#'
#' @return list
#' @noRd
get_vintage_pattern_regex <- function(vintage_pattern = NULL,
                                      prod_regex      = NULL,
                                      int_regex       = NULL,
                                      test_regex      = NULL
                                      ) {


  list(

    vintage_pattern = ifel_isnull(vintage_pattern,
                                 "\\d{8}_\\d{4}_\\d{2}_\\d{2}_(PROD|TEST|INT)$"),

    prod_regex      = ifel_isnull(prod_regex,
                             "PROD$"),

    int_regex       = ifel_isnull(int_regex,
                             "INT$"),

    test_regex      = ifel_isnull(test_regex,
                             "TEST$")
    )
}

#' Efficient "if" "else" evaluation of null.
#'
#' @param x object to evaluate
#' @param y in case x null. If X is not null, then x.
#'
#' @return object of class(x)
ifel_isnull <- function(x, y) {

  if (is.null(x)) {
    y
  } else {
    x
  }

}



#' create vintage call to be parsed into `get_vintage_pattern_regex()`
#'
#' @param vintage_pattern either NULL, chracter with regex or list of arguments
#'   for `get_vintage_pattern_regex()`
#'
#' @return list to be parses t `get_vintage_pattern_regex()`
#' @examples
#' \dontrun{
#' vintage_pattern <- NULL
#' create_vintage_pattern_call(vintage_pattern)
#'
#' vintage_pattern <- list("r.*", "", "^hjkhj\\.d")
#' create_vintage_pattern_call(vintage_pattern)
#'
#' vintage_pattern <- c("r.*", "", "^hjkhj\\.d")
#' create_vintage_pattern_call(vintage_pattern)
#'
#' vintage_pattern <- c(vintage_pattern = "r.*", test_regex = "", int_regex =  "^hjkhj\\.d")
#' create_vintage_pattern_call(vintage_pattern)
#' }
create_vintage_pattern_call <- function(vintage_pattern = NULL) {

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  stopifnot( exprs = {
    class(vintage_pattern) %in% c("NULL",  "list",  "character")
  }
  )

  #   ______________________________________________________________________
  #   Computations                                                      ####
  vp <-
    if (is.null(vintage_pattern)) {

      get_vintage_pattern_regex()

    } else {


      lf <-
        formals(get_vintage_pattern_regex) |>
        names() |>
        length()

      l <- length(vintage_pattern)

      stopifnot(l >= 1 && l <= lf)

      if (inherits(vintage_pattern, "list")) { # if list

        do.call(get_vintage_pattern_regex,
                vintage_pattern)


      } else { # if character
        do.call(get_vintage_pattern_regex,
                as.list(vintage_pattern))
      }

    }


  #   ____________________________________________________________
  #   Return                                                     ####
  return(vp)

}



#' Identify valid data directories
#' Helper function to facilitate testing
#'
#' @return logical
#' @noRd
id_valid_dirs <- function(dirs_names,
                          vintage_pattern) {
  grepl(vintage_pattern, dirs_names)
}

#' Sort available data folders
#' Helper function to facilitate unit testing
#'
#' @param versions character: vector of available versions
#' @param prod_regex character: Regex expression to identify production versions
#' @param int_regex character: Regex expression to identify internal versions
#' @param test_regex character: Regex expression to identify test versions
#'
#' @return character
#' @noRd
sort_versions <- function(versions,
                          prod_regex,
                          int_regex,
                          test_regex) {
  versions_prod <- versions[grepl(prod_regex, versions)]
  versions_prod <- sort(versions_prod, decreasing = TRUE)

  versions_int  <- versions[grepl(int_regex,  versions)]
  versions_int  <- sort(versions_int, decreasing = TRUE)

  versions_test <- versions[grepl(test_regex, versions)]
  versions_test <- sort(versions_test, decreasing = TRUE)

  # sort directories
  sorted_versions <- c(versions_prod, versions_int, versions_test)

  return(sorted_versions)
}


coerce_chr_to_fct <- function(df) {
  df <- as.data.frame(df)
  character_vec <- unname(unlist(lapply(df, is.character)))
  df[, character_vec] <- lapply(df[, character_vec], as.factor)
  df <- data.table::as.data.table(df)

  return(df)
}

#' helper function to create a list of return columns for various pipapi functions
#'
#' @param ... Named vectors of columns to be returned
#'
#' @return list
#' @export
#'
create_return_cols <- function(...) {
  out <- list(...)
  return(out)
}




#' Sorted available PIP versions in data directory
#'
#' @param data_dir character: data directory
#'
#' @return character vector of sorted available PIP versions in data directory
available_versions <- function(data_dir) {
  vintage_pattern <- create_vintage_pattern_call()
    fs::dir_ls(data_dir,
               type   = "directory") |>
    fs::path_file() |>
    sort_versions(prod_regex = vintage_pattern$prod_regex,
                  int_regex  = vintage_pattern$int_regex,
                  test_regex = vintage_pattern$test_regex)

}
