#' Temporary function that takes care of basic data cleaning and validation
#'
#' @param data_folder_root character: Path to data folder relative to the location
#' of the plumber.R file
#'
#' @return list
#'

clean_api_data <- function(data_folder_root) {
  paths <- fs::dir_ls(paste0(data_folder_root, "/survey_data"), recurse = FALSE, type = "file")
  paths_ids <- basename(paths)
  paths_ids <- tools::file_path_sans_ext(paths_ids)

  # Clean svy_lkup
  svy_lkup <- fst::read_fst(sprintf("%s/estimations/survey_means.fst", data_folder_root),
    as.data.table = TRUE
  )
  # TEMP cleaning - START
  svy_lkup <- svy_lkup[!is.na(svy_lkup$survey_mean_ppp), ]
  svy_lkup <- svy_lkup[!is.na(svy_lkup$ppp), ]
  svy_lkup <- svy_lkup[svy_lkup$cache_id %in% paths_ids, ]
  # TEMP cleaning - END
  svy_lkup$path <- sprintf(
    "%ssurvey_data/%s.fst",
    data_folder_root, svy_lkup$cache_id
  )
  # Clean ref_lkup
  ref_lkup <- fst::read_fst(sprintf("%s/estimations/interpolated_means.fst", data_folder_root),
    as.data.table = TRUE
  )
  # TEMP cleaning - START
  ref_lkup <- ref_lkup[!is.na(ref_lkup$predicted_mean_ppp), ]
  ref_lkup <- ref_lkup[!is.na(ref_lkup$ppp), ]
  ref_lkup <- ref_lkup[ref_lkup$cache_id %in% paths_ids, ]
  # TEMP cleaning - END
  ref_lkup$path <- sprintf(
    "%ssurvey_data/%s.fst",
    data_folder_root, ref_lkup$cache_id
  )

  # Load dist_stats
  dist_stats <- fst::read_fst(sprintf("%s/estimations/dist_stats.fst", data_folder_root),
    as.data.table = TRUE
  )

  # Load pop_region
  pop_region <- fst::read_fst(sprintf("%s/_aux/pop_region.fst", data_folder_root),
    as.data.table = TRUE
  )

  # Add country profiles lkups
  cp_lkups <- readRDS(sprintf("%s/_aux/country_profiles.RDS", data_folder_root))

  # Add poverty lines table
  pl_lkup <- fst::read_fst(sprintf("%s/_aux/poverty_lines.fst", data_folder_root),
    as.data.table = TRUE
  )

  # Create query controls
  country <- list(
    values = c(
      "all",
      sort(unique(c(
        svy_lkup$country_code,
        ref_lkup$country_code
      )))
    ),
    type = "character"
  )
  year <- list(
    values = c(
      "all", "mrv",
      sort(unique(c(
        svy_lkup$reporting_year,
        ref_lkup$reporting_year
      )))
    ),
    type = "character"
  )
  povline <- list(
    values = c(min = 0, max = 100),
    type = "numeric"
  )
  popshare <- list(
    values = c(min = 0, max = 1),
    type = "numeric"
  )
  fill_gaps <- aggregate <- list(
    values = c(TRUE, FALSE),
    type = "logical"
  )
  group_by <- list(
    values = c("none", "wb"),
    type = "character"
  )
  welfare_type <- list(
    values = c("all", sort(unique(c(
      svy_lkup$welfare_type,
      ref_lkup$welfare_type
    )))),
    type = "character"
  )
  reporting_level <- list(
    values = c(
      "all",
      sort(unique(c(
        svy_lkup$pop_data_level,
        ref_lkup$pop_data_level
      )))
    ),
    type = "character"
  )
  ppp <- list(
    values = c(min = 0, max = 1000000), # CHECK THE VALUE OF MAX
    type = "numeric"
  )

  versions <- fs::dir_ls(
    path = data_folder_root,
    type = "directory",
    recurse = FALSE
  )

  formats <- list(values = c("json", "csv", "rds"), type = "character")

  # Create list of query controls
  query_controls <- list(
    country = country,
    year = year,
    povline = povline,
    popshare = popshare,
    fill_gaps = fill_gaps,
    aggregate = aggregate,
    group_by = group_by,
    welfare_type = welfare_type,
    reporting_level = reporting_level,
    ppp = ppp,
    version = versions,
    format = formats
  )

  # Create list of lkups
  lkups <- list(
    svy_lkup = svy_lkup,
    ref_lkup = ref_lkup,
    dist_stats = dist_stats,
    pop_region = pop_region,
    cp_lkups = cp_lkups,
    pl_lkup = pl_lkup,
    query_controls = query_controls,
    data_root = data_folder_root
  )

  return(lkups)
}
