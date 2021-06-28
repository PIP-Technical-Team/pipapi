#' Temporary function that takes care of basic data cleaning and validation
#'
#' @param data_folder_root character: Path to data folder relative to the location
#' of the plumber.R file
#'
#' @return list
#'

clean_api_data <- function(data_folder_root) {

  # poverty_lines <- fst::read_fst(paste0(data_folder_root, "/_aux/poverty_lines.fst"))

  paths <- fs::dir_ls(paste0(data_folder_root, "/survey_data"), recurse = FALSE, type = "file")
  paths_ids <- basename(paths)
  paths_ids <- tools::file_path_sans_ext(paths_ids)

  # Clean svy_lkup
  svy_lkup <- fst::read_fst(paste0(data_folder_root, "/estimations/survey_means.fst"))
  # TEMP cleaning - START
  svy_lkup <- svy_lkup[!is.na(svy_lkup$survey_mean_ppp), ]
  svy_lkup <- svy_lkup[!is.na(svy_lkup$ppp), ]
  svy_lkup <- svy_lkup[svy_lkup$cache_id %in% paths_ids, ]
  # TEMP cleaning - END
  svy_lkup$path <- paste0(data_folder_root, "survey_data/", svy_lkup$cache_id, ".fst")
  svy_lkup <- data.table::setDT(svy_lkup)

  # Clean ref_lkup
  ref_lkup <- fst::read_fst(paste0(data_folder_root, "/estimations/interpolated_means.fst"))
  # TEMP cleaning - START
  ref_lkup <- ref_lkup[!is.na(ref_lkup$predicted_mean_ppp), ]
  ref_lkup <- ref_lkup[!is.na(ref_lkup$ppp), ]
  ref_lkup <- ref_lkup[ref_lkup$cache_id %in% paths_ids, ]
  # TEMP cleaning - END
  ref_lkup$path <- paste0(data_folder_root, "survey_data/", ref_lkup$cache_id, ".fst")
  ref_lkup <- data.table::setDT(ref_lkup)
  ref_lkup <- ref_lkup %>%
    dplyr::mutate(
      interpolation_id = paste(country_code, reporting_year, pop_data_level, sep = "_")
    )
  # Load dist_stats
  dist_stats <- fst::read_fst(paste0(data_folder_root, "/estimations/dist_stats.fst"))
  dist_stats <- data.table::setDT(dist_stats)

  # Clean pop_region
  pop_region <- fst::read_fst(paste0(data_folder_root, "/_aux/pop-region.fst")) %>%
    dplyr::rename(reporting_year = year, reporting_pop = pop)
  pop_region <- data.table::setDT(pop_region)

  # Create query controls
  country <- list(values = c("all", sort(unique(c(svy_lkup$country_code, ref_lkup$country_code)))),
                  type = "character")
  year <- list(values = c("all", sort(unique(c(svy_lkup$reporting_year, ref_lkup$reporting_year)))),
               type = "character")
  povline <- list(values = c(min = 0, max = 100),
                  type = "numeric")
  popshare <- list(values = c(min = 0, max = 1),
                   type = "numeric")
  fill_gaps <- aggregate <- list(values = c(TRUE, FALSE),
                                 type = "logical")
  group_by <- list(values = c("wb", "inc"),
                   type = "character")
  welfare_type <- list(values = c("all", sort(unique(c(svy_lkup$welfare_type, ref_lkup$welfare_type)))),
                       type = "character")
  reporting_level <- list(values = c("all", sort(unique(c(svy_lkup$pop_data_level, ref_lkup$pop_data_level)))),
                       type = "character")
  ppp <- list(values = c(min = 0, max = 1000000), # CHECK THE VALUE OF MAX
              type = "numeric")

  versions <- fs::dir_ls(path = data_folder_root,
                         type = "directory",
                         recurse = FALSE)

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
    version = versions
  )

  # Create list of lkups

  lkups <- list(svy_lkup       = svy_lkup,
                ref_lkup       = ref_lkup,
                dist_stats     = dist_stats,
                pop_region     = pop_region,
                query_controls = query_controls,
                data_root      = data_folder_root)

  return(lkups)


}
