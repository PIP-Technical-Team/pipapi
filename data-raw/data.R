# library(pipapi)
# lkups <- pipapi::create_versioned_lkups(Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))
pkgload::load_all()

data_dir <-
  if (Sys.info()["user"] == "wb384996") {
    Sys.getenv('PIPAPI_DATA_ROOT_FOLDER_SERVER')
  } else {
    Sys.getenv('PIPAPI_DATA_ROOT_FOLDER_LOCAL')
  }

fs::dir_tree(data_dir, recurse = 0)

latest_version <-
  available_versions(data_dir) |>
  max()

lkups <- create_versioned_lkups(data_dir,
                                vintage_pattern = latest_version)
lkup <- lkups$versions_paths[[lkups$latest_release]]

ctr  <- "AGO"
year <- 2000


empty_response <- pip(ctr, year, lkup = lkup)[-1]
reporting_level_list <- c("national", "rural", "urban")

tmp <- ui_cp_charts(country = ctr, povline = 1.9, lkup = lkup)
tmp1 <- tmp[[ctr]]$pov_charts[[1]]$pov_trend[-c(1:3)]
tmp2 <- tmp[[ctr]]$pov_charts[[1]]$pov_mrv[-c(1:11)]
empty_response_cp_poverty <- list(pov_trend = tmp1, pov_mrv = tmp2)

empty_response_grp <- pip_grp("all", year, lkup = lkup, group_by = "wb")
empty_response_grp <- empty_response_grp[-c(1:nrow(empty_response_grp))]


empty_response_fg <- structure(list(country_code = character(0), survey_id = character(0),
               cache_id = character(0), wb_region_code = character(0), reporting_year = numeric(0),
               surveyid_year = character(0), survey_year = numeric(0), survey_time = character(0),
               survey_acronym = character(0), survey_coverage = character(0),
               survey_comparability = numeric(0), comparable_spell = character(0),
               welfare_type = character(0), reporting_level = character(0),
               survey_mean_lcu = numeric(0), survey_mean_ppp = numeric(0),
               survey_median_ppp = numeric(0), survey_median_lcu = numeric(0),
               predicted_mean_ppp = numeric(0), ppp = numeric(0), cpi = numeric(0),
               reporting_pop = numeric(0), reporting_gdp = numeric(0), reporting_pce = numeric(0),
               pop_data_level = character(0), gdp_data_level = character(0),
               pce_data_level = character(0), cpi_data_level = character(0),
               ppp_data_level = character(0), distribution_type = character(0),
               gd_type = character(0), is_interpolated = logical(0), is_used_for_line_up = logical(0),
               is_used_for_aggregation = logical(0), estimation_type = character(0),
               interpolation_id = character(0), display_cp = numeric(0),
               country_name = character(0), africa_split = character(0),
               africa_split_code = character(0), region_name = character(0),
               region_code = character(0), world = character(0), world_code = character(0),
               path = character(0), data_interpolation_id = character(0),
               poverty_line = numeric(0), mean = numeric(0), median = numeric(0),
               headcount = numeric(0), poverty_gap = numeric(0), poverty_severity = numeric(0),
               watts = numeric(0)), row.names = integer(0), class = "data.frame")

usethis::use_data(
  empty_response,
  empty_response_cp_poverty,
  reporting_level_list,
  empty_response_grp,
  empty_response_fg,
  overwrite = TRUE
)
