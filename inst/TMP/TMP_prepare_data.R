# remotes::install_github('PIP-Technical-Team/pipdm@master')
# library(pipdm)
# devtools::load_all('../pipdm')
library(data.table)
library(magrittr)

dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER")
dir <- paste0(dir, "/20210401/")

# ---- Fix lkup tables ----

# # Fix tables so that upstream bug fixes are reflected in the outputs
#
# # Helper functions
# add_is_aggregation <- function(dt) {
#   # Add is_used_for_aggregation column
#   dt[, n_rl := .N, by = cache_id]
#   dt[, is_used_for_aggregation := ifelse((dt$reporting_level %in% c("urban", "rural") & dt$n_rl == 3), TRUE, FALSE)]
#   dt$n_rl <- NULL
#   return(dt)
# }
#
# add_is_line_up <- function(dt){
#
#   assertthat::assert_that(all(c("cache_id", "country_code", "reporting_level") %in% names(dt)))
#
#   # Countries wo/ any national (reporting level) surveys
#   cc <- pipdm:::check_no_national_survey(dt)
#
#   # Create number of rows per cache_id
#   dt[, n_rl := .N, by = cache_id]
#
#   # Create check
#   check <- (dt$reporting_level == "national" & dt$n_rl == 1) |  # Surveys w/ national reporting level and no split by U/R domain (e.g USA)
#     (dt$reporting_level %in% c("urban", "rural") & dt$n_rl == 3) | # Surveys split by U/R domain (e.g. CHN, IND)
#     dt$country_code %in% cc  # Countries wo/ any national surveys (e.g. ARG, SUR)
#
#   # Add is_used_for_line_up
#   dt$is_used_for_line_up <- ifelse(check, TRUE, FALSE)
#   dt$n_rl <- NULL
#
#   return(dt)
# }
#
# # Load AUX data
# pfw_table <- pipaux::load_aux("pfw", maindir = "Y:/PIP-Data_QA/") %>% setDT()
# gdp_table <- pipaux::load_aux("gdp", maindir = "Y:/PIP-Data_QA/") %>% setDT()
# pce_table <- pipaux::load_aux("pce", maindir = "Y:/PIP-Data_QA/") %>% setDT()
# pop_table <- pipaux::load_aux("pop", maindir = "Y:/PIP-Data_QA/") %>% setDT()
# cl_table <- pipaux::load_aux("country_list", maindir = "Y:/PIP-Data_QA/") %>% setDT()
# incgrp_table <- pipaux::load_aux("income_groups", maindir = "Y:/PIP-Data_QA/") %>% setDT()
# censored <- pipaux::load_aux("censoring", maindir = "Y:/PIP-Data_QA/")
#
# # Load estimation tables
# svy_mean <- fst::read_fst(paste0(dir, "estimations/survey_means.fst")) %>% setDT()
# dist_stats <- fst::read_fst(paste0(dir, "estimations/dist_stats.fst")) %>% setDT()
#
# # ARG 1980 fix
# svy_mean[country_code == "ARG" & reporting_year == 1980]$reporting_level <- "urban"
# svy_mean[country_code == "ARG" & reporting_year == 1980]$pop_data_level <- "urban"
# dist_stats[country_code == "ARG" & reporting_year == 1980]$reporting_level <- "urban"
# dist_stats[country_code == "ARG" & reporting_year == 1980]$pop_data_level <- "urban"
#
# # Lineup fixes
# svy_mean <- add_is_line_up(svy_mean)  # Add is_used_for_line_up column
# svy_mean <- add_is_aggregation(svy_mean)  # Modify is_used_for_aggregation column
#
# # Create ref year table
# ref_year_table <- db_create_ref_year_table(
#   dsm_table = svy_mean,
#   gdp_table = gdp_table,
#   pce_table = pce_table,
#   pop_table = pop_table,
#   ref_years = 1981:2021,
#   pip_years = 1977:2019
# )
#
# # Fix NRU region code
# # Inconsistency between PFW and PCN masterfile
# ref_year_table[country_code == "NRU",]$pcn_region_code <- "EAP"
#
# # Fix ARG 1980 which is incorrectly coded
# ref_year_table[country_code == "ARG",]$pop_data_level <- "urban"
# ref_year_table[country_code == "ARG",]$reporting_level <- "urban"
#
# # Create coverage list of tables
# coverage_list <- db_create_coverage_table(
#   ref_year_table = ref_year_table,
#   incgrp_table = incgrp_table,
#   cl_table = cl_table,
#   pop_table = pop_table,
#   ref_years = 1981:2021
# )
#
# # Create regional censoring table
# censored <- db_create_censoring_table(
#   censored = censored,
#   coverage_list = coverage_list,
#   coverage_threshold = 50)
#
# # Create new PROD estimation tables
# svy_est <- db_create_svy_estimation_table(
#   svy_mean, dist_stats, gdp_table = gdp_table, pce_table = pce_table)
# ref_est <- db_create_ref_estimation_table(ref_year_table, dist_table = dist_stats)
#
#
# # Save datasets
# fst::write_fst(svy_est, paste0(dir, "estimations/prod_svy_estimation.fst"))
# fst::write_fst(ref_est, paste0(dir, "estimations/prod_ref_estimation.fst"))
# fst::write_fst(dist_stats, paste0(dir, "estimations/dist_stats.fst"))
# saveRDS(censored, paste0(dir, "estimations/censored.RDS"))




# ---- Add survey_time ----

# Load AUX data
pfw_table <- pipaux::load_aux("pfw", maindir = "Y:/PIP-Data_QA/") %>% setDT()
pfw_table <- pfw_table[, c('country_code', 'reporting_year', 'welfare_type',
              'survey_coverage', 'survey_acronym', 'survey_time')]

# Load estimation tables
prod_svy <- fst::read_fst(paste0(dir, "estimations/prod_svy_estimation.fst")) %>% setDT()
prod_ref <- fst::read_fst(paste0(dir, "estimations/prod_ref_estimation.fst")) %>% setDT()

prod_svy <- merge(prod_svy, pfw_table,
      by = c('country_code', 'reporting_year', 'welfare_type',
             'survey_coverage', 'survey_acronym'),
      all.x = TRUE)

prod_ref <- merge(prod_ref, pfw_table,
                  by = c('country_code', 'reporting_year', 'welfare_type',
                         'survey_coverage', 'survey_acronym'),
                  all.x = TRUE)


fst::write_fst(prod_svy, paste0(dir, "estimations/prod_svy_estimation.fst"))
fst::write_fst(prod_ref, paste0(dir, "estimations/prod_ref_estimation.fst"))

# ---- Remove countries wo/ complete GDP series ----

prod_ref <- fst::read_fst(paste0(dir, "estimations/prod_ref_estimation.fst")) %>% setDT()
codes <- c('FSM', 'NRU', 'SOM', 'SSD', 'SVK', 'TLS', 'TUV', 'VEN', 'XKX')
prod_ref <- prod_ref[!country_code %in% codes]
fst::write_fst(prod_ref, paste0(dir, "estimations/prod_ref_estimation.fst"))

# ---- Create timestamp file ----

# dir <- paste0(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"), "/20210401/")
writeLines(as.character(Sys.time()), paste0(dir, "data_update_timestamp.txt"))

