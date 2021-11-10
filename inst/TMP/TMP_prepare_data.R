library(data.table)
library(magrittr)

dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER")
dir <- paste0(dir, "/20210401/")

# Add display_cp / show_portal --------------------------------------------

pfw <- pipaux::load_aux("pfw")
prod_syv <- fst::read_fst(paste0(dir, "estimations/prod_svy_estimation.fst"))
prod_ref <- fst::read_fst(paste0(dir, "estimations/prod_ref_estimation.fst"))

cols <- c("country_code", "surveyid_year", "survey_acronym",
          "welfare_type", "survey_coverage")

# Subset PFW columns
pfw <- pfw[, c( cols, "display_cp")]
# names(pfw)[ncol(pfw)] <- "display_cp"

# Merge datasets
prod_syv <- merge(prod_syv, pfw, by = cols, all.x = TRUE)
prod_ref <- merge(prod_ref, pfw, by = cols, all.x = TRUE)

# Save datasets
fst::write_fst(prod_syv, paste0(dir, "estimations/prod_svy_estimation.fst"))
fst::write_fst(prod_ref, paste0(dir, "estimations/prod_ref_estimation.fst"))


# Create censor rows dataset ----------------------------------------------

coverage <- fst::read_fst(paste0(dir, "_aux/coverage.fst"), as.data.table = TRUE)
coverage <- coverage[!is.na(pcn_region_code)] # Why is there NA observations?
coverage <- coverage[coverage < 50]
coverage %>% data.table::setnames('pcn_region_code', 'region_code')
coverage$statistic <- 'all'
coverage$id <- with(coverage, sprintf('%s_%s', region_code, reporting_year))
coverage <- coverage[,c('region_code', 'reporting_year', 'statistic')]

censor_DEU <-
  lkups$versions_paths$latest_release$svy_lkup[(country_code == 'DEU' & reporting_year < 1990)][,
    c('country_code', 'survey_acronym', 'reporting_year', 'reporting_level', 'welfare_type')]
censor_DEU$statistic <- 'all'
censor_DEU$id <- with(censor_DEU,
  sprintf(
    "%s_%s_%s_%s_%s",
    country_code, reporting_year,
    survey_acronym, welfare_type,
    reporting_level
  ))

censored <- list(
  region = coverage,
  country = censor_DEU
)

saveRDS(censored, paste0(dir, "_aux/censored.RDS"))

# ---- Create timestamp file ----

# dir <- paste0(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER"), "/20210401/")
writeLines(as.character(Sys.time()), paste0(dir, "data_update_timestamp.txt"))

