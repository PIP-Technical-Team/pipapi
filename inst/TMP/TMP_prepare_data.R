dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER")
dir <- paste0(dir, "/20210401/")

# Add display_cp / show_portal --------------------------------------------

pfw <- pipaux::load_aux("pfw")
prod_syv <- fst::read_fst(paste0(dir, "estimations/prod_svy_estimation.fst"))
prod_ref <- fst::read_fst(paste0(dir, "estimations/prod_ref_estimation.fst"))

cols <- c("country_code", "surveyid_year", "survey_acronym",
          "welfare_type", "survey_coverage")

# Subset PFW columns
pfw <- pfw[, c( cols, "show_portal")]
names(pfw)[ncol(pfw)] <- "display_cp"

# Merge datasets
prod_syv <- merge(prod_syv, pfw, by = cols, all.x = TRUE)
prod_ref <- merge(prod_ref, pfw, by = cols, all.x = TRUE)

# Save datasets
fst::write_fst(prod_syv, paste0(dir, "estimations/prod_svy_estimation.fst"))
fst::write_fst(prod_ref, paste0(dir, "estimations/prod_ref_estimation.fst"))

