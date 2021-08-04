library(readr)
library(fst)
library(dplyr)

# Globals -----------------------------------------------------------------
data_folder_root <- sub('[/]20210401', '', Sys.getenv('DATA_FOLDER_ROOT'))

v1 <- "20210401/"
v0 <- "00010101/"


# Indicators master -------------------------------------------------------
tmp <- read_csv(paste0(data_folder_root, v1, "_aux/indicators_master.csv"))
fst::write_fst(tmp, paste0(data_folder_root, v1, "_aux/indicators_master.fst"))
fst::write_fst(tmp, paste0(data_folder_root, v0, "_aux/indicators_master.fst"))

# Countries master ----
tmp <- fst::read_fst(paste0(data_folder_root, v1, "_aux/countries.fst")) %>%
  distinct() %>%
  select(region_code, country_code, country_name) %>%
  arrange(region_code, country_code)

class <- readxl::read_excel(paste0(data_folder_root, v1, "_aux/income_classification.xlsx"),
                            sheet = "List of economies") %>%
  janitor::clean_names() %>%
  select(country_code = code, income_group)

tmp <- tmp %>%
  left_join(class, by = "country_code")
tmp$iso2_code <- countrycode::countrycode(tmp$country_code, origin = "iso3c", destination = "iso2c")

fst::write_fst(tmp, paste0(data_folder_root, v1, "_aux/countries.fst"))
fst::write_fst(tmp, paste0(data_folder_root, v0, "_aux/countries.fst"))

# Decomposition master ----------------------------------------------------
tmp <- read_csv(paste0(data_folder_root, v1, "_aux/decomposition_master.csv"))
fst::write_fst(tmp, paste0(data_folder_root, v1, "_aux/decomposition_master.fst"))
fst::write_fst(tmp, paste0(data_folder_root, v0, "_aux/decomposition_master.fst"))


# Poverty line master ----------------------------------------------------
tmp <- read_csv(paste0(data_folder_root, v1, "_aux/poverty_lines.csv"))
tmp$name <- as.character(tmp$name)
fst::write_fst(tmp, paste0(data_folder_root, v1, "_aux/poverty_lines.fst"))
fst::write_fst(tmp, paste0(data_folder_root, v0, "_aux/poverty_lines.fst"))


# Regions master ----------------------------------------------------------

# tmp <- fst::read_fst(paste0(data_folder_root, v1, "_aux/pop-region.fst")) %>%
#   select(-pop, -year) %>%
#   distinct()
# write_csv(tmp, paste0(data_folder_root, v1, "_aux/regions.csv"))

tmp <- read_csv(paste0(data_folder_root, v1, "_aux/regions.csv"))

fst::write_fst(tmp, paste0(data_folder_root, v1, "_aux/regions.fst"))

# rgn_lkup <- fst::read_fst("TEMP/country_list.fst") %>%
#   select(pcn_region_code, region = region) %>%
#   distinct()


# Add comparable spell to survey means dataset -----------------------------

tmp <- fst::read_fst(paste0(data_folder_root, v1, "estimations/survey_means.fst"))
dl <- split(tmp, list(x$country_code, x$survey_comparability))
dl <- lapply(dl, function(x) {
  if (nrow(x) == 1) {
    x$comparable_spell <- x$reporting_year
  } else {
    x$comparable_spell <-
      sprintf('%s - %s',
              x$reporting_year[1],
              x$reporting_year[length(x$reporting_year)]
      )
  }
  return(x)
})
tmp <- data.table::rbindlist(dl)
fst::write_fst(tmp, paste0(data_folder_root, v1, "estimations/survey_means.fst"))


# Add comparable spell to interpolated means dataset -----------------------------

tmp <- fst::read_fst(paste0(data_folder_root, v1, "estimations/interpolated_means.fst"))
dl <- split(tmp, list(x$country_code, x$reporting_year, x$survey_comparability))
dl <- lapply(dl, function(x) {
  if (nrow(x) == 1) {
    x$comparable_spell <- x$reporting_year
  } else {
    x$comparable_spell <-
      sprintf('%s - %s',
              x$reporting_year[1],
              x$reporting_year[length(x$reporting_year)]
      )
  }
  return(x)
})
tmp <- data.table::rbindlist(dl)
fst::write_fst(tmp, paste0(data_folder_root, v1, "estimations/interpolated_means.fst"))
