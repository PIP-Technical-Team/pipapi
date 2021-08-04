library(readr)
library(fst)
library(dplyr)
library(haven)
library(readxl)

# Globals -----------------------------------------------------------------

data_folder_root <- sub('[/]20210401', '', Sys.getenv('DATA_FOLDER_ROOT'))
v1 <- "20210401/"
v0 <- "00010101/"

# Indicators master -------------------------------------------------------

tmp <- read_csv(paste0(data_folder_root, v1, "_aux/indicators_master.csv"))
fst::write_fst(tmp, paste0(data_folder_root, v1, "_aux/indicators_master.fst"))
fst::write_fst(tmp, paste0(data_folder_root, v0, "_aux/indicators_master.fst"))

# Countries master --------------------------------------------------------

tmp <- fst::read_fst(paste0(data_folder_root, v1, "_aux/countries.fst")) %>%
  distinct() %>%
  select(region_code, country_code, country_name) %>%
  arrange(region_code, country_code)

class <- readxl::read_excel(paste0(data_folder_root, v1,
                                   "_aux/income_classification.xlsx"),
                            sheet = "List of economies") %>%
  janitor::clean_names() %>%
  select(country_code = code, income_group)

tmp <- tmp %>%
  left_join(class, by = "country_code")
tmp$iso2_code <-
  countrycode::countrycode(tmp$country_code,
                           origin = "iso3c", destination = "iso2c")

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


# Metadata master ---------------------------------------------------------

survey_pfw <- haven::read_dta('TEMP/Survey_price_framework.dta')
metadata <- readxl::read_xlsx('TEMP/metadata_to_update_MN@3.xlsx')
survey_pfw$link <- with(survey_pfw,
                        sprintf('%s_%s_%s', code, year, survname))
# Merge datasets (inner join)
metadata <-
  merge(metadata, survey_pfw[c('code', 'year', 'survname', 'link')],
      by = 'link', all = FALSE)

# Rename cols
metadata <- metadata %>%
  dplyr::rename(country_code = code,
              surveyid_year = year,
              survey_acronym = survname)

# Select columns (as mentioned by Minh in email 29.07.2021)
metadata <- metadata[
  c('country_code', 'surveyid_year', 'survey_acronym',
    'title', 'year_start', 'year_end',
    'authoring_entity_name', 'abstract',
    'collection_dates_cycle', 'collection_dates_start',
    'collection_dates_end', 'coverage',
    'sampling_procedure', 'collection_mode',
    'coll_situation','weight','cleaning_operations')]

fst::write_fst(metadata,
               paste0(data_folder_root,
                      v1, "_aux/survey_metadata.fst"))
