library(readr)
library(fst)
library(dplyr)
library(data.table)

# Globals -----------------------------------------------------------------
data_folder_root <- sub('[/]20210401', '', Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))

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


# Country profiles master --------------------------------------------------


tmpfiles <- list.files(paste0(data_folder_root, v1, '/_aux'),
                       pattern = 'indicator_values_country',
                       full.names = TRUE)
tmp <- lapply(tmpfiles, function(x) {
  x <- read.csv(x)
  x <- data.table::setDT(x)
  names(x) <- tolower(sub('xyzD[MCP]xyz', '', names(x)))
  x <- data.table::setnames(
    x, skip_absent = TRUE,
    c('country', 'requestyear', 'datayear', 'welfaretype',
      'coverage', 'interpolation', 'survname', 'comparability',
      'comparable_spell', 'povertyline', 'yearrange', 'si_pov_all_poor',
      'sp_pop_totl','si_pov_nahc', 'ny_gnp_pcap_cd', 'ny_gdp_mktp_kd_zg',
      'si_pov_gini', 'si_pov_theil', 'si_pov_all', 'si_pov_share_all',
      'si_mpm_educ', 'si_mpm_edue', 'si_mpm_elec', 'si_mpm_imps',
      'si_mpm_impw', 'si_mpm_mdhc', 'si_mpm_poor', 'si_spr_pcap_zg'
      ),
    c('country_code', 'reporting_year', 'survey_year', 'welfare_type',
      'survey_coverage','is_interpolated', 'survey_acronym',
      'survey_comparability', 'comparable_spell',
      'poverty_line', 'year_range', 'pop_in_poverty',
      'population', 'national_headcount', 'gni', 'gdp_growth',
      'gini', 'theil', 'headcount', 'poverty_share',
      'mpm_education_attainment', 'mpm_education_enrollment',
      'mpm_electricity', 'mpm_sanitation', 'mpm_water',
      'mpm_headcount', 'mpm_monetary', 'shared_prosperity'
      )
  )
  if (any(grepl('welfare_type', names(x)))) {
    x$welfare_type <- data.table::fifelse(
      x$welfare_type == 'CONS', 'consumption', 'income')
  }
  if (any(grepl('survey_coverage', names(x)))) {
    # Recode survey coverage
    x <- x[, `:=`(
      survey_coverage = data.table::fcase(
        survey_coverage == "N", "national",
        survey_coverage == "R", "rural",
        survey_coverage == "U", "urban",
        default = "")
    )]
  }
  return(x)
})
names(tmp) <- gsub('.*indicator_values_country_|[.]csv', '', tmpfiles)

# Create list of key indicators datasets
key_indicators <- merge(tmp$KI1, tmp$KI5_KI6_KI7, all = TRUE,
                        by = c('country_code', 'reporting_year'))
key_indicators <- merge(key_indicators,
                        tmp$chart5[, c('country_code', 'reporting_year', 'mpm_headcount')],
                        all = TRUE,
                        by = c('country_code', 'reporting_year'))
key_indicators <- list(
  national_headcount = key_indicators[, c('country_code', 'reporting_year', 'national_headcount')],
  mpm_headcount = key_indicators[, c('country_code', 'reporting_year', 'mpm_headcount')],
  population = key_indicators[, c('country_code', 'reporting_year', 'population')],
  gni = key_indicators[, c('country_code', 'reporting_year', 'gni')],
  gdp = key_indicators[, c('country_code', 'reporting_year', 'gdp_growth')]
)
key_indicators[1:3] <- lapply(key_indicators[1:3], function(x){
  x <- x %>%
    dplyr::filter(!is.na(x[,3])) %>%
    dplyr::group_by(country_code) %>%
    dplyr::filter(reporting_year == max(reporting_year)) %>%
    dplyr::ungroup() %>%
    data.table::as.data.table()
})
key_indicators[4:5] <- lapply(key_indicators[4:5], function(x){
  x <- x %>%
    dplyr::filter(!is.na(x[,3])) %>%
    dplyr::group_by(country_code) %>%
    dplyr::slice_tail(n = 2) %>%
    dplyr::ungroup() %>%
    data.table::as.data.table()
})

ki4 <- tmp$chart6_KI4[, c('country_code', 'year_range',
                          'distribution','shared_prosperity')]
ki4$year1 <- sapply(strsplit(ki4$year_range, '-'), function(x) x[[1]])
ki4$year2 <- sapply(strsplit(ki4$year_range, '-'), function(x) x[[2]])
ki4 <- ki4 %>%
  group_by(country_code) %>%
  dplyr::filter(distribution %in% c('b40', 'tot')) %>%
  dplyr::filter(year2 == max(year2)) %>%
  dplyr::ungroup() %>%
  dplyr::select(country_code, year_range,
                distribution, shared_prosperity) %>%
  data.table::as.data.table() %>%
  data.table::dcast(country_code + year_range ~ distribution,
                    value.var = 'shared_prosperity')

names(ki4)[3:4] <- c('share_below_40', 'share_total')
key_indicators <- append(key_indicators, list(shared_prosperity = ki4))

cp <- list(key_indicators = key_indicators, charts = NULL)
saveRDS(cp, paste0(data_folder_root, v1, "_aux/country_profiles.RDS"))

