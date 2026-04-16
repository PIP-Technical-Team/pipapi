# Load basics ----------

devtools::load_all(".")
library(fastverse)
data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path()


lkups <- create_versioned_lkups(
  data_dir = data_dir,
  vintage_pattern = "20260430_2021_01_02_INT"
)
lkup <- lkups$versions_paths[[lkups$latest_release]]


# lkup <-  lkups$versions_paths$`20230328_2011_02_02_PROD`
# lkupp <-  lkups$versions_paths$`20230919_2017_01_02_PROD`
# lkupt <-  lkups$versions_paths$`20240109_2017_01_02_TEST`

# first testing -------------
options(pipapi.query_live_data = TRUE)
getOption("pipapi.query_live_data")


cl <- pip(country = "ALL", lkup = lkup, fill_gaps = FALSE)

fg <- pip(country = "ALL", lkup = lkup, fill_gaps = TRUE)

wb <- pip_agg(group_by = "wb", lkup = lkup, povline = 3)

ago <- pip(country = "AGO", lkup = lkup, fill_gaps = TRUE)


# cache most common queries --------------
options(pipapi.query_live_data = FALSE)
Sys.setenv(PIP_CACHE_LOCAL_KEY = 'abc', PIP_CACHE_SERVER_KEY = 'abc')
pipapi:::reset_cache(lkup = lkup)
povlines <- get_aux_table(lkup$data_root, "poverty_lines") |>
  _[, poverty_line]


cl <- pip(
  country = "ALL",
  lkup = lkup,
  fill_gaps = FALSE,
  povline = povlines
)


fg <- pip(country = "ALL", lkup = lkup, fill_gaps = TRUE, povline = povlines)

wb <- pip_agg(group_by = "wb", lkup = lkup, povline = povlines)
pushoverr::pushover("finished inter caching")

# Debugging --------------

# remove columns where all obs ar NAs

ttt <- pip(country = "MWI", lkup = lkup, fill_gaps = FALSE, povline = 3)
head(ttt)


qqq <- pip(country = "MWI", lkup = lkup, fill_gaps = TRUE, povline = 3)
head(qqq)


dt <- dt[, .SD, .SDcols = \(x) !all(is.na(x))]

dt[is.na(median), .(country_code, reporting_year)]


wb <- pip_agg(group_by = "wb", lkup = lkup)


wb <- pip_agg(
  group_by = "wb",
  lkup = lkup,
  additional_ind = TRUE,
  country = "ECA"
)

dt <- pip("VEN", lkup = lkup, additional_ind = TRUE)


dt <- pip("COL", lkup = lkup)
dt <- pip("COL", lkup = lkup, additional_ind = TRUE)


dt <- pip("VEN", lkup = lkup, fill_gaps = TRUE)


dd <- ui_cp_charts(
  country = "CHN",
  povline = 2.15,
  lkup = lkup
)


# default parameters]

lp <- list(
  year = c(1990, 2000),
  povline = 2.25,
  group_by = c("wb"),
  welfare_type = c("all"),
  reporting_level = c("all"),
  debug = FALSE,
  censor = TRUE,
  lkup = lkup
)


# devtools::load_all(".")

# setup -------------
library(fastverse)
withr::local_envvar(c("PIPAPI_APPLY_CACHING" = FALSE))
Sys.getenv("PIPAPI_APPLY_CACHING")


lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))

lkups$versions

latest <- lkups$latest_release

latest
lkup <- lkups$versions_paths[[latest]]


# start API -----------------------
# start_api(port =  8091)

# Aux tables  -----------
svy <- get_aux_table(data_dir = lkup$data_root, table = "spr_svy")

lnp <- get_aux_table(data_dir = lkup$data_root, table = "spr_lnp")

npl <- get_aux_table(data_dir = lkup$data_root)


# PIP ------------
debugonce(pip)

## survey years -------------
dt <- pip("PRY", 2018, lkup = lkup, additional_ind = FALSE)

y <- 2018
dt <- pip("IND", y, lkup = lkup, fill_gaps = FALSE)

dt[, .(reporting_level, median, spl, spr)]


## lineup years ----------------
dt <- pip("IND", y, lkup = lkup, fill_gaps = TRUE)
dt[, .(reporting_level, median, spl, spr)]


dt <- pip("COL", lkup = lkup, additional_ind = TRUE)


# PIP GRP ----------------

dtp <- pip_grp_logic(country = "LAC", lkup = lkupp, povline = 2.15)
setDT(dtp)


dtt <- pip_grp_logic(country = "LAC", lkup = lkupt, povline = 2.15)
setDT(dtt)


fs::path(tdire, "lac_test", ext = "fst") |>
  fst::write_fst(dtt, path = _)


dq <- pip_grp_logic(
  lkup = lkup,
  group_by = "wb",
  povline = 2.15,
  reporting_level = "national",
  year = 2018,
  country = "WLD"
)

dq <- pip_grp(
  country = "all",
  year = 2010,
  group_by = "wb",
  povline = 1.9,
  lkup = lkups
)
dq[]

dq <- pip_grp_logic(
  country = "all",
  year = 2010,
  group_by = "wb",
  povline = 1.9,
  lkup = lkup
)
dq[]

dq <- pip_grp(
  country = "all",
  year = 2010,
  group_by = "wb",
  povline = 1.9,
  lkup = lkup
)
dq[]


# Constants
lkups <- create_versioned_lkups(Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL"))
lkup2 <- lkups$versions_paths[[lkups$latest_release]]
censored <-
  test_path("testdata", "/censored.rds") |>
  readRDS()

out_pip <- pip(
  country = "all",
  year = 2010,
  group_by = "wb",
  povline = 1.9,
  lkup = lkup2
)

out_pip_grp <- pip_grp(
  country = "all",
  year = 2010,
  group_by = "wb",
  povline = 1.9,
  lkup = lkup2
)


dq |>
  fgroup_by(region_code) |>
  fselect(reporting_year) |>
  fmax()


dq |>
  fsubset(region_code == "WLD" & reporting_year == 2018) |>
  fselect(headcount)


ui <- ui_pc_regional(lkup = lkup, povline = 2.15)

ui |>
  fgroup_by(region_code) |>
  fselect(reporting_year) |>
  fmax()


ui |>
  fsubset(region_code == "WLD" & reporting_year == 2018) |>
  fselect(headcount)


dt <- pip(lkup = lkup, povline = 2.15)

dt |>
  fsubset(round(poverty_line, 12) == round(spl, 12) & headcount != spr) |>
  fselect(
    country_code,
    reporting_year,
    reporting_level,
    headcount,
    spr,
    distribution_type
  )


dtf <- pip(lkup = lkup, povline = 2.15, fill_gaps = TRUE)


debugonce(pip)
debugonce(ag_average_poverty_stats)
chn <- pip(lkup = lkup, country = "CHN", povline = 2.15, year = 1993)


de <-
  lkup$svy_lkup[country_code == "BDI" & reporting_year == 1998, path] |>
  fst::read_fst()

ps <- de$welfare < 322.6106
fmean(ps, w = de$weight)
