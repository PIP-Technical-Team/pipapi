library(readr)
library(fst)
library(dplyr)
library(data.table)

# Globals -----------------------------------------------------------------
data_folder_root <- sub('[/]20210401', '', Sys.getenv('PIPAPI_DATA_ROOT_FOLDER'))

v1 <- "20210401/"
v0 <- "00010101/"


# Decomposition master ----------------------------------------------------

tmp <- read_csv(paste0(data_folder_root, v1, "_aux/decomposition_master.csv"))
fst::write_fst(tmp, paste0(data_folder_root, v1, "_aux/decomposition_master.fst"))
fst::write_fst(tmp, paste0(data_folder_root, v0, "_aux/decomposition_master.fst"))

