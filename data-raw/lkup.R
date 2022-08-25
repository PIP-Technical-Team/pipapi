## ----setup----------------------------------------------------------------------
pkgload::load_all()
library(data.table)

create_fake_data <- function(ver, pr = 0.01 ) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {

    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####
  ## ----define-folders-------------------------------------------------------------
  sdir <-
    Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_SERVER") |>
    fs::path(ver)

  ldir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path(ver)


  ## ----copy-aux-estimations-------------------------------------------------------
  ## Auxiliary data

  fs::dir_copy(path     = fs::path(sdir, "_aux"),
               new_path = fs::path(ldir, "_aux"),
               overwrite = TRUE)


  ## Estimations data
  fs::dir_copy(path     = fs::path(sdir, "estimations"),
               new_path = fs::path(ldir, "estimations"),
               overwrite = TRUE)



  ## ----set-survey-data------------------------------------------------------------
  ## Survey data

  ssv_dir <- fs::path(sdir, "survey_data")
  lsv_dir <- fs::path(ldir, "survey_data")
  fs::dir_create(lsv_dir)

  sv_file_paths <-
    fs::dir_ls(ssv_dir,
               type = "file",
               regexp = "fst$") |>
    sort()


  sv_files <-
    fs::path_file(sv_file_paths) |>
    fs::path_ext_remove()

  names(sv_file_paths) <- sv_files



  ## ----survey-data, eval=FALSE----------------------------------------------------
  purrr::walk(sv_file_paths,
              ~{
                df <- fst::read_fst(.x, as.data.table = TRUE)

                if (nrow(df) < 100) {
                  dt <- copy(df)  # Group data.
                } else {
                  # Take pr for each area
                  dt <- df[, .SD[sample(.N, size = floor(.N*pr))], by = area]
                }

                filename <- fs::path_file(.x)

                npath <- fs::path(lsv_dir,filename)
                fst::write_fst(x = dt,
                               path = npath)
              })


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(TRUE)

}


# vers <- c("20220810_2017_01_02_TEST", "20220609_2011_02_02_PROD", "20220810_2017_01_02_PROD")
# purrr::map(vers, create_fake_data)


ver <- "20220810_2017_01_02_TEST"
data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL")
lkups <- create_versioned_lkups(data_dir = data_dir,
                                vintage_pattern = "TEST$")

lkup <-  lkups$versions_paths[[ver]]

usethis::use_data(lkup, overwrite = TRUE)

saveRDS(lkup, "tests/testthat/testdata/lkup.rds")


