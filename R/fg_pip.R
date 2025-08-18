#' Compute imputed year stats
#'
#' Compute the main PIP poverty and inequality statistics for imputed years.
#'
#' @inheritParams pip
#' @return data.frame
#' @keywords internal
fg_pip <- function(country,
                   year,
                   povline,
                   popshare,
                   welfare_type,
                   reporting_level,
                   ppp,
                   lkup) {

  valid_regions       <- lkup$query_controls$region$values
  interpolation_list  <- lkup$interpolation_list
  data_dir            <- lkup$data_root
  refy_lkup           <- lkup$refy_lkup # cleaned refy table, unique by country-years but some columns removed in order to do that

  cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")
  # fg_pip is called from multiple places like pip, pip_grp_logic. We have connection object created
  # when calling from `pip`. For other functions we create it here.
  # if (is.null(con)) {
  #   cache_file_path <- fs::path(lkup$data_root, 'cache', ext = "duckdb")
  #   con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = cache_file_path, read_only = TRUE)
  # }

  # Handle interpolation
  metadata <- subset_lkup(
    country         = country,
    year            = year,
    welfare_type    = welfare_type,
    reporting_level = reporting_level,
    lkup            = refy_lkup, # only place this is used, for 'interpolation_id'
    valid_regions   = valid_regions,
    data_dir        = data_dir,
    povline         = povline,
    cache_file_path = cache_file_path,
    fill_gaps       = TRUE)


  data_present_in_master <- metadata$data_present_in_master
  povline  <- metadata$povline
  metadata <- metadata$lkup
  # Remove aggregate distribution if popshare is specified
  # TEMPORARY FIX UNTIL popshare is supported for aggregate distributions
  metadata <- filter_lkup(metadata = metadata,
                          popshare = popshare)
  setDT(metadata)

  # Return empty dataframe if no metadata is found (i.e. all in cache)
  if (nrow(metadata) == 0) {
    print("ZP: no metadata - i.e. nothing additional to estimate")
    return(list(main_data     = pipapi::empty_response_fg,
                data_in_cache = data_present_in_master))
  }

  full_list <- create_full_list(country                = country,
                                year                   = year,
                                refy_lkup              = refy_lkup,
                                data_present_in_master = data_present_in_master)

  lt <-
    pipdata::load_list_refy(input_list = full_list,
                            path = fs::path(data_dir,
                                            "lineup_data"))

  lt <- lapply(lt,
               FUN = \(x) {
                  x <- x |>
                   pipdata::attr_to_column("reporting_level_rows") |> # only rep level????
                   pipdata::attr_to_column("country_code") |>
                   pipdata::attr_to_column("reporting_year") |>
                    pipdata::attr_to_column("mean", dist_stats = TRUE) |>
                    pipdata::attr_to_column("median", dist_stats = TRUE) |>
                   fmutate(file = paste0(country_code,
                                         "_",
                                         reporting_year))

                  x
               })

  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "pipdata_list",
  #                 value = lt)

  # ZP Add: do fgt estimations using `res <- lapply(lt, process_dt, povline = povline)`
  #-------------------------
  res <- lapply(lt,
                process_dt,
                povline      = povline,
                mean_and_med = TRUE)
  res <- rbindlist(res,
                   fill = TRUE)

  # # TO BE REMOVED, ONLY FOR TESTING!!!
  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "res_povest",
  #                 value = res)

  # ZP Add: join to dist_stats
  #-------------------------
  # ly_dist <- lkup$lineup_dist_stats
  # res <- res |>
  #   joyn::left_join(y            = ly_dist,
  #                   by           = c("file",
  #                                    "reporting_level",
  #                                    "reporting_year"),
  #                   relationship = "one-to-one",
  #                   reportvar    = FALSE,
  #                   verbose      = FALSE)
  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "res_dist",
  #                 value = res)

  # ZP Add: join to metadata
  #-------------------------
  metadata[,
           file := basename(path)]
  print(metadata)

  # TO BE REMOVED, ONLY FOR TESTING!!!
  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "metadata_check2",
  #                 value = metadata)
  # metadata <- copy(metadata_check2)
  #stop("---------------")
  # try metadata unique code
  tmp_metadata <- metadata
  # Handle multiple distribution types (for aggregated distributions)
  if (length(unique(tmp_metadata$distribution_type)) > 1) {
    tmp_metadata[, distribution_type := "mixed"]
  }
  # convert survey_comparability to NA
  # NOTE: This should not be necessary. for the new lineup distribution
  # metadata should come without this variable.
  tmp_metadata[, survey_comparability := NA]
  # get all vars
  meta_vars <- setdiff(names(tmp_metadata), "reporting_year")
  # transform to NA when necessary - i.e. when interpolated (two rows per reporting_year)
  tmp_metadata[, (meta_vars) := lapply(.SD, \(x) {
    if (uniqueN(x) == 1) {
      x
    } else {
      NA
    }}),
    by      = c("reporting_year", "country_code", "reporting_level", "welfare_type"),
    .SDcols = meta_vars]

  # Remove duplicate rows by reporting_year (keep only one row per
  # reporting_year)
  tmp_metadata_unique <- funique(tmp_metadata)
  tmp_metadata_unique[,
                      file := paste0(country_code,
                                     "_",
                                     reporting_year)]
  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "tmp_metadata_unique_check",
  #                 value = tmp_metadata_unique)
  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "tmp_metadata_unique_check2",
  #                 value = tmp_metadata_unique)
  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "res_final",
  #                 value = res)

  out <- join(res,
              tmp_metadata_unique,
              on            = c("file",
                                "reporting_level"),
              how           = "left", # ZP: change from full to left,
                                      #  this rm nowcast years - i.e. years not included
                                      #  as lineup years
              validate      = "m:1",
              drop.dup.cols = TRUE,
              verbose       = 0)

  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "out_check",
  #                 value = out)
  out[, `:=`(
    file   = NULL
  )]

  setnames(out,
           "povline",
           "poverty_line")

  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "out1",
  #                 value = out)
  # Ensure that out does not have duplicates
  out <- fg_remove_duplicates(out)
  # rlang::env_poke(env   = globalenv(),
  #                 nm    = "out2",
  #                 value = out)

  # Fix issue with rounding of poverty lines
  out[,
      poverty_line := round(poverty_line, digits = 3) ]

  # Formatting. MUST be done in data.table tom modify by reference
  out[, path := as.character(path)]

  if ("max_year" %in% names(out)) {
    out[, max_year := NULL]
  }

  # in_cache <- data_present_in_master |>
  #   fmutate(file = paste(country_code, reporting_year, sep = "_")) |>
  #   fselect(file) |>
  #   reg_elem()
  # # rm rows in out that are in cache
  # out <- out[!(file %in% in_cache)]

  return(list(main_data     = out,
              data_in_cache = data_present_in_master))

}


#' Remove duplicated rows created during the interpolation process
#'
#' @param df data.table: Table of results created in `fg_pip()`
#' @param cols character: Columns with potential duplicate values
#'
#' @return data.table
#'
fg_remove_duplicates <- function(df,
                                 cols = c("comparable_spell",
                                          "cpi",
                                          "display_cp",
                                          "gd_type",
                                          # "interpolation_id",
                                          "path",
                                          "predicted_mean_ppp",
                                          "survey_acronym",
                                          "survey_comparability",
                                          "survey_coverage",
                                          "survey_id",
                                          "survey_mean_lcu",
                                          "survey_mean_ppp",
                                          "survey_median_lcu",
                                          "survey_median_ppp",
                                          "survey_time",
                                          "survey_year",
                                          "surveyid_year")) {
  # not all cols need to be changes
  cols <- setdiff(cols,
                  colnames(df))
  # Modify cache_id
  # * Ensures that cache_id is unique for both extrapolated and interpolated surveys
  # * Ensures that cache_id can be kept as an output of fg_pip() while still removing duplicated rows
  # df$cache_id <- fg_standardize_cache_id(cache_id = df$cache_id,
  #                                        interpolation_id = df$data_interpolation_id,
  #                                        reporting_level = df$reporting_level)
  # Set collapse vars to NA (by type)
  df <- fg_assign_nas_values_to_dup_cols(df   = df,
                                         cols = cols)

  # Ensure that out does not have duplicates
  df <- unique(df)

  return(df)
}

#' Standardize cache_id format to avoid duplication of rows
#'
#' @param cache_id character
#' @param interpolation_id character
#' @param reporting_level character
#'
#' @return character
fg_standardize_cache_id <- function(cache_id,
                                    interpolation_id,
                                    reporting_level) {

  out <- ifelse(grepl("|", interpolation_id, fixed = TRUE),
                gsub(paste0("_",
                            unique(reporting_level),
                            collapse = '|'),
                     '',
                     interpolation_id),
                cache_id)
  return(out)
}

#' Coerce variable causing potential duplicates to NAs
#'
#' @inheritParams fg_remove_duplicates
#'
#' @return data.table
fg_assign_nas_values_to_dup_cols <- function(df,
                                             cols) {
  #Classes are maintained by default.
  df[, (cols) := NA]
  return(df)
}






#' Create full list for fg data load, not including country-years in cache
#'
#' @param country Country selected in [fg_pip] function
#' @param year Year/s selected in [fg_pip] function
#' @param refy_lkup reference year lkup table with full lineups and new method
#' @param data_present_in_master cache data
#'
#' @return list
create_full_list <- function(country, year, refy_lkup, data_present_in_master) {

  if (!is.null(data_present_in_master)) {
    data_not_in_cache <-
      joyn::anti_join(x         =  refy_lkup,
                      y         =  data_present_in_master,
                      by        = c("country_code", "reporting_year"),
                      reportvar = FALSE,
                      verbose   = FALSE)
  } else {
    data_not_in_cache <- refy_lkup
  }

  # Extract unique combinations of country-year
  if (any(c("ALL", "WLD") %in% country)) {
    cntry <- data_not_in_cache$country_code |>
      funique()
  } else {
    cntry <- data_not_in_cache[country_code %in% country,
    ]$country_code |>
      funique()
  }
  if (any(c("ALL") %in% year)) {
    yr <- data_not_in_cache$reporting_year |>
      unique()
  } else {
    yr <- data_not_in_cache[reporting_year %in% year,
    ]$reporting_year |>
      funique()
  }
  dtemp <-
    data_not_in_cache |>
    fsubset(country_code     %in% cntry &
              reporting_year %in% yr) |>
    fsubset(reporting_year %in% lkup$valid_years$lineup_years) |>
    fselect(country_code,
            year = reporting_year) |>
    funique()

  # Split years by country
  full_list <- dtemp[,
                     .(year = list(year)),
                     by = country_code][
                       , .(country_code, year = year)
                     ]

  # Convert to desired structure
  full_list <- list(
    country_code = full_list$country_code,
    year         = lapply(full_list$year,
                          as.numeric))


  full_list

}
