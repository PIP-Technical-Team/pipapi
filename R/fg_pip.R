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
  ref_lkup            <- lkup$ref_lkup  # the normal refy table, some country-years have two rows (interpolation)
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

  # Return empty dataframe if no metadata is found
  if (nrow(metadata) == 0) {
    print("ZP: no metadata - i.e. nothing additional to estimate")
    return(list(main_data     = pipapi::empty_response_fg,
                data_in_cache = data_present_in_master))
  }


  # ZP Add: load refy data
  #-------------------------
  # Extract unique combinations of country-year
  # if (any(c("ALL", "WLD") %in% country)) {
  #   cntry <- refy_lkup$country_code |>
  #     unique()
  #   print("A")
  #   #cntry[!cntry %in% c("SSD", "SVK", "TLS", "VEN", "XKX")] # to be removed
  # } else {
  #   cntry <- refy_lkup[country_code %in% country,
  #                      .(country_code)] |>
  #     funique()
  #   print("B")
  # }
  # if (any(c("ALL") %in% year)) {
  #   yr <- refy_lkup$reporting_year |>
  #     unique()
  #   print("C")
  # } else {
  #   yr <- refy_lkup[reporting_year %in% year,
  #                      .(reporting_year)] |>
  #     funique()
  #   print("D")
  # }
  #
  # print(as.vector(cntry))
  # print(yr)
  # lt <-
  #   pipload::load_list_refy(input_list = list(country_code = cntry,
  #                                             year         = yr),
  #                           path = fs::path(data_dir,
  #                                           "lineup_data"))


  #'     # ZP Add: load refy data
  #-------------------------
  # Extract unique combinations of country-year
  if (any(c("ALL", "WLD") %in% country)) {
    cntry <- refy_lkup$country_code |>
      unique()
    print("A")
    #cntry[!cntry %in% c("SSD", "SVK", "TLS", "VEN", "XKX")] # to be removed
  } else {
    cntry <- refy_lkup[country_code %in% country,
                       .(country_code)] |>
      funique()
    print("B")
  }
  if (any(c("ALL") %in% year)) {
    yr <- refy_lkup$reporting_year |>
      unique()
    print("C")
  } else {
    yr <- refy_lkup[reporting_year %in% year,
                    .(reporting_year)] |>
      funique()
    print("D")
  }
  dtemp <-
    ref_lkup |>
    fsubset(country_code     %in% cntry &
              reporting_year %in% yr) |>
    fselect(country_code,
            year = reporting_year) |>
    funique()

  # Split years by country
  full_list <- dtemp[, .(year = list(year)), by = country_code][
    , .(country_code, year = year)
  ]

  # Convert to desired structure
  full_list <- list(
    country_code = full_list$country_code,
    year = lapply(full_list$year, as.numeric)
  )
  #return(full_list)
  print(as.vector(cntry))
  print(yr)
  lt <-
    pipload::load_list_refy(input_list = full_list,
                            path = fs::path(data_dir,
                                            "lineup_data"))



  print(names(lt))
  lt <- lapply(lt,
               FUN = \(x) {
                  x <- x |>
                   pipload::attr_to_column("reporting_level_rows") |> # only rep level????
                   pipload::attr_to_column("country_code") |>
                   pipload::attr_to_column("reporting_year") |>
                   pipload::attr_to_column("mean",
                                           dist_stats = TRUE) |>
                   pipload::attr_to_column("median",
                                           dist_stats = TRUE) |>
                   fmutate(file = paste0(country_code,
                                         "_",
                                         reporting_year))

                  x
               })

  rlang::env_poke(env   = globalenv(),
                  nm    = "pipload_list",
                  value = lt)

  # ZP Add: do fgt estimations using `res <- lapply(lt, process_dt, povline = povline)`
  #-------------------------
  res <- lapply(lt,
                process_dt,
                povline      = povline,
                mean_and_med = TRUE)
  res <- rbindlist(res,
                   fill = TRUE)

  # TO BE REMOVED, ONLY FOR TESTING!!!
  rlang::env_poke(env   = globalenv(),
                  nm    = "res_povest",
                  value = res)

  # ZP Add: join to metadata
  #-------------------------
  metadata[,
           file := basename(path)]
  # TO BE REMOVED, ONLY FOR TESTING!!!
  rlang::env_poke(env   = globalenv(),
                  nm    = "metadata_check",
                  value = metadata)
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
    by = reporting_year, .SDcols = meta_vars]

  # Remove duplicate rows by reporting_year (keep only one row per
  # reporting_year)
  tmp_metadata_unique <- unique(tmp_metadata, by = "reporting_year")
  tmp_metadata_unique[,
                      file := paste0(country_code,
                                     "_",
                                     reporting_year)]
  rlang::env_poke(env   = globalenv(),
                  nm    = "tmp_metadata_unique_check",
                  value = tmp_metadata_unique)

  out <- join(res,
              tmp_metadata_unique,
              on       = c("file",
                           "reporting_level"),
              how      = "full",
              validate = "m:1",
              verbose  = 0)

  out[, `:=`(
    #mean   = survey_mean_ppp,
    #median = survey_median_ppp,
    file   = NULL
  )]


  setnames(out,
           "povline",
           "poverty_line")


  # Ensure that out does not have duplicates
  out <- fg_remove_duplicates(out)

  # Fix issue with rounding of poverty lines
  out[,
      poverty_line := round(poverty_line, digits = 3) ]

  # Formatting. MUST be done in data.table tom modify by reference
  out[, path := as.character(path)]

  if ("max_year" %in% names(out)) {
    out[, max_year := NULL]
  }

  return(list(main_data     = out,
              data_in_cache = data_present_in_master))

}


# process_dt_fg <- function(dt, povline, mean_and_med = FALSE) {
#   dt[, compute_fgt_dt(.SD, "welfare", "weight", povline, mean_and_med),
#      by = .(file, reporting_level)]
# }


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
  # Modify cache_id
  # * Ensures that cache_id is unique for both extrapolated and interpolated surveys
  # * Ensures that cache_id can be kept as an output of fg_pip() while still removing duplicated rows
  df$cache_id <- fg_standardize_cache_id(cache_id = df$cache_id,
                                         interpolation_id = df$data_interpolation_id,
                                         reporting_level = df$reporting_level)
  # Set collapse vars to NA (by type)
  df <- fg_assign_nas_values_to_dup_cols(df = df,
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
