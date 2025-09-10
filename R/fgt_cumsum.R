# MAIN -------------------------------

#' format the lfst list to usable data to estimate poverty
#'
#' @param lfst list from load_list_refy()
#'
#' @return list with DT and g (GRP object)
#' @keywords internal
#' Format lfst list for poverty estimation
#'
#' Takes a list from load_list_refy() and returns a list with a data.table and a GRP object for grouped operations.
#'
#' @param lfst List from load_list_refy().
#' @return List with elements: DT (data.table) and g (GRP object).
#' @keywords internal
format_lfst <- \(lfst, dict) {

  DT <- rbindlist(lfst, fill = TRUE)

  # Convert to factors (is it faster?)
  if (!is.integer(DT$index)) {
    DT[, index := as.integer(index)]
  }

  out <- encode_pairs(DT = DT,
                      dict = dict,
                      drop_labels = TRUE)

  ## Grouping ----------
  g <- GRP(out, ~ id_rl, sort = FALSE)

  list(DT = out,
       g = g)
}




#' Computes total population by group using the output of format_lfst().
#'
#' @param LDTg List from format_lfst() with DT and g objects.
#' @param dict data dictionary from build_pair_dict()
#'
#' @return data.table with total population by group.
#' @keywords internal
get_total_pop <- \(LDTg, dict) {
  list2env(LDTg, envir = environment())
  rm(LDTg)
  add_vars(g[["groups"]],
           get_vars(DT, c("weight")) |>
           fsum(g)) |>
    setnames(old = "weight",
             new =  "W") |>
    encode_pairs(dict, drop_labels = TRUE)
}


fgt_cumsum <- \(LDTg, tpop, povline,
                drop_vars = TRUE) {
  list2env(LDTg, envir = environment())
  rm(LDTg)

  # Temporal values to be added to the data.table
  tz      <- pmax(povline, 1e-12)
  tz2     <- pmax(povline^2, 1e-16)
  tlogz   <- log(tz)

  # 1) Compute cutpoint index for each z, using ONLY non-zero rows for welfare
  #    -> findInterval(povline, welfare) returns values in 0..N (never N+1)
  ID <- DT[index > 0L,
           {
             idx <- findInterval(povline, welfare, left.open = TRUE)
             # 2) Attach z, z2, logz in-group (no replication/copies)
             data.table(index = idx,
                        z     = tz,
                        z2    = tz2,
                        logz  = tlogz)
           },
           by = id_rl]

  # 3) Minimal cumulative view (shallow column subset; avoids copying DT)
  DT_min <- get_vars(DT,
                     c("id_rl", "index", "cw", "cwy", "cwy2", "cwylog"))

  # 4) join cutpoints to cumulatives (index==0 hits the already-present zero row)
  CS <- join(
    x = ID,
    y = DT_min,
    on   = c("id_rl","index"),
    how  = "left",
    validate = "m:1",          # many cutpoints -> 1 cumulative row
    drop.dup.cols = "y",
    verbose = 0) |>
  # 5) Bring total population W
    join(tpop,
         on = "id_rl",
         how = "left",
         validate = "m:1",
         drop.dup.cols = "y") |>
    setorder(id_rl, index)


  # 6) Compute measures (vectorized). Small clamps for numerical safety.
  CS[, `:=`(
    headcount        = cw / W,
    poverty_gap      = (z * cw - cwy) / (z_s * W),
    poverty_severity = (z2 * cw - 2 * z * cwy + cwy2) / (z2_s * W),
    watts            = (logz * cw - cwylog) / W
  )]

  if (!drop_vars) {
    return(CS)
  }
  get_vars(CS, c("id_rl", "headcount", "poverty_gap", "poverty_severity", "watts"))

}


# --- helpers ---------------------------------------------------------------

# ------------------------------- #
# 1) Build pair dictionary (DT)   #
# ------------------------------- #

#' Dictionary for fast joins
#'
#' @param lkup lkup object
#' @param fill_gaps TRUE for lineup years, FALSE for survey years
#'
#' @return data.table with dictionary for merges.
#' @keywords internal
#' Build dictionary for fast joins
#'
#' Creates a data.table dictionary for merging by id and reporting_level.
#'
#' @param lkup Lookup object containing refy_lkup and svy_lkup.
#' @param fill_gaps Logical, TRUE for lineup years, FALSE for survey years.
#' @return data.table with columns id, reporting_level, and code.
#' @keywords internal
build_pair_dict <- function(lkup, fill_gaps = TRUE) {

  FT <- if (fill_gaps) {
    lkup$refy_lkup[, .(country_code, reporting_year, reporting_level)]
  } else {
    lkup$svy_lkup[, .(country_code, reporting_year, reporting_level)]
  } |>
    funique()

  FT[, id := paste0(country_code, "_", reporting_year)
     ][, c("country_code", "reporting_year") := NULL]

  cols <- c("id", "reporting_level")
  dict <- unique(FT[, ..cols])

  # deterministic code order
  setorderv(dict, cols, order = 1L)     # radix by default
  dict[, code := as.integer(.I)]        # fast in DT
  setkeyv(dict, cols)                   # fast key lookups when needed
  setindexv(dict, "code")               # index on code
  dict
}


# -------------------------------------------- #
# 2) Encode: add integer code via collapse::join
# -------------------------------------------- #
# DT: data.table to encode (by reference not guaranteed since join copies x->result)
# dict: data.table from build_pair_dict()
# code_col: name of code column to write
#' Encode pairs with integer code
#'
#' Adds an integer code column to a data.table by joining with a dictionary.
#'
#' @param DT data.table to encode.
#' @param dict data.table from build_pair_dict().
#' @param id_col Name of id column.
#' @param level_col Name of reporting level column.
#' @param code_col Name of code column to write.
#' @param drop_labels Logical, drop id and level columns if TRUE.
#' @param strict Logical, error if any pairs are missing from dict.
#' @param verbose Integer, verbosity level.
#' @return data.table with code column added.
#' @keywords internal
encode_pairs <- function(DT, dict,
                         id_col = "id", level_col = "reporting_level",
                         code_col = "id_rl",
                         drop_labels = FALSE,
                         strict = TRUE,
                         verbose = 0L) {

  stopifnot(is.data.table(DT), is.data.table(dict))
  cols <- c(id_col, level_col)
  stopifnot(all(cols %in% names(DT)), all(c(cols, "code") %in% names(dict)))


  out <- join(
    x = DT,
    y = dict,
    on = cols,
    how = "left",
    drop.dup.cols = "y",
    validate = "m:1",
    verbose = verbose
  )
  # Ensure it's a data.table (join usually preserves)
  if (!is.data.table(out)) setDT(out)

  # Rename 'code' -> code_col if needed
  if (code_col != "code" && "code" %in% names(out)) {
    setnames(out, "code", code_col)
  }

  if (strict) {
    if (anyNA(out[[code_col]])) {
      nas <- is.na(out[[code_col]])
      miss <- unique(out[nas, ..cols])[1:min(10L, sum(nas))]
      cli::cli_abort(
        c(
          "encode_pairs(): {fsum(nas)} unseen (id, reporting_level) pair(s).",
          "Examples:\n{paste(capture.output(print(miss)), collapse = '\n')}"
        )
      )
    }
  }

  if (drop_labels) out[, (cols) := NULL]
  out
}

# ------------------------------------------------ #
# 3) Decode: join labels by code via collapse::join #
# ------------------------------------------------ #
#' Decode integer code to id and reporting level
#'
#' Joins labels by code using a dictionary.
#'
#' @param DT data.table to decode.
#' @param dict data.table from build_pair_dict().
#' @param code_col Name of code column in DT.
#' @param id_col Name of id column in dict.
#' @param level_col Name of reporting level column in dict.
#' @param keep_code Logical, keep code column if TRUE.
#' @param add_true_vars logical, add `country_code` and `reporting_year` and
#' removes var `id`
#' @param verbose Integer, verbosity level.
#'
#' @return data.table with id and reporting_level columns added.
#' @keywords internal
decode_pairs <- function(DT, dict,
                         code_col = "id_rl",
                         id_col = "id",
                         level_col = "reporting_level",
                         keep_code = FALSE,
                         add_true_vars = TRUE,
                         verbose = 0L) {
  stopifnot(exprs = {
    is.data.table(DT)
    is.data.table(dict)
    })
  stopifnot(exprs = {
    code_col %in% names(DT)
    all(c("code", id_col, level_col) %in% names(dict))
    })

  out <- join(
    x = DT,
    y = dict,
    on = setNames("code", code_col),   # map DT[code_col] to dict$code
    how = "left",
    drop.dup.cols = "y",
    validate = "m:1",
    verbose = verbose
  ) |>
    qDT()

  if (add_true_vars) {
    out[, `:=`(
        country_code   = gsub("(.+)(_.+)", "\\1", id),
        reporting_year = as.integer(gsub("(.+_)(.+)", "\\2", id))
      )][,
         id := NULL]
  }

  if (!keep_code) out[, (code_col) := NULL]
  out
}

# ----------------------------------------------------- #
# 4) Update dict with new pairs (append-only, fast DT)  #
# ----------------------------------------------------- #
#' Update dictionary with new pairs
#'
#' Appends new (id, reporting_level) pairs to the dictionary if needed.
#'
#' @param dict data.table dictionary from build_pair_dict().
#' @param DT data.table with id and reporting_level columns.
#' @param id_col Name of id column.
#' @param level_col Name of reporting level column.
#' @return Updated data.table dictionary.
#' @keywords internal
update_pair_dict <- function(dict, DT,
                             id_col = "id", level_col = "reporting_level") {
  stopifnot(is.data.table(dict), is.data.table(DT))
  cols <- c(id_col, level_col)
  stopifnot(all(c(cols, "code") %in% names(dict)), all(cols %in% names(DT)))

  new_pairs <- fsetdiff(unique(DT[, ..cols]), dict[, ..cols])
  if (nrow(new_pairs)) {
    new_pairs[, code := as.integer(max(dict$code) + seq_len(.N))]
    setkeyv(new_pairs, cols)
    setindexv(new_pairs, "code")
    dict <- rbindlist(list(dict, new_pairs), use.names = TRUE)
    setkeyv(dict, cols)
    setindexv(dict, "code")
  }
  dict
}



#' load refy list
#'
#' @param input_list list. output from [create_full_list]
#' @param path character: directory path
#'
#' @return character vector
#' @keywords internal
#' Load refy list
#'
#' Loads a list of files and returns a named list of data.tables, each with an id column.
#'
#' @param input_list Character vector of file paths (output from create_full_list).
#' @return Named list of data.tables, each with an id column.
#' @keywords internal
load_list_refy <- \(input_list){

  id_names <- input_list |>
    fs::path_file() |>
    fs::path_ext_remove()

  seq_flex <- if (interactive()) {
    cli::cli_progress_along
  } else {
    base::seq_along
  }


  lfst <- lapply(seq_flex(input_list),
                 \(i) {
                   x <- lup_files[i]
                   idn <- fs::path_file(x) |>
                     fs::path_ext_remove()
                   fst::read_fst(x, as.data.table = TRUE) |>
                     _[, id := idn]
                 }) |>
    setNames(id_names)

  lfst
}

