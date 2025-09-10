#' format the lfst list to usable data to estimate poverty
#'
#' @param lfst list from load_list_refy()
#'
#' @return list with DT and g (GRP object)
#' @keywords internal
format_lfst <- \(lfst) {

  DT <- rbindlist(lfst, fill = TRUE)

  # Convert to factors (is it faster?)
  DT[, names(.SD) := lapply(.SD, qF),
     .SDcols = c("id", "reporting_level")]

  # fix
  # DT[index == 0,
  #    names(.SD) := 0,
  #    .SDcols = is.numeric]
  #
  # DT <- DT[!grepl("^CHN_", id)]

  ## Grouping ----------

  g <- GRP(DT, ~ id + reporting_level, sort = FALSE)

  list(DT = DT,
       g = g)
}


