#' Add set of extra indicators to pip output
#'
#' @param dt data.frame: country level out from PIP
#'
#' @return data.frame
#' @keywords internal
get_additional_indicators <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ADd variables   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## get current var names --------
  # df <- as.data.frame(dt[1, ])
  original_names <- copy(names(dt))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add new navariables --------

  dt[,
      bottom40 := decile1 + decile2 + decile3 + decile4,
  ][,
    `:=`(
      pop_in_poverty = wbpip::get_number_poor(
           headcount = headcount,
           pop       = reporting_pop),

      average_shortfall = wbpip::get_average_shortfall(
        headcount = headcount,
        povgap    = poverty_gap,
        povline   = poverty_line),

      total_shortfall = wbpip::get_total_shortfall(
        headcount = headcount,
        pop       = reporting_pop,
        povgap    = poverty_gap,
        povline   = poverty_line),

      income_gap_ratio = wbpip::get_income_gap_ratio(
        headcount = headcount,
        povgap    = poverty_gap),

      palma_ratio = wbpip::get_palma_ratio(
        top10     = decile10,
        bottom40  = bottom40),

      p90p10_ratio = wbpip::get_9010_ratio(top10    = decile9,
                                              bottom10 = decile1)

    )
  ]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## get difference between the names --------


  # By the end of pip.R variables of final data are selected using the
  #`lkup$return_cols$pip$cols` vectors. Since both that vector of variable names and the set
  #of additional indicators may change in the future, we need to make sure that
  #variables that are kept in the final data are just the additional variables
  #from this function {additional_indicators()} and the ones in the
  #`lkup$return_cols$pip$cols` vector.

  add_names <- names(dt)

  newnames <- add_names[!add_names %in% original_names]

  setattr(dt, "new_indicators_names", newnames)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(TRUE))

}



#' Add set of extra indicators to pip output in aggregate data.
#'
#' @param dt data.frame: global/regional level  from PIP-grp
#'
#' @return data.frame
#' @keywords internal
get_additional_indicators_grp <- function(dt) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ADd variables   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## get current var names --------
  # df <- as.data.frame(dt[1, ])
  original_names <- copy(names(dt))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add new navariables --------

  dt[,
    `:=`(
      pop_in_poverty = wbpip::get_lh_number_poor(
        headcount = headcount,
        pop       = reporting_pop),

      average_shortfall = wbpip::get_lh_average_shortfall(
        headcount = headcount,
        povgap    = poverty_gap,
        povline   = poverty_line),

      total_shortfall = wbpip::get_lh_total_shortfall(
        headcount = headcount,
        pop       = reporting_pop,
        povgap    = poverty_gap,
        povline   = poverty_line),

      income_gap_ratio = wbpip::get_lh_income_gap_ratio(
        headcount = headcount,
        povgap    = poverty_gap)
    )
  ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## get difference between the names --------
  add_names <- names(dt)

  newnames <- add_names[!add_names %in% original_names]

  setattr(dt, "new_indicators_names", newnames)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(invisible(TRUE))

}
