# This file exists to be used in API especially for /regression-params endpoint.
# Most of this function exists in wbpip and/or pipster package. However, at the time of development
# they were not yet ready to be utilized. Hence, we have made a local copy of those functions to be used in API.
# The file is named as such so as to remember that this file needs to be cleaned up.

### Start functions ###

#' Get Group Data Parameters
#'
#' Get Parameters and key values derived from the quadratic and Beta Lorenz
#' parametrization. `welfare` and `population` must be vectors of a group data
#' dataset
#'
#' @param welfare numeric vector of cumulative share of welfare (income/consumption)
#' @param weight numeric vector of cumulative share of the population
#' @param mean numeric scalar of distribution mean. Default is NULL
#' @param population numeric scalar with actual size of population. Default is NULL
#'
#' @return list with Group data parameters parameters
#' @references
#' Datt, G. 1998. "[Computational Tools For Poverty Measurement And
#' Analysis](https://ageconsearch.umn.edu/record/94862/)". FCND
#' Discussion Paper 50. World Bank, Washington, DC.
#'
#' Krause, M. 2013. "[Corrigendum to Elliptical Lorenz
#' curves](https://doi.org/10.1016/j.jeconom.2013.01.001)". *Journal of
#' Econometrics 174* (1): 44.
#'
#' Villasenor, J., B. C. Arnold. 1989. "[Elliptical Lorenz
#' curves](https://EconPapers.repec.org/RePEc:eee:econom:v:40:y:1989:i:2:p:327-338)".
#' *Journal of Econometrics 40* (2): 327-338.
#'
#' @examples
#' # Get Lorenz parameters
#' res <- pipgd_params(
#'   welfare = pip_gd$L,
#'   weight = pip_gd$P)
#' str(res)
#' @noRd
pipgd_params <- function(welfare,
                         weight,
                         mean = NULL,
                         population = NULL) {

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  pl <- as.list(environment())
  check_pipgd_params(pl)

  #   ____________________________________________________________________________
  #   Computations                                                            ####

  # create results list
  l_res <- vector(mode = "list", length = 2)
  names(l_res) <- c("gd_params", "data")


  # Apply Lorenz quadratic fit ----------------------------------------------

  ## STEP 1: Prep data to fit functional form-------------
  functional_form_lq <-
    wbpip::create_functional_form_lq(welfare    = welfare,
                              population = weight)

  ## STEP 2: Estimate regression coefficients using LQ parametrization------
  reg_results_lq <- wbpip::regres(functional_form_lq, is_lq = TRUE)
  names(reg_results_lq$coef) <- c("A", "B", "C")

  # add to results list
  l_res$gd_params$lq$reg_results <- reg_results_lq


  ## STEP 3: get key values
  # Compute key numbers from Lorenz quadratic form
  kv <- gd_lq_key_values(reg_results_lq$coef[["A"]],
                         reg_results_lq$coef[["B"]],
                         reg_results_lq$coef[["C"]])

  l_res$gd_params$lq$key_values <- kv


  # Apply Lorenz beta fit ---------------------------------------------------

  ## STEP 1: Prep data to fit functional form --------------
  functional_form_lb <-
    wbpip::create_functional_form_lb(welfare    = welfare,
                                      population = weight)

  ## STEP 2: Estimate regression coefficients using LB parameterization
  reg_results_lb <- wbpip::regres(functional_form_lb, is_lq = FALSE)
  names(reg_results_lb$coef) <- c("A", "B", "C")

  # add to results list
  l_res$gd_params$lb$reg_results <- reg_results_lb

  l_res$gd_params$lb$key_values <- NA

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  l_res$data$welfare    <- welfare
  l_res$data$weight     <- weight
  l_res$data$mean       <- mean
  l_res$data$population <- population
  class(l_res) <- "pipgd_params"
  l_res
}




#' Check parameters of get_gd functions
#'
#' @param lp list of parameters
#'
#' @return invisible TRUE
#' @keywords internal
#' @noRd
check_pipgd_params <- function(lp) {


  #   ____________________________________________________________________________
  #   Computations                                                            ####

  nlp <- names(lp)

  ## params --------------------
  if ("params" %in% nlp) {
    if (!is.null(lp$params) && !inherits(lp$params, "pipgd_params")) {
      cli::cli_abort(c("argument {.field params} must be of
                       class {.code pipgd_params}.",
                       "It should be created using {.fun pipgd_params}"))
    }
  }

  ## welfare -----------


  ## welfare and params -----------
  if ( all(c("params", "welfare") %in% nlp)) {
    if (!is.null(lp$params) &&
        (!is.null(lp$welfare)  || !is.null(lp$population))) {
      cli::cli_abort("You must specify either {.field params} or
                {.field welfare} and {.field population}")
    }
  }


  ## povline and popshare ----------
  if ( all(c("povline", "popshare") %in% nlp)) {
    if (!is.na(lp$povline) && !is.null(lp$popshare)) {
      cli::cli_abort("You must specify either {.field povline} or
                {.field popshare}")
    }
  }


  # "Either `params` or `welfare` and `population` should be spefied" =
  #   (is.null(params) && !is.null(welfare) && !is.null(population)) ||
  #   (!is.null(params) && is.null(welfare) && is.null(population))
  #
  # "`params` should be a list from `pipgd_validate_lorenz()`" =
  #   is.list(params) || is.null(params)
  #
  # "`complete` must be logical" =
  #   is.logical(complete)

  ## lorenz -----------
  if ( all(c("lorenz") %in% nlp)) {

    if (!is.null(lp$lorenz) && !lp$lorenz %in% c("lq", "lb")) {

      cli::cli_abort("{.field lorenz} must be either 'lq' or 'lb', or
                {.code NULL} to let the algorithm select")
    }
  }


  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(invisible(TRUE))

}


#' gd_lq_key_values
#' Get key values in Table 2 of Datt (1998) paper
#' @inheritParams gd_estimate_lq
#' @return list
#' @noRd
gd_lq_key_values <- function(A, B, C) {

  # Theorem 3 from original Lorenz quadratic paper
  e <- -(A + B + C + 1) # e = -(A + B + C + 1): condition for the curve to go through (1, 1)
  m <- (B^2) - (4 * A) # m < 0: condition for the curve to be an ellipse (m is called alpha in paper)
  n <- (2 * B * e) - (4 * C) # n is called Beta in paper
  r <- sqrt((n^2) - (4 * m * e^2))  # r is called K in paper


  s1 <- (r - n) / (2 * m)
  s2 <- -(r + n) / (2 * m)

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  l_res <- list(
    e = e,
    m = m,
    n = n,
    r = r,
    s1 = s1,
    s2 = s2
  )
  return(l_res)

}


#### pip_gd_lorenz ####

#' Check validity of Lorenz Curve
#'
#' @inheritParams pipgd_params
#' @param params list of parameters from `pipgd_validate_lorenz()`
#' @param complete logical: If TRUE, returns a list a cumulative returns from
#'   previously used `get_gd` functions. Default is `FALSE`
#' @param mean numeric: welfare mean of distribution.
#' @param povline numeric: value of poverty line. Default is the `mean` value
#' @param popshare numeric: range (0,1). Share of population. Provide share of
#'   population instead of poverty line
#' @param times_mean numeric factor that multiplies the mean to create a
#'   relative povertyline. Default is 1
#'
#'
#' @return list of distributional validity of each Lorenz model
#' @noRd
#'
#' @examples
#' # Using Lorenz parameters from pipgd_validate_lorenz
#'  res <-
#' pipgd_params(welfare = pip_gd$L,
#'              weight = pip_gd$P) |>
#'   pipgd_validate_lorenz() |>
#'   pipgd_select_lorenz()
#'
#' # Using welfare and population vecotrs
#' res2 <- pipgd_select_lorenz(welfare = pip_gd$L,
#'                             weight = pip_gd$P)
#' identical(res, res2)
pipgd_validate_lorenz <-
  function(params     = NULL,
           welfare    = NULL,
           weight     = NULL,
           mean       = 1,
           times_mean = 1,
           popshare   = NULL,
           povline    = ifelse(is.null(popshare),
                               mean*times_mean,
                               NA_real_),
           complete   = getOption("pipster.return_complete")
  ) {

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  pl <- as.list(environment())
  check_pipgd_params(pl)


  #   ____________________________________________________________________________
  #   Computations                                                            ####
  if (!is.null(welfare)) {
    params <- pipgd_params(welfare = welfare,
                           weight  = weight)
  }

  if (!is.null(popshare)) {
    povline_lq <- mean * wbpip::derive_lq(popshare,
                                   params$gd_params$lq$reg_results$coef[["A"]],
                                   params$gd_params$lq$reg_results$coef[["B"]],
                                   params$gd_params$lq$reg_results$coef[["C"]])

    povline_lb <- mean * wbpip::derive_lb(popshare,
                                   params$gd_params$lb$reg_results$coef[["A"]],
                                   params$gd_params$lb$reg_results$coef[["B"]],
                                   params$gd_params$lb$reg_results$coef[["C"]])

  } else {
    povline_lb <- povline_lq <- povline

  }

  # Validity or LQ
  validity_lq <- wbpip::check_curve_validity_lq(
    A = params$gd_params$lq$reg_results$coef[["A"]],
    B = params$gd_params$lq$reg_results$coef[["B"]],
    C = params$gd_params$lq$reg_results$coef[["C"]],
    key_values = params$gd_params$lq$key_values)

  headcount_lq <- gd_compute_headcount_lq(mean,
                                          povline_lq,
                                          params$gd_params$lq$reg_results$coef[["B"]],
                                          params$gd_params$lq$key_values$m,
                                          params$gd_params$lq$key_values$n,
                                          params$gd_params$lq$key_values$r)

  validity_lq$headcount <- headcount_lq

  # Validity of LB
  # Compute poverty stats
  headcount_lb <- wbpip::gd_compute_headcount_lb(mean,
                                          povline_lb,
                                          params$gd_params$lb$reg_results$coef[["A"]],
                                          params$gd_params$lb$reg_results$coef[["B"]],
                                          params$gd_params$lb$reg_results$coef[["C"]])

  # Check validity
  validity_lb <-
    wbpip::check_curve_validity_lb(headcount = headcount_lb,
                            params$gd_params$lb$reg_results$coef[["A"]],
                            params$gd_params$lb$reg_results$coef[["B"]],
                            params$gd_params$lb$reg_results$coef[["C"]])

  validity_lb$headcount <- headcount_lb

  if ( povline_lb != mean*times_mean) {
    times_mean <- povline_lb/mean
  }

  norm_lb_label <- paste0("Normality with a mean of ", mean,
                          " and a poverty line of ", povline_lb,
                          ";", times_mean, " times the mean.")

  attr(validity_lb$is_normal, "label") <- norm_lb_label


  #   __________________________________________________________________
  #   Return                                                          ####

  if (isFALSE(complete))
    params <- vector("list")

  params$gd_params$lq$validity <- validity_lq
  params$gd_params$lb$validity <- validity_lb

  return(params)

}


#' Get selected Lorenz curve for distributional stats
#'
#' @inheritParams pipgd_params
#' @inheritParams pipgd_validate_lorenz
#' @param params list of parameters from `pipgd_validate_lorenz()`
#'
#' @return list of values with best lorenz fit for distributional Stats
#' @noRd
#'
#' @examples
#' # Using Lorenz parameters from get_gd_lorenz_params
#' withr::local_options(pipster.return_complete  = TRUE)
#' params <- pipgd_validate_lorenz(
#'   welfare = pip_gd$L,
#'   weight = pip_gd$P)
#'
#' params <- pipgd_validate_lorenz(
#'   params = params,
#'   complete = TRUE)
#' pipgd_select_lorenz(params = params)
#'
#' # Using Lorenz parameters from pipgd_validate_lorenz
#' params <- pipgd_validate_lorenz(
#'   welfare = pip_gd$L,
#'   weight = pip_gd$P,
#'   complete = TRUE)
#' pipgd_select_lorenz(params = params)
#'
#' # Using original vectors
#'
#' pipgd_select_lorenz(
#'   welfare = pip_gd$L,
#'   weight = pip_gd$P)
pipgd_select_lorenz <-
  function(params     = NULL,
           welfare    = NULL,
           weight     = NULL,
           mean       = 1,
           times_mean = 1,
           popshare   = NULL,
           povline    = ifelse(is.null(popshare),
                               mean*times_mean,
                               NA_real_),
           complete   = getOption("pipster.return_complete")) {

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  pl <- as.list(environment())
  check_pipgd_params(pl)

  #   ____________________________________________________________________________
  #   Computations                                                            ####
  if (!is.null(welfare)) {
    params <- pipgd_validate_lorenz(welfare = welfare,
                                    weight = weight,
                                    complete   = TRUE,
                                    mean       = mean,
                                    times_mean = times_mean,
                                    povline    = povline,
                                    popshare   = popshare)
  }

  ## Selected Lorenz for  Distribution-------
  lq <- append(params$gd_params$lq$validity,
               params$gd_params$lq$reg_results["sse"])

  lb <- append(params$gd_params$lb$validity,
               params$gd_params$lb$reg_results["sse"])

  use_lq_for_dist <-
    wbpip::use_lq_for_distributional(lq,lb)

  ## Selected Lorenz for Poverty -----------

  fit_lb <- wbpip::gd_compute_fit_lb(params$data$welfare,
                              params$data$weight,
                              params$gd_params$lb$validity$headcount,
                              params$gd_params$lb$reg_results$coef[["A"]],
                              params$gd_params$lb$reg_results$coef[["B"]],
                              params$gd_params$lb$reg_results$coef[["C"]])

  fit_lq <- wbpip::gd_compute_fit_lq(welfare = params$data$welfare,
                              population = params$data$weight,
                              headcount = params$gd_params$lq$validity$headcount,
                              A = params$gd_params$lb$reg_results$coef[["A"]],
                              B = params$gd_params$lb$reg_results$coef[["B"]],
                              C = params$gd_params$lb$reg_results$coef[["C"]],
                              key_values = params$gd_params$lq$key_values)

  lq <- append(lq,
               fit_lq["ssez"])
  lb <- append(lb,
               fit_lb["ssez"])


  use_lq_for_pov <- wbpip::use_lq_for_poverty(lq, lb)

  l_res <- list(for_dist = ifelse(use_lq_for_dist, "lq", "lb"),
                for_pov  = ifelse(use_lq_for_pov, "lq", "lb"),
                use_lq_for_dist = use_lq_for_dist,
                use_lq_for_pov  = use_lq_for_pov)

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$selected_lorenz <- l_res
  return(params)

}


#' Compute poverty for Quadratic Lorenz
#'
#' @return poverty headcount
#' @noRd
gd_compute_headcount_lq <- function(
    mean,
    povline,
    B,
    m,
    n,
    r
) {

  #   _____________________________________________________________________
  #   Compute headcount
  #   _____________________________________________________________________
  bu <- B + (2 * povline / mean)
  headcount <- -(n + ((r * bu) / sqrt(bu^2 - m))) / (2 * m)


  #   _____________________________________________________________________
  #   Return
  #   _____________________________________________________________________
  return(headcount)

}


#' Lorenz curve
#'
#' Returns the Lorenz curve. User provides the cumulative welfare and
#' cumulative weight, as well as the number of points on the lorenz curve required.
#' By default, the best fitting Lorenz parameterization (quadratic or beta) is
#' selected.
#'
#' @param params list of parameters
#' @param welfare numeric vector of cumulative share of welfare (income/consumption)
#' @param weight numeric vector of cumulative share of the population
#' @param mean numeric scalar of distribution mean. Default is 1
#' @param times_mean factor that multiplies the mean to create a relative poverty line. Default is 1
#' @param popshare range (0,1). Share of population. Provide share of population instead of poverty line
#' @param povline value of poverty line. Default is the `mean` value
#' @param complete If TRUE, returns a list a cumulative returns from
#'   previously used `get_gd` functions. Default is `FALSE`
#' @param lorenz either "lb" or "lq"
#' @param n_bins atomic double vector of length 1: number of points on the
#' lorenz curve
#'
#' @return Returns a list which contains:
#'  * numeric lorenz curve,
#'  * corresponding points on x-axis,
#'  * whether lq or lb parameterization, and
#'  * if `complete=TRUE`, also returns all params.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Generating a Lorenz Curve with default settings
#' pipgd_lorenz_curve(welfare = pip_gd$L,
#'                    weight = pip_gd$P)
#'
#' # Example 2: Specifying the number of bins for the Lorenz Curve
#' pipgd_lorenz_curve(welfare = pip_gd$L,
#'                    weight = pip_gd$P,
#'                    n_bins = 50)
#'
#' # Example 3: Using pre-calculated parameters
#' use_params <- pipgd_params(welfare = pip_gd$L,
#'                            weight = pip_gd$P)
#' pipgd_lorenz_curve(params = use_params)
#'
#'
#' # Example 4: Generating Lorenz Curve with a specific Lorenz model(e.g. Lorenz beta)
#' pipgd_lorenz_curve(params = use_params,
#'                    lorenz = "lb")
#'}
#'
pipgd_lorenz_curve <- function(
    params     = NULL,
    welfare    = NULL,
    weight     = NULL,
    mean       = 1,
    times_mean = 1,
    popshare   = NULL,
    povline    = ifelse(is.null(popshare),
                        mean*times_mean,
                        NA_real_),
    complete   = getOption("pipster.return_complete"),
    lorenz     = NULL,
    n_bins     = 100
){

  #____________________________________________________________________
  #   Defenses
  #____________________________________________________________________
  pl <- as.list(environment())
  check_pipgd_params(pl)

  #____________________________________________________________________
  #   Params
  #____________________________________________________________________
  if (!is.null(welfare)) {
    params <- pipgd_select_lorenz(
      welfare  = welfare,
      weight   = weight,
      complete = TRUE,
      mean     = mean,
      povline  = povline
    )
  } else {
    params <- pipgd_select_lorenz(
      welfare  =  params$data$welfare,
      weight   =  params$data$weight,
      complete = TRUE,
      mean     = mean,
      povline  = povline
    )
  }

  #   _________________________________________________________________
  #   Select Lorenz
  #   _________________________________________________________________
  if (is.null(lorenz)) {
    lorenz <- params$selected_lorenz$for_dist
  } else {
    match.arg(lorenz, c("lq", "lb"))
  }

  #   _________________________________________________________________
  #   Lorenz Calculations
  #   _________________________________________________________________

  x_vec <- seq(from = 0, to = 1, length.out = n_bins)

  if (lorenz == "lb") {


    lc <- wbpip::value_at_lb(
      x = x_vec,
      A = params$gd_params$lb$reg_results$coef[["A"]],
      B = params$gd_params$lb$reg_results$coef[["B"]],
      C = params$gd_params$lb$reg_results$coef[["C"]]
    )

  } else if (lorenz == "lq") {

    lc <- sapply(
      X   = x_vec,
      FUN = function(x1){
        wbpip::value_at_lq(
          x = x1,
          A = params$gd_params$lq$reg_results$coef[["A"]],
          B = params$gd_params$lq$reg_results$coef[["B"]],
          C = params$gd_params$lq$reg_results$coef[["C"]]
        )

      }
    )

  }

  attributes(lc) <- NULL

  #   _________________________________________________________________
  #   Return
  #   _________________________________________________________________
  if (isFALSE(complete)) {
    params <- vector("list")
  }

  params$lorenz_curve$output <- lc
  params$lorenz_curve$points <- x_vec
  params$lorenz_curve$lorenz <- lorenz

  params


}


