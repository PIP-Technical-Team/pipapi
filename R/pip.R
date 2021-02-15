pip <- function(country   = "all",
                povline   = NULL,
                popshare  = NULL,
                year      = "all",
                fill_gaps = FALSE,
                aggregate = FALSE,
                welfare_type = "all",
                svy_coverage = "all",
                ppp       = NULL,
                server    = NULL,
                lkup,
                paths) {


  # Handle interpolation
  if (fill_gaps == TRUE) {

    out <- fg_pip(country      = country,
                  povline      = povline,
                  popshare     = popshare,
                  year         = year,
                  aggregate    = aggregate,
                  welfare_type = welfare_type,
                  svy_coverage = svy_coverage,
                  ppp          = ppp,
                  server       = server,
                  lkup         = lkup,
                  paths        = paths)
  } else {
    out <- rg_pip(country      = country,
                  povline      = povline,
                  popshare     = popshare,
                  year         = year,
                  aggregate    = aggregate,
                  welfare_type = welfare_type,
                  svy_coverage = svy_coverage,
                  ppp          = ppp,
                  server       = server,
                  lkup         = lkup,
                  paths        = paths)
  }
  # out$is_interpolated <- fill_gaps

  return(out)
}


