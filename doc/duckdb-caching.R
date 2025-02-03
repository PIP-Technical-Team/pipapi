## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE, echo = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  # 1.
#  pip(country = "all", year = 2000, lkup = lkup)
#  
#  # 2.
#  pip(country = "AGO", year = 2000, lkup = lkup)

## -----------------------------------------------------------------------------
#  pip(country = c("AGO", "USA"), year = 2000, lkup = lkup)

## -----------------------------------------------------------------------------
#  pip(country = "USA", year = 2000, lkup = lkup)

## -----------------------------------------------------------------------------
#  pip(country = c("ARG", "USA"), year = 2000, lkup = lkup)

## -----------------------------------------------------------------------------
#  pip(country = "all", year = 2000, lkup = lkup)

## -----------------------------------------------------------------------------
#  pip(country = "AGO", year = "all", lkup = lkup)

## -----------------------------------------------------------------------------
#  pip(country = "all", year = "all", lkup = lkup)

## -----------------------------------------------------------------------------
#  microbenchmark::microbenchmark(
#    pip_DEV = pip(country = c("AGO", "USA"), year = 2000, lkup = lkup)
#  )
#  
#  #Unit: microseconds
#  #       expr    min      lq    mean  median       uq    max neval
#  # duckdb_DEV 628.59 669.893 2475.44 689.934 719.7505 177901   100
#  
#  microbenchmark::microbenchmark(
#    duckdb_caching = pip(country = c("AGO", "USA"), year = 2000, lkup = lkup)
#  )
#  
#  #Unit: milliseconds
#  #           expr      min       lq     mean   median       uq     max neval
#  # duckdb_caching 138.3669 143.9853 148.3353 147.0136 152.0311 181.543   100

## -----------------------------------------------------------------------------
#  country_list <- c("AGO", "ARG", "AUT", "BEL", "BGD", "BLR", "BOL", "CAN", "CHE",
#    "CHL", "COL", "CRI", "DEU", "DNK", "DOM", "ECU", "ESP", "EST",
#    "FIN", "FRA", "FSM", "GBR", "GEO", "GRC", "GTM", "HRV", "HUN",
#    "IDN", "IDN", "IDN", "IRL", "ITA", "KGZ", "LTU", "LUX", "MAR",
#    "MDA", "MEX", "MKD", "MRT", "NOR", "PAN", "PER", "PHL", "PHL",
#    "POL", "ROU", "RUS", "RWA", "SLV", "STP", "SWE", "SWZ", "THA",
#    "TON", "TUN", "TWN", "TZA", "URY", "USA", "UZB", "ZAF")
#  
#  tictoc::tic()
#  
#  for(i in seq_along(country_list)) {
#    out <- pip(country = country_list[seq_len(i)], year = 2000, lkup = lkup)
#  }
#  
#  tictoc::toc()
#  
#  ## For DEV version
#  # 9.14 sec elapsed
#  
#  ## For Duckdb
#  # 10.39 sec elapsed

## -----------------------------------------------------------------------------
#  tictoc::tic()
#  
#  for(i in seq_along(country_list)) {
#    out <- pip(country = country_list[seq_len(i)], year = "all", lkup = lkup)
#  }
#  
#  tictoc::toc()
#  ## DEV
#  # 177.28 sec elapsed
#  
#  ## Duckdb caching
#  # 17.97 sec elapsed

