# Getter function: Returns the entire .pipapienv environment
#' Get the entire .pipapienv environment
#'
#' @return The .pipapienv environment
#' @export
#'
#' @examples
#' env <- get_pipapienv()
get_pipapienv <- function() {
  .pipapienv
}

# Getter for a specific key from .pipapienv
#' Get a value from .pipapienv
#'
#' @param key A character string representing the key
#'
#' @return The value associated with the key in .pipapienv
#' @export
#'
#' @examples
#' set_in_pipapienv("example_key", 42)
#' get_from_pipapienv("example_key") # returns 42
get_from_pipapienv <- function(key) {
  rlang::env_get(.pipapienv, key, default = NULL) # Returns NULL if key doesn't exist
}

# Setter function: Assign a value in .pipapienv
#' Set a value in .pipapienv
#'
#' @param key A character string representing the key
#' @param value The value to store in .pipapienv
#'
#' @return The assigned value (invisibly)
#' @export
#'
#' @examples
#' set_in_pipapienv("example_key", 42)
set_in_pipapienv <- function(key, value) {
  rlang::env_poke(.pipapienv, key, value)
  invisible(value)  # Return value invisibly to avoid clutter in console
}
