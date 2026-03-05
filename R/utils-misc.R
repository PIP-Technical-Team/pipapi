# utils-misc.R
#
# General-purpose helpers that do not fit into a more specific utils-* file.
#
# Functions:
#   is_empty()         - TRUE if a vector is length-0 and non-NULL
#   fillin_list()      - populate a list with same-named objects from caller frame
#   clear_cache()      - reset a cachem disk cache object
#   get_caller_names() - return names of all functions on the call stack
#   unnest_dt_longer() - efficient tidyr::unnest_longer for data.tables


#' Test whether a vector is length zero and is not NULL
#'
#' @param x Vector to test
#'
#' @return logical. \code{TRUE} if \code{x} is empty but not \code{NULL}
#' @export
#'
#' @examples
#' x <- vector()
#' is_empty(x)
#'
#' y <- NULL
#' length(y)
#' is_empty(y)
is_empty <- function(x) {
  if (length(x) == 0 & !is.null(x)) {
    TRUE
  } else {
    FALSE
  }
}


#' Populate a list from same-named objects in the parent frame
#'
#' Fills named elements of \code{l} with the values of identically-named
#' objects found in the calling function's environment.
#'
#' @param l list to populate. All names must exist in the parent frame.
#' @param assign logical: whether to assign the filled list back to the
#'   parent frame variable of the same name.
#'
#' @return The populated list \code{l} (invisibly).
#' @export
#'
#' @examples
#' l <- list(x = NULL,
#' y = NULL,
#' z = NULL)
#'
#' x <-  2
#' y <-  "f"
#' z <- TRUE
#' fillin_list(l)
#' l
fillin_list <- function(l, assign = TRUE) {
  stopifnot(exprs = {
    is.list(l)
    is.data.frame(l) == FALSE
  })

  if (FALSE) {
    return()
  }

  # name of the list in parent frame
  nm_l = deparse(substitute(l))

  # names of the objects of the list
  nm_obj <- names(l)

  # all the objects in parent frame
  obj_in_parent <- ls(envir = parent.frame())

  # make sure that all the objects in list are in parent frame
  if (!all(nm_obj %in% obj_in_parent)) {
    non_in_parent <- nm_obj[!nm_obj %in% obj_in_parent]

    stop_msg <- paste(
      "The following objects are not in calling function: \n",
      paste(non_in_parent, collapse = ", ")
    )

    stop(stop_msg)
  }

  val_obj <- lapply(nm_obj, get, envir = parent.frame())
  names(val_obj) <- nm_obj

  for (i in seq_along(nm_obj)) {
    x <- val_obj[[nm_obj[i]]]
    if (!is_empty(x)) {
      l[[nm_obj[i]]] <- x
    }
  }

  if (assign == TRUE) {
    assign(nm_l, l, envir = parent.frame())
  }

  return(invisible(l))
}


#' Clear a cachem disk cache
#'
#' Resets the cache and reports success or failure.
#'
#' @param cd A \code{cachem::cache_disk()} object
#' @return Named list with \code{$status} (\code{"success"} or
#'   \code{"error"}) and \code{$msg}.
#' @keywords internal
clear_cache <- function(cd) {
  tryCatch(
    {
      if (cd$size() > 0) {
        cd$reset()
        n <- cd$size()
        if (n == 0) {
          out <- list(status = 'success', msg = 'Cache cleared.')
        } else {
          out <- list(
            status = 'error',
            msg = sprintf('Something went wrong. %n items remain in cache.', n)
          )
        }
      } else {
        out <- list(
          status = 'success',
          msg = 'Cache directory is empty. Nothing to clear.'
        )
      }
      return(out)
    },
    error = function(e) {
      out <- list(status = 'error', msg = 'Cache directory not found.')
      return(out)
    }
  )
}


#' Get function names on the call stack
#'
#' Walks \code{sys.calls()} and returns a character vector of function name
#' strings. Handles \code{do.call()} specially by peeking at the next frame.
#'
#' @return character vector of call names (invisibly)
#' @export
get_caller_names <- function() {
  # Get the list of calls on the call stack
  calls <- sys.calls()

  lcalls <- length(calls)
  caller_names <- vector("character", length = lcalls)

  tryCatch(
    expr = {
      i <- 1
      while (i <= lcalls) {
        call <- calls[[i]]
        call_class <- class(call[[1]])
        call_type <- typeof(call[[1]])
        call_length <- length(call[[1]])

        call[[1]] <-
          deparse(call[[1]]) |>
          as.character()

        if (length(call[[1]]) > 1) {
          call[[1]] <-
            paste0(call[[1]], collapse = "-") |>
            substr(1, 10)
        }

        call_text <- call[[1]]

        if (call[[1]] == as.name("do.call")) {
          caller_names[i] <- "do.call"
          i <- i + 1 # jump one call
          caller_names[i] <- deparse(call[[2]])
        } else {
          # Regular call: Directly take the function name
          caller_names[i] <- deparse(call[[1]])
        }
        i <- i + 1
      }
    },

    error = function(err) {
      msg <- c(
        paste("Error in call", i),
        paste("class:", call_class),
        paste("type:", call_type),
        paste("length:", call_length),
        paste("text:", call_text)
      )
      rlang::abort(msg, parent = err)
    },

    warning = function(w) {
      msg <- c(
        paste("Warning in call", i),
        paste("class:", call_class),
        paste("type:", call_type),
        paste("length:", call_length),
        paste("text:", call_text)
      )
      rlang::warn(msg, parent = w)
    }
  )

  invisible(caller_names)
}


#' Efficient unnest_longer for data.tables
#'
#' An efficient alternative to \code{tidyr::unnest_longer()} that operates
#' directly on data.tables.
#'
#' @param tbl a dataframe/tibble/data.table
#' @param cols one or more column names in \code{tbl} that contain list columns
#'
#' @return A longer data.table
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'  a = LETTERS[1:5],
#'  b = LETTERS[6:10],
#'  list_column1 = list(c(LETTERS[1:5]), "F", "G", "H", "I"),
#'  list_column2 = list(c(LETTERS[1:5]), "F", "G", "H", "K")
#' )
#'  unnest_dt_longer(df, grep("^list_column", names(df), value = TRUE))
#' }
unnest_dt_longer <- function(tbl, cols) {
  tbl <- data.table::as.data.table(tbl)
  clnms <- rlang::syms(setdiff(colnames(tbl), cols))

  tbl <- eval(
    rlang::expr(tbl[, lapply(.SD, unlist), by = list(!!!clnms), .SDcols = cols])
  )

  colnames(tbl) <- c(as.character(clnms), cols)

  tbl
}
