#' Escape regex metacharacters in vintage folder names
#'
#' @param x character vector
#' @return character vector with regex metacharacters escaped
#' @keywords internal
escape_vintage_regex <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character(0))
  }
  gsub("([.\\\\|()[{}\\\\^$+*?\\]])", "\\\\\\1", as.character(x), perl = TRUE)
}

#' Build regular expression matching exact vintage folder names
#'
#' Takes a character vector of vintage folder names and constructs an
#' anchored regular expression pattern that matches only those exact names.
#'
#' @param folders character: Vector of vintage folder names
#' @return character scalar regex matching any of the specified folders exactly
#' @examples
#' build_vintage_pattern(c("20250930_2021_01_02_PROD", "20260922_2021_01_02_PROD"))
#' @export
build_vintage_pattern <- function(folders) {
  if (!is.character(folders) || length(folders) == 0L) {
    cli::cli_abort(
      "Argument {.arg folders} must be a non-empty character vector."
    )
  }
  folders <- trimws(folders)
  folders <- unique(folders[!is.na(folders) & nzchar(folders)])
  if (length(folders) == 0L) {
    cli::cli_abort(
      "Argument {.arg folders} must contain at least one non-empty folder name."
    )
  }
  escaped <- escape_vintage_regex(folders)
  sprintf("^(%s)$", paste(escaped, collapse = "|"))
}

#' Validate scalar string argument
#' @noRd
validate_scalar_string <- function(x, arg_name, allow_null = TRUE) {
  if (is.null(x)) {
    if (allow_null) return(invisible(NULL))
    cli::cli_abort(
      "Argument {.arg {arg_name}} must be a non-empty character scalar."
    )
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    cli::cli_abort(
      "Argument {.arg {arg_name}} must be a non-empty character scalar."
    )
  }
  invisible(x)
}

#' Filter binding list by field equality
#' @noRd
filter_pairing_field <- function(items, field, target, ignore_case = FALSE) {
  if (is.null(target)) return(items)
  target_val <- if (ignore_case) {
    tolower(as.character(target))
  } else {
    as.character(target)
  }
  Filter(
    function(b) {
      val <- as.character(b[[field]])
      if (ignore_case) val <- tolower(val)
      identical(val, target_val)
    },
    items
  )
}

#' Read vintage folder names for a pairing from pairing.yml
#'
#' Reads a pairing configuration file produced by `{pipschema}` and returns the
#' list of vintage folder names matching the container's schema and/or
#' pipapi version / commit SHA.
#'
#' @param pairing_path character: Path to the pairing.yml file
#' @param schema character: Optional schema name (e.g. "new-lineup" or "pre-lineup")
#' @param pipapi_version character: Optional pipapi version string (e.g. "1.5.15")
#' @param pipapi_sha character: Optional 40-character pipapi commit SHA
#' @return character vector of unique, sorted vintage folder names
#' @examples
#' \dontrun{
#' read_pairing_vintages(
#'   pairing_path = "/Data/pairing.yml",
#'   schema = "new-lineup",
#'   pipapi_version = "1.5.15"
#' )
#' }
#' @export
read_pairing_vintages <- function(pairing_path,
                                  schema = NULL,
                                  pipapi_version = NULL,
                                  pipapi_sha = NULL) {
  validate_scalar_string(pairing_path, "pairing_path", allow_null = FALSE)
  validate_scalar_string(schema, "schema", allow_null = TRUE)
  validate_scalar_string(pipapi_version, "pipapi_version", allow_null = TRUE)
  validate_scalar_string(pipapi_sha, "pipapi_sha", allow_null = TRUE)

  if (is.null(schema) && is.null(pipapi_version) && is.null(pipapi_sha)) {
    cli::cli_abort(
      "At least one selector ({.arg schema}, {.arg pipapi_version}, or {.arg pipapi_sha}) must be provided."
    )
  }

  if (!file.exists(pairing_path)) {
    cli::cli_abort("Pairing file does not exist: {.path {pairing_path}}")
  }
  pairing <- yaml::read_yaml(pairing_path)
  if (!is.list(pairing) || is.null(pairing$pairing_file_version)) {
    cli::cli_abort("Invalid pairing file format in {.path {pairing_path}}.")
  }
  ver <- pairing$pairing_file_version
  if (length(ver) != 1L || is.na(ver) ||
      !any(vapply(list(1L, 1, "1"), identical, logical(1), ver))) {
    cli::cli_abort(
      "Unsupported pairing_file_version: {.val {ver}} (expected 1)."
    )
  }
  bindings <- pairing$bindings
  if (is.null(bindings) || length(bindings) == 0L) {
    cli::cli_abort("Pairing file {.path {pairing_path}} has no bindings.")
  }

  matched <- bindings
  matched <- filter_pairing_field(matched, "schema", schema)
  matched <- filter_pairing_field(matched, "pipapi_version", pipapi_version)
  matched <- filter_pairing_field(
    matched, "pipapi_sha", pipapi_sha, ignore_case = TRUE
  )

  if (length(matched) == 0L) {
    cli::cli_abort(c(
      "No pairing bindings matched the requested criteria in {.path {pairing_path}}.",
      i = paste0(
        "Criteria: schema = {.val {schema}}, ",
        "pipapi_version = {.val {pipapi_version}}, ",
        "pipapi_sha = {.val {pipapi_sha}}"
      )
    ))
  }

  folders <- unlist(
    lapply(matched, function(b) as.character(b$folders)),
    use.names = FALSE
  )
  folders <- trimws(folders[!is.na(folders)])
  folders <- folders[nzchar(folders)]
  if (length(folders) == 0L) {
    cli::cli_abort(
      "Matched pairing bindings in {.path {pairing_path}} contain no valid folder names."
    )
  }
  sort(unique(folders))
}

#' Resolve whether to use the new lineup calculation pathway
#' @noRd
resolve_use_new_lineup <- function(versions, schema = NULL) {
  if (!is.null(schema)) {
    if (!is.character(schema) || length(schema) != 1L ||
        !schema %in% c("new-lineup", "pre-lineup")) {
      cli::cli_abort(c(
        "Unknown schema: {.val {schema}}.",
        i = "Expected 'new-lineup' or 'pre-lineup'."
      ))
    }
    return(identical(schema, "new-lineup"))
  }
  use_new_lineup_version(versions)
}
