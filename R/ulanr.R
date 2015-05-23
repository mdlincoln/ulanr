#' Validate input variables
#'
#' A helper function that validates the classes and lengths of inputs to
#' uland_id and ulan_data functions
#'
#' @param names A character vector of names to match to a canonical ULAN id.
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
#'
#' @return Will stop() code if there are any invalid variable types
validate_input <- function(names, early_year, late_year) {
  # Check names validity
  if(class(names) != "character")
    stop("names should be a character vector")

  # Check if early_year and late_year are compatible
  if(length(early_year) != length(late_year))
    stop("early_year and late_year must be of equal length")

  # Check early_year validity
  if(class(early_year) != "numeric")
    stop("early_year should be a numeric vector")
  if(length(early_year) != 1 & length(early_year) != length(names))
    stop("early_year must be the same length as names, or length 1")

  # Check late_year validity
  if(class(late_year) != "numeric")
    stop("late_year should be a numeric vector")
}

#' Name to ULAN ID
#'
#' Queries the Getty ULAN to find the best matching ID for a given string. You
#' may filter the results by specifying an early or late date.
#'
#' @param names A character vector of names to match to a canonical ULAN id.
#' @param early_year Match only artists who died after this year. Like
#'   \code{late_year}, this argument should be a numeric vector of length 1, or
#'   of the same length as \code{names}. If length 1, the same date restrictions
#'   will be used to match every value of \code{names}. Otherwise, each name
#'   match can be restricted to its own pair of early_year and late_year. If no
#'   \code{early_year} or \code{late_year} are specified, then artists from all
#'   time periods will be eligible for matching. Any NA values in
#'   \code{early_year} or \code{late_year} will be coerced to default maxima and
#'   minima.
#' @param late_year Match only artists who were born before this year.
#' @param method This value determines which method will be used to match the
#'   name to a canonical ULAN id.
#' @param progress_bar Display or hide a progress bar. By default, will only
#'   display a progress bar if processing more than 50 names. Passing TRUE will
#'   force a progress bar to appear; passing FALSE will always hide it.
#'
#' @return A canonical ULAN id for an artist as a 9-digit integer. If no match
#'   is found, then NULL will be returned isntead.
#'
#' @note \code{method = "sparql"} requires an internet connection.
#'
#' @export
#' @examples
#' \dontrun{ulan_id("Rembrandt", early_year = 1600,
#'                  late_year = 1700, method = "sparql")}
#' \dontrun{ulan_id(c("Rembrandt", "Rothko"), early_year = c(1600, 1900),
#'                  late_year = c(1700, 2000), method = "sparql")}
ulan_id <- function(names, early_year = -9999, late_year = 2090, method = c("sparql"), progress_bar = "default") {

  # Check names, early_year, and late_year for valid class, length, and value
  validate_input(names, early_year, late_year)

  # Replace any NA values in early_year and late_year with default time range
  if(any(is.na(early_year))) {
    warning("NAs in early_year have been coerced to -9999")
    early_year[is.na(early_year)] <- -9999
  }

  if(any(is.na(late_year))) {
    warning("NAs in late_year have been coerced to 2090")
    late_year[is.na(late_year)] <- 2090
  }

  # Dispatch name to query handler based on selected method
  if(method == "sparql") {
    ulan_sparql(names, early_year, late_year, progress_bar)
  } else {
    stop("Method ", method, "is not recognized. Try ?ulan_id for help.")
  }
}
