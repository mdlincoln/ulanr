#' Match names to the Getty ULAN
#'
#' Queries the Getty ULAN to find matching entries for a given string. You
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
#' @param inclusive Method for filtering search results using the
#'   early_year/late_year parameters. TRUE (the default) will only include
#'   artists whose life dates fall within the range [late_year, early_year].
#'   FALSE will include artists whose life dates intersect with [early_year,
#'   late_year]
#' @param method This value determines which method will be used to match the
#'   name to a canonical ULAN id. \code{sparql} will query the Getty's live
#'   endpoint, relying on its Lucene index for finding close matches, while
#'   \code{local} instead uses string distance measures based on a local table
#'   of ULAN entries.
#' @param max_results The maximum number of results to return. Defaults to 5.
#'   Depending on the query, the actual number of results returned may be lower.
#'   When \code{method = "sparql"} is used, values over 50 will be ignored.
#'
#' @return A named list of data.frames, one per submitted name, with 7 columns and no
#'   more than \code{max_results} rows:
#' \describe{
#' \item{\code{id}}{integer. ULAN id}
#' \item{\code{pref_name}}{character. ULAN preferred name}
#' \item{\code{birth_year}}{integer. Artist birth year, if assigned.}
#' \item{\code{death_year}}{integer. Artist death year, if assigned}
#' \item{\code{gender}}{character. Artist gender, if assigned.}
#' \item{\code{nationality}}{character. Artist nationality, if assigned.}
#' \item{\code{score}}{numeric. The score of the result. When \code{method =
#' "sparql"}, this is the Lucene index score (a higher score for a closer
#' match). When \code{method = "local"}, it will instead be a scaled string
#' distance score.}
#' }
#'
#' Unmatched names will return a data.frame with NAs for all values save
#' \code{name}.
#'
#' @note \code{method = "sparql"} requires an internet connection.
#'
#' @export
#' @examples
#' \dontrun{ulan_id("Rembrandt", early_year = 1600,
#'                  late_year = 1700, method = "sparql")}
#' \dontrun{ulan_id(c("Rembrandt", "Rothko"), early_year = c(1600, 1900),
#'                  late_year = c(1700, 2000), method = "sparql")}
ulan_match <- function(names, early_year = -9999, late_year = 2090, inclusive = TRUE, method = c("sparql", "local"), max_results = 5) {

  method <- match.arg(method)

  # Check names validity
  stopifnot(is.character(names))
  stopifnot(all(!is.na(names)))
  stopifnot(all(!names == ""))

  # early_year and late_year must be numeric
  stopifnot(all(is.numeric(early_year), is.numeric(late_year)))

  # Check if early_year and late_year are compatible
  stopifnot(length(early_year) == length(late_year))

  stopifnot(length(early_year) == 1 | length(early_year) == length(names))

  # Check inclusive validity
  stopifnot(is.logical(inclusive))

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
  ulan_dispatcher <- switch(method,
                       "local" = ulan_stringdist_match_handler,
                       "sparql" = ulan_sparql_match_handler)

  # For long queries or if explicitly set, create and increment txtProgressBar
  if(all(interactive(), length(names) > 5)) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_dispatcher(a, b, c, inclusive, max_results)},
      names, early_year, late_year, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    close(pb)
  } else {
    ids <- mapply(function(a, b, c) ulan_dispatcher(a, b, c, inclusive, max_results),
                  names, early_year, late_year, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }
  return(ids)
}
