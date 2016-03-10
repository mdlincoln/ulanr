#' String Distance Lookup
#'
#' Run the table lookup using stringdist function, returning a data frame of all matches
#'
#' @param name Passed from ulan_stringdist_match_handler.
#' @param early_year Passed from ulan_stringdist_match_handler.
#' @param late_year Passed from ulan_stringdist_match_handler.
#' @param inclusive Passed from ulan_stringdist_match_handler.
#' @param stringdist_ops Passed from ulan_stringdist_match_handler, to be used
#'   in stringdist() call.
ulan_stringdist_lookup <- function(name, early_year, late_year, inclusive, stringdist_ops) {
  # Strip punctuation from name string
  name <- trimws(tolower(gsub("[[:punct:]]", "", name)))

  # Should life dates be a subset of early_year and late_year, or merely
  # intersecting with early_year and late_year?
  if(inclusive) {
    score_table <- dplyr::filter_(ulanrdata::query_table, .dots = list(~birth_year <= late_year & death_year >= early_year))
  } else {
    score_table <- dplyr::filter_(ulanrdata::query_table, .dots = list(~birth_year >= early_year & death_year <= late_year))
  }

  # Look for exact matches - if we find them, then it's not necessary to do any
  # costly string distance calculations
  match_tries <- dplyr::filter_(score_table, .dots = list(~alt_name == name))

  if(nrow(match_tries) > 0) {
    match_tries$score <- 0
    return(match_tries)
  }

  # Calculate string distance scores, rescaling scores 0-10
  score_table$score <- stringdist::stringdist(name, score_table$alt_name, method = "cosine")

  # Sort by inverse score
  score_table <- dplyr::distinct_(dplyr::arrange_(score_table, "score"), "id")

  return(score_table)
}

#' Handle strindist results
#'
#' Returns one-row data frame with artist attributes
#'
#' @param name Passed from ulan_match.
#' @param early_year Passed from ulan_match.
#' @param late_year Passed from ulan_match.
#' @param inclusive Passed from ulan_match.
#' @param max_results Passed from ulan_match.
#' @param stringdist_ops Passed from ulan_match.
ulan_stringdist_match_handler <- function(name, early_year, late_year, inclusive, max_results, stringdist_ops = NULL) {

  # Check that ulanrdata is installed
  check_ulanrdata_package()

  score_table <- ulan_stringdist_lookup(name, early_year, late_year, inclusive, stringdist_ops)

  if(is.null(nrow(score_table)) | nrow(score_table) == 0) {
    construct_results(results = NA, name = name)
  } else {
    score_table <- score_table[1:min(c(nrow(score_table), max_results)),]
    construct_results(results = score_table)
  }
}
