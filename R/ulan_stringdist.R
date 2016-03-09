#' String Distance Lookup
#'
#' Run the table lookup using stringdist function, returning a data frame of all matches
#'
#' @param name
#' @param early_year
#' @param late_year
#' @param inclusive
#' @param stringdist_ops
ulan_stringdist_lookup <- function(name, early_year, late_year, inclusive, stringdist_ops) {
  # Strip punctuation from name string
  name <- trimws(tolower(gsub("[[:punct:]]", "", name)))

  # Should life dates be a subset of early_year and late_year, or merely
  # intersecting with early_year and late_year?
  if(inclusive) {
    score_table <- dplyr::filter(ulanrdata::query_table, birth_year <= late_year & death_year >= early_year)
  } else {
    score_table <- dplyr::filter(ulanrdata::query_table, birth_year >= early_year & death_year <= late_year)
  }

  # Look for exact matches - if we find them, then it's not necessary to do any
  # costly string distance calculations
  match_tries <- dplyr::filter(score_table, alt_name == name)

  if(nrow(match_tries) > 0) {
    match_tries$score <- 10
    return(match_tries)
  }

  # Calculate string distance scores, rescaling scores 0-10
  score_table <- dplyr::mutate(score_table, score = stringdist::stringdist(alt_name, name))
  score_table <- dplyr::mutate(score_table, score = 10/(score + 0.1))

  # Sort by inverse score
  score_table <- dplyr::distinct(dplyr::arrange(score_table, desc(score)), id)

  return(score_table)
}

#' Handle strindist results
#'
#' Returns one-row data frame with artist attributes
#'
#' @param name
#' @param early_year
#' @param late_year
#' @param inclusive
#' @param max_results
#' @param stringdist_ops
ulan_stringdist_match_handler <- function(name, early_year, late_year, inclusive, max_results, stringdist_ops = NULL) {

  # Check that ulanrdata is installed
  check_ulanrdata_package()

  score_cutoff <- 4

  score_table <- ulan_stringdist_lookup(name, early_year, late_year, inclusive, stringdist_ops)

  if(is.null(nrow(score_table)) | nrow(score_table) == 0 | score_table$score[1] < score_cutoff) {
    construct_results(results = NA, name = name)
  } else {
    score_table <- dplyr::slice(score_table, 1:max_results)
    construct_results(results = score_table)
  }
}
