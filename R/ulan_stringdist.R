# String Distance Lookup
#
# Run the table lookup using stringdist function, returning a data frame of all matches
ulan_stringdist_match_handler <- function(name, early_year, late_year, inclusive, max_results, cutoff_score) {
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
    match_tries$score <- 1
    return(construct_results(match_tries))
  }

  # Calculate string similarity
  score_table$score <- stringdist::stringsim(name, score_table$alt_name, method = "cosine")

  # Sort by inverse score
  score_table <- dplyr::distinct_(score_table, "id")
  score_table <- dplyr::filter_(score_table, .dots = list(~score > cutoff_score))
  score_table <- dplyr::arrange_(score_table, ~desc(score))

  if(is.null(nrow(score_table)) | nrow(score_table) == 0) {
    construct_results(results = NA, name = name)
  } else {
    score_table <- score_table[1:min(c(nrow(score_table), max_results)),]
    construct_results(results = score_table)
  }
}
