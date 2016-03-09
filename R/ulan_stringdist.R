# Run the table lookup using stringdist function, returning a data frame of all matches
ulan_stringdist_lookup <- function(name, early_year, late_year, inclusive, stringdist_ops = NULL) {
  # Return NA for missing or empty values of name
  if(any(is.null(name), is.na(name), name == ""))
    return(NA)

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

# Returns one-row data frame with artist attributes
ulan_stringdist_match_handler <- function(name, early_year, late_year, inclusive, max_results, stringdist_ops = NULL) {

  score_table <- ulan_stringdist_lookup(name, early_year, late_year, inclusive, stringdist_ops)

  if(is.null(nrow(score_table)) | nrow(score_table) == 0) {
    warning("No matches found for the following name: ", name)
    return(dplyr::data_frame(
      id = NA,
      pref_name = NA,
      birth_year = NA,
      death_year = NA,
      gender = NA,
      nationality = NA,
      score = NA
    ))
  } else {
    score_table <- dplyr::slice(score_table, 1:max_results)
    dplyr::select(score_table, id, pref_name, birth_year, death_year, gender, nationality, score)
  }
}

ulan_stringdist_match <- function(names, early_year, late_year, inclusive, max_results) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if(use_pb(names)) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_stringdist_match_handler(a, b, c, inclusive, max_results)},
      names, early_year, late_year, SIMPLIFY = FALSE, USE.NAMES = TRUE)
    close(pb)
  } else {
    ids <- mapply(function(a, b, c) ulan_stringdist_match_handler(a, b, c, inclusive, max_results), names, early_year, late_year, SIMPLIFY = FALSE, USE.NAMES = TRUE)
  }
  return(ids)
}
