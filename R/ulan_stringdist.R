# Run the table lookup using stringdist function, returning a data frame of all matches
ulan_stringdist_lookup <- function(name, early_year, late_year, inclusive, stringdist_ops = NULL) {
  # Return NA for missing or empty values of name
  if(is.null(name))
    return(NA)
  if(is.na(name))
    return(NA)
  if(name == "")
    return(NA)

  # Strip punctuation from name string
  name <- tolower(gsub("[[:punct:]]", "", name))

  # Should life dates be a subset of early_year and late_year, or merely
  # intersecting with early_year and late_year?
  if(inclusive) {
    score_table <- dplyr::filter(ulanrdata::query_table, birth <= late_year & death >= early_year)
  } else {
    score_table <- dplyr::filter(ulanrdata::query_table, birth >= early_year & death <= late_year)
  }

  # Calculate string distance scores
  score_table <- dplyr::mutate(score_table, score = stringdist::stringdist(alt_name, name))

  # Sort by string distance score
  score_table <- dplyr::arrange(score_table, score)

  return(score_table)
}

# Returns a single ID
ulan_stringdist_id_handler <- function(name, early_year, late_year, inclusive, stringdist_ops = NULL) {

  score_table <- ulan_stringdist_lookup(name, early_year, late_year, inclusive, stringdist_ops = NULL)

  if(score_table$score[1] > 10) {
    warning("No matches found for the following name: ", name)
    return(NA)
  } else {
    score_table$id[1]
  }
}

# Returns one-row data frame with artist attributes
ulan_stringdist_data_handler <- function(name, early_year, late_year, inclusive, stringdist_ops = NULL) {

  score_table <- ulan_stringdist_lookup(name, early_year, late_year, inclusive, stringdist_ops = NULL)

  if(score_table$score[1] > 10) {
    warning("No matches found for the following name: ", name)
    return(dplyr::data_frame(
      id = NA,
      pref_name = NA,
      birth = NA,
      death = NA,
      gender = NA,
      nationality = NA,
      name = name
    ))
  } else {
    results <- dplyr::select(score_table, -score)[1,]
    results$name <- name
    return(results)
  }
}

ulan_stringdist_id <- function(names, early_year, late_year, inclusive, progress_bar) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if((progress_bar == "default" & length(names) >= 50) | progress_bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c, d) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_stringdist_id_handler(a, b, c, d)},
      names, early_year, late_year, inclusive, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    close(pb)
    return(ids)
  } else {
    mapply(function(a, b, c, d) ulan_stringdist_id_handler(a, b, c, d), names, early_year, late_year, inclusive, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  }
}

ulan_stringdist_data <- function(names, early_year, late_year, inclusive, progress_bar) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if((progress_bar == "default" & length(names) >= 50) | progress_bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c, d) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_stringdist_data_handler(a, b, c, d)},
      names, early_year, late_year, inclusive, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    close(pb)
    dplyr::bind_rows(ids)
  } else {
    ids <- mapply(function(a, b, c, d) ulan_stringdist_data_handler(a, b, c, d), names, early_year, late_year, inclusive, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    dplyr::bind_rows(ids)
  }
}
