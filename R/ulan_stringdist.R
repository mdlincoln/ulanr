ulan_stringdist_handler <- function(name, early_year, late_year) {

  # Return NA for missing or empty values of name
  if(is.null(name))
    return(NA)
  if(is.na(name))
    return(NA)
  if(name == "")
    return(NA)

  # Strip punctuation from name string
  name <- tolower(gsub("[[:punct:]]", "", name))

  score_table <- dplyr::arrange(
    dplyr::mutate(
      dplyr::filter(
        ulanrdata::query_table,
        birth <= late_year & death >= early_year),
      score = stringdist::stringdist(alt_name, name)),
    score
  )[1,]

  if(score_table[[1, 7]] > 10) {
    warning("No matches found for the following name: ", name)
    return(NA)
  } else {
    score_table[[1, 1]]
  }
}

ulan_stringdist <- function(names, early_year, late_year, progress_bar) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if((progress_bar == "default" & length(names) >= 50) | progress_bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_stringdist_handler(a, b, c)},
      names, early_year, late_year, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    close(pb)
    return(ids)
  } else {
    mapply(function(a, b, c) ulan_stringdist_handler(a, b, c), names, early_year, late_year, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  }
}
