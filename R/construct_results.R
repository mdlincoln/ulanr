# Construct a dataframe. If given NA, this function will return a warning and
# an empty data frame with the same set of columns as a complete result.
construct_results <- function(results, name = NULL) {
  if(!is.data.frame(results)) {
    warning("No matches found for ", name)
    dplyr::data_frame(
      id = NA_integer_,
      pref_name = NA_character_,
      birth_year = NA_integer_,
      death_year = NA_integer_,
      gender = NA_character_,
      nationality = NA_character_,
      score = NA_real_)
  } else {
    dplyr::select_(results, "id", "pref_name", "birth_year", "death_year", "gender", "nationality", "score")
  }
}
