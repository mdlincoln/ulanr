ulan_amatch_handler <- function(name, early_year, late_year) {
  ulanrdata::id_altname %>% dplyr::filter(
    birth <= late_year &
      death >= early_year &
    agrepl(name, alt_name))
}

ulan_amatch <- function(names, early_year, late_year, progress_bar) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if((progress_bar == "default" & length(names) >= 50) | progress_bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_amatch_handler(a, b, c)},
      names, early_year, late_year, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    close(pb)
    return(ids)
  } else {
    mapply(function(a, b, c) ulan_amatch_handler(a, b, c), names, early_year, late_year)
  }
}
