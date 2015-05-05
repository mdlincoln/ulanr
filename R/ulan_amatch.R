ulan_amatch_handler <- function(name, early_year, late_year) {
  ulanrdata::id_altname %>% dplyr::filter(
    birth <= late_year &
      death >= early_year &
    agrepl(name, alt_name))
}

ulan_amatch <- function(names, early_year, late_year) {
  mapply(function(a, b, c) ulan_amatch_handler(a, b, c), names, early_year, late_year)
}
