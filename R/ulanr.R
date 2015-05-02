#' Name to ULAN ID
#'
#' Queries the Getty ULAN to find the best matching ID for a given string. You
#' may filter the results by specifying an early or late date.
#'
#' @param name A name to match with a canonical ULAN id.
#' @param years Years used to restrict the matching functions. This argument
#'   should be a range of years in the form \code{c(1550, 1650)}. ULAN IDs will
#'   only be looked up for artists whose life dates intersect with this range.
#'   If no years are specified, then ULAN artists will be matched from all
#'   periods.)
#' @param method This value determines which method will be used to match the
#'   name to a canonical ULAN id.
#'
#' @return A canonical ULAN id for an artist as a 9-digit integer. If no match
#'   is found, then NULL will be returned isntead.
#'
#' @note \code{method = "sparql"} requires an internet connection.
#'
#' @export
#' @examples
#' \dontrun{ulan_id("Rembrandt", years = c(1600, 1700), method = "sparql")}
#' \dontrun{ulan_id("Rembrandt", years = c(1700, 1800), method = "sparql")}
ulan_id <- function(name, years = NULL, method = c("sparql")) {

  # Check if there is any name
  if(any(is.na(name), is.null(name), name == "")) {
    warning("NA, NULL, and '' names returned as NULL")
    return(NULL)
  }

  # Check years validity
  if(!(is.null(years))) {
    if (length(years) != 2)
      stop("Year should be a numeric vector with two values.")
    if (years[1] > years[2])
      stop("The first value for years should be smaller than the second value.")
  }

  # Dispatch name to query handler based on selected method
  if(method == "sparql") {
    ulan_sparql(name, years)
  } else {
    stop("Method ", method, "is not recognized. Try ?ulan_id for help.")
  }
}
