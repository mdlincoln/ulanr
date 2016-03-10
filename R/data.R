#' Testing datasets from the Rijksmuseum
#'
#' Samples from the Rijksmuseum's database of artists that have already been coded with IDs from the ULAN. \code{rkm_prolific} lists the 100 most prolific artists in the collection (counted by number of works owned for which the artist is listed as a creator), while \code{rkm_random} is a random sample of 100 artists.
#'
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'   \item{\code{rkm_name}}{character. The name string given to the artist by the Rijksmuseum.}
#'   \item{\code{rkm_birth_year}}{integer. The birth year given to the artist by the Rijksmuseum, when available.}
#'   \item{\code{rkm_death_year}}{integer. The death year given to the artist by the Rijksmuseum, when available.}
#'   \item{\code{actual_id}}{integer. The ULAN ID for the artist, when one is available. \code{NA} values indicate that there is no entry for the artist in the ULAN database.}
#' }
#'
#' @source https://www.rijksmuseum.nl/nl/api
#' @name rkm_test_data

#' @rdname rkm_test_data
"rkm_random"

#' @rdname rkm_test_data
"rkm_prolific"
