#' Search for a matching ULAN id by using the Getty's live SPARQL endpoint and
#' its Lucene index.
#'
#' This internal function implements the \code{method = "sparql"} option for
#' \link{ulan_id}. See that funciton for documentation.
#'
#' @param name A character string of an artist's name
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
ulan_sparql_handler <- function(name, early_year, late_year) {

  # Return NA for missing or empty values of name
  if(is.null(name))
    return(NA)
  if(is.na(name))
    return(NA)
  if(name == "")
    return(NA)

  # Strip punctuation from name string
  name <- tolower(gsub("[[:punct:]]", "", name))

  # Construct the query
  query_string = paste0("
    SELECT ?id
    WHERE {
    ?artist skos:inScheme ulan: ;
      rdf:type gvp:PersonConcept ;
      dc:identifier ?id ;
      xl:prefLabel|xl:altLabel [luc:term '", name, "'] .

    ?artist foaf:focus [gvp:biographyPreferred ?bio] .
    ?bio gvp:estStart ?startdate ;
         gvp:estEnd ?enddate .
    FILTER(?enddate >= '", early_year, "'^^xsd:gYear && ?startdate <= '",
                        late_year, "'^^xsd:gYear)
    } LIMIT 1")

  # Fire the query to the Getty SPARQL endpoint and parse the results
  results <- SPARQL::SPARQL(url = "http://vocab.getty.edu/sparql", query = query_string)$results

  if(nrow(results) == 0) {
    warning("No matches found for the following name: ", name)
    return(NA)
  } else {
    as.integer(results[1,1])
  }
}

#' Iterate the SPARQL method over the provided vectors
#'
#' This internal function maps the inputs from the generic \link{ulan_id}
#' function to the SPARQL implementation
#'
#' @param names A character string of an artist's name
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
#' @param progress_bar Display a progress bar for long vectors.
ulan_sparql <- function(names, early_year, late_year, progress_bar) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if((progress_bar == "default" & length(names) >= 50) | progress_bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_sparql_handler(a, b, c)},
      names, early_year, late_year, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    close(pb)
    return(ids)
  } else {
    mapply(function(a, b, c) ulan_sparql_handler(a, b, c), names, early_year, late_year, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  }
}
