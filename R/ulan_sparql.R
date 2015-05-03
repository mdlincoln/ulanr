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

  # Construct the base of the query
  base_query = paste0("
    SELECT ?id
    WHERE {
    ?artist skos:inScheme ulan: ;
      rdf:type gvp:PersonConcept ;
      dc:identifier ?id ;
      xl:altLabel [luc:term '", name, "'] .")

  # Construct date filter
  date_filter <- paste0("
      ?artist foaf:focus [gvp:biographyPreferred ?bio] .
      ?bio gvp:estStart ?startdate ;
           gvp:estEnd ?enddate .
      FILTER(?enddate >= '", early_year, "'^^xsd:gYear && ?startdate <= '",
                        late_year, "'^^xsd:gYear)")

  # Limit to the top result
  query_string <- paste0(base_query, date_filter, "} LIMIT 1")

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
#' @param name A character string of an artist's name
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
ulan_sparql <- function(names, early_year, late_year) {
  mapply(function(a, b, c) ulan_sparql_handler(a, b, c), names, early_year, late_year, SIMPLIFY = TRUE, USE.NAMES = FALSE)
}
