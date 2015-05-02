#' Search for a matching ULAN id by using the Getty's live SPARQL endpoint and
#' its Lucene index.
#'
#' This internal function implements the \code{method = "sparql"} option for
#' \link{ulan_id}. See that funciton for documentation.
#'
#' @param name A character string of an artist's name
#' @param years Window in which to search for matching artists
ulan_sparql <- function(name, years) {

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

  # If life date limits have been provided, add them to the query
  if(!(is.null(years))) {
    date_filter <- paste0("
      ?artist foaf:focus [gvp:biographyPreferred ?bio] .
      ?bio gvp:estStart ?startdate ;
           gvp:estEnd ?enddate .
      FILTER(?enddate >= '", years[1], "'^^xsd:gYear && ?startdate <= '",
      years[2], "'^^xsd:gYear)")

    base_query <- paste0(base_query, date_filter)
  }

  # Limit to the top result
  query_string <- paste0(base_query, "} LIMIT 1")

  # Fire the query to the Getty SPARQL endpoint and parse the results
  results <- SPARQL::SPARQL(url = "http://vocab.getty.edu/sparql", query = query_string)$results

  if(nrow(results) == 0) {
    warning("No matches found for the following name: ", name)
    return(NULL)
  } else {
    as.integer(results[1,1])
  }
}
