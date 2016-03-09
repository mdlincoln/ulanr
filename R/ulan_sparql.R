#' Construct SPARQL date filter
#'
#' Constructs a portion of the SPARQL query to filter artists based on life
#' dates
#'
#' @param inclusive Passed by ulan_sparql_match_handler.
#' @param early_year Passed by ulan_sparql_match_handler.
#' @param late_year Passed by ulan_sparql_match_handler.
date_filter <- function(inclusive, early_year, late_year) {
  if(inclusive) {
    paste0("FILTER(?birth_year >= '", early_year, "'^^xsd:gYear && ?death_year <= '",
           late_year, "'^^xsd:gYear)")
  } else {
    paste0("FILTER(?birth_year <= '", late_year, "'^^xsd:gYear && ?death_year >= '",
           early_year, "'^^xsd:gYear)")
  }
}

#' Format a SPARQL query as a URL
#'
#' Properly escapes the query to send to the Getty SPARQL endpoint
#'
#' @param query Passed by ulan_sparql_match_handler.
sparql_url <- function(query) {
  endpoint <- "http://vocab.getty.edu/sparql"
  escaped_query <- URLencode(query, reserved = TRUE)
  paste0(endpoint, ".csv?query=", escaped_query)
}

#' Search for a matching ULAN id and its associated databy using the Getty's
#' live SPARQL endpoint and its Lucene index.
#'
#' This internal function implements the \code{method = "sparql"} option for
#' \link{ulan_data}. See that funciton for documentation.
#'
#' @param name A character string of an artist's name
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
#' @param inclusive Logical. Should life dates be filtered inclusive of the
#'   [early_year, late_year] range?
#' @param max_results The maximum number of results to return
ulan_sparql_match_handler <- function(name, early_year, late_year, inclusive, max_results) {

  # Return NA for missing or empty values of name
  if(any(is.null(name), is.na(name), name == ""))
    return(construct_results(NA))

  # Strip punctuation from name string
  strip_name <- tolower(gsub("[[:punct:]]", "", name))

  # Limit max results
  sparql_limit <- ifelse(max_results > 50, 50, max_results)

  # Construct the query
  query_string <- paste0("
    SELECT ?id ?pref_name ?birth_year ?death_year ?gender ?nationality ?score
    WHERE {
      ?artist skos:inScheme ulan: ;
        luc:term '", strip_name, "' ;
        luc:score ?score ;
        rdf:type gvp:PersonConcept ;
        dc:identifier ?id ;
        gvp:prefLabelGVP [gvp:term ?pref_name] .

      ?artist foaf:focus ?focus .
      ?focus gvp:biographyPreferred ?bio .
      ?bio gvp:estStart ?birth_year ;
        gvp:estEnd ?death_year . ",
      date_filter(inclusive, early_year, late_year),
      "OPTIONAL {
        ?bio schema:gender [gvp:prefLabelGVP [gvp:term ?gender]] .
      }

      OPTIONAL {
        ?focus gvp:nationalityPreferred [gvp:prefLabelGVP [gvp:term ?nationality]] .
      }
    } LIMIT ", sparql_limit)

  # Fire the query to the Getty SPARQL endpoint and parse the results
  results <- readr::read_csv(sparql_url(query_string), col_types = "iciiccn")

  if(nrow(results) < 1) {
    construct_results(NA, name = name)
  } else {
    # Select the "value" fields from the returned list, and reformat them as a
    # tidy dataframe
    construct_results(results)
  }
}
