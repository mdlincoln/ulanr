#' Construct SPARQL date filter
#'
#' Constructs a portion of the SPARQL query to filter artists based on life
#' dates
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
ulan_sparql_match_handler <- function(name, early_year, late_year, inclusive, max_results) {

  # Helper function to construct a tidy dataframe from the list returned from
  # the SPARQL query. The first column is the input name. If no results are
  # returned, then the remaining columns will all be "NA"
  construct_results <- function(sparql_results) {
    if("data.frame" %in% class(sparql_results)) {
      sparql_results$name <- name
      sparql_results$id <- as.integer(sparql_results$id)
      dplyr::select(sparql_results, name, id, pref_name, birth_year, death_year, gender, nationality, score)
    } else {
      data.frame(
        name = name,
        id = NA,
        pref_name = NA,
        birth_year = NA,
        death_year = NA,
        gender = NA,
        nationality = NA,
        score = NA
      )
    }
  }

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
  results <- readr::read_csv(sparql_url(query_string))

  if(nrow(results) == 0) {
    warning("No matches found for the following name: ", name)
    construct_results(NA)
  } else {
    # Select the "value" fields from the returned list, and reformat them as a
    # tidy dataframe
    construct_results(results)
  }
}

#' Iterate the SPARQL method over the provided vectors
#'
#' This internal function maps the inputs from the generic \link{ulan_data}
#' function to the SPARQL implementation
ulan_sparql_match <- function(names, early_year, late_year, inclusive, max_results) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if(use_pb(names)) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_sparql_match_handler(a, b, c, inclusive, max_results)},
      names, early_year, late_year, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    close(pb)
  } else {
    ids <- mapply(function(a, b, c) ulan_sparql_match_handler(a, b, c, inclusive, max_results), names, early_year, late_year,
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  return(ids)
}
