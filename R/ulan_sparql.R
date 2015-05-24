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
  query_string <- paste0("
    SELECT ?id
    WHERE {
    ?artist skos:inScheme ulan: ;
      rdf:type gvp:PersonConcept ;
      dc:identifier ?id ;
      xl:prefLabel|xl:altLabel [luc:term '", name, "'] .

    ?artist foaf:focus [gvp:biographyPreferred ?bio] .
    ?bio gvp:estStart ?startdate ;
         gvp:estEnd ?enddate .
    FILTER(?startdate >= '", early_year, "'^^xsd:gYear && ?enddate <= '",
                        late_year, "'^^xsd:gYear)
    } LIMIT 1")

  # Fire the query to the Getty SPARQL endpoint and parse the results
  results <- jsonlite::fromJSON(
    paste0("http://vocab.getty.edu/sparql.json?query=",
           URLencode(query_string, reserved = TRUE)))

  if(length(results$results$bindings) == 0) {
    warning("No matches found for the following name: ", name)
    return(NA)
  } else {
    as.integer(results$results$bindings$id$value)
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

#' Search for a matching ULAN id and its associated databy using the Getty's
#' live SPARQL endpoint and its Lucene index.
#'
#' This internal function implements the \code{method = "sparql"} option for
#' \link{ulan_data}. See that funciton for documentation.
#'
#' @param name A character string of an artist's name
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
ulan_sparql_data_handler <- function(name, early_year, late_year) {

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
  query_string <- paste0("
    SELECT ?id ?startdate ?enddate ?gender ?nationality
    WHERE {
      ?artist skos:inScheme ulan: ;
        rdf:type gvp:PersonConcept ;
        dc:identifier ?id ;
        xl:prefLabel|xl:altLabel [luc:term '", name, "'] .

      ?artist foaf:focus ?focus .
      ?focus gvp:biographyPreferred ?bio .
      ?bio gvp:estStart ?startdate ;
        gvp:estEnd ?enddate .
        FILTER(?startdate >= '", early_year, "'^^xsd:gYear && ?enddate <= '",
                    late_year, "'^^xsd:gYear)

      OPTIONAL {
        ?bio schema:gender [gvp:prefLabelGVP [gvp:term ?gender]] .
      }

      OPTIONAL {
        ?focus gvp:nationalityPreferred [gvp:prefLabelGVP [gvp:term ?nationality]] .
      }
    } LIMIT 1")

  # Fire the query to the Getty SPARQL endpoint and parse the results
  results <- jsonlite::fromJSON(
  RCurl::getURL(paste0("http://vocab.getty.edu/sparql.json?query=",
           URLencode(query_string, reserved = TRUE))))

  if(length(results$results$bindings) == 0) {
    warning("No matches found for the following name: ", name)
    return(NA)
  } else {
    # Select the "value" fields from the returned list, and reformat them as a
    # tidy dataframe
    raw_bindings <- unlist(results$results$bindings)
    select_bindings <- raw_bindings[grepl("value", names(raw_bindings))]
    select_data <- data.frame(as.list(select_bindings), stringsAsFactors = FALSE)
    names(select_data) <- gsub("\\.value", "", names(select_data))
    return(select_data)
  }
}

#' Iterate the SPARQL method over the provided vectors
#'
#' This internal function maps the inputs from the generic \link{ulan_data}
#' function to the SPARQL implementation
#'
#' @param names A character string of an artist's name
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
#' @param progress_bar Display a progress bar for long vectors.
ulan_sparql_data <- function(names, early_year, late_year, progress_bar) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if((progress_bar == "default" & length(names) >= 50) | progress_bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_sparql_data_handler(a, b, c)},
      names, early_year, late_year, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    close(pb)
    # Bind all returned dataframes together
    return(dplyr::bind_rows(ids))
  } else {
    ids <- mapply(function(a, b, c) ulan_sparql_data_handler(a, b, c), names, early_year, late_year, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    # Bind all returned dataframes together
    return(dplyr::bind_rows(ids))
  }
}
