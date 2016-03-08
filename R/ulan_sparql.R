#' Construct SPARQL date filter
#'
#' Constructs a portion of the SPARQL query to filter artists based on life
#' dates
#'
#' @param inclusive Logical. Should life dates be filtered inclusive of the
#'   [early_year, late_year] range?
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
date_filter <- function(inclusive, early_year, late_year) {
  if(inclusive) {
    paste0("FILTER(?startdate >= '", early_year, "'^^xsd:gYear && ?enddate <= '",
           late_year, "'^^xsd:gYear)")
  } else {
    paste0("FILTER(?startdate <= '", late_year, "'^^xsd:gYear && ?enddate >= '",
           early_year, "'^^xsd:gYear)")
  }
}

#' Format a SPARQL query as a URL
#'
#' Properly escapes the query to send to the Getty SPARQL endpoint
#'
#' @param query Query string.
#'
#' @return An atomic character vector.
sparql_url <- function(query) {
  endpoint <- "http://vocab.getty.edu/sparql"
  escaped_query <- URLencode(query, reserved = TRUE)
  paste0(endpoint, ".csv?query=", escaped_query)
}

#' Search for a matching ULAN id by using the Getty's live SPARQL endpoint and
#' its Lucene index.
#'
#' This internal function implements the \code{method = "sparql"} option for
#' \link{ulan_id}. See that funciton for documentation.
#'
#' @param name A character string of an artist's name
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
#' @param inclusive Logical. Should life dates be filtered inclusive of the
#'   [early_year, late_year] range?
ulan_sparql_id_handler <- function(name, early_year, late_year, inclusive) {

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
      luc:term '", name, "' ;
      rdf:type gvp:PersonConcept ;
      dc:identifier ?id .

    ?artist foaf:focus [gvp:biographyPreferred ?bio] .
    ?bio gvp:estStart ?startdate ;
         gvp:estEnd ?enddate .",
    date_filter(inclusive, early_year, late_year),
    "} LIMIT 1")

  # Fire the query to the Getty SPARQL endpoint and parse the results
  results <- readr::read_csv(sparql_url(query_string))

  if(nrow(results) == 0) {
    warning("No matches found for the following name: ", name)
    return(NA)
  } else {
    results[1,1]
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
#' @param inclusive Logical. Should life dates be filtered inclusive of the
#'   [early_year, late_year] range?
#' @param progress_bar Display a progress bar for long vectors.
ulan_sparql_id <- function(names, early_year, late_year, inclusive, progress_bar) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if((progress_bar == "default" & length(names) >= 50) | progress_bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- as.integer(mapply(function(a, b, c, d) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_sparql_id_handler(a, b, c, d)},
      names, early_year, late_year, inclusive, SIMPLIFY = TRUE, USE.NAMES = FALSE))
    close(pb)
    return(ids)
  } else {
    as.integer(mapply(function(a, b, c, d) ulan_sparql_id_handler(a, b, c, d),
           names, early_year, late_year, inclusive,SIMPLIFY = TRUE, USE.NAMES = FALSE))
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
#' @param inclusive Logical. Should life dates be filtered inclusive of the
#'   [early_year, late_year] range?
ulan_sparql_data_handler <- function(name, early_year, late_year, inclusive) {

  # Helper function to construct a tidy dataframe from the list returned from
  # the SPARQL query. The first column is the input name. If no results are
  # returned, then the remaining columns will all be "NA"
  construct_results <- function(sparql_results) {
    if("data.frame" %in% class(sparql_results)) {
      sparql_results$name <- name
      dplyr::select(sparql_results, name, id, pref_name, startdate, enddate, gender, nationality)
    } else {
      data_frame(
        name = name,
        id = NA,
        pref_name = NA,
        startdate = NA,
        enddate = NA,
        gender = NA,
        nationality = NA
      )
    }
  }

  # Return NA for missing or empty values of name
  if(any(is.null(name), is.na(name), name == ""))
    return(construct_results(NA))

  # Strip punctuation from name string
  strip_name <- tolower(gsub("[[:punct:]]", "", name))

  # Construct the query
  query_string <- paste0("
    SELECT ?id ?pref_name ?startdate ?enddate ?gender ?nationality
    WHERE {
      ?artist skos:inScheme ulan: ;
        luc:term '", strip_name, "' ;
        rdf:type gvp:PersonConcept ;
        dc:identifier ?id ;
        gvp:prefLabelGVP [gvp:term ?pref_name] .

      ?artist foaf:focus ?focus .
      ?focus gvp:biographyPreferred ?bio .
      ?bio gvp:estStart ?startdate ;
        gvp:estEnd ?enddate . ",
      date_filter(inclusive, early_year, late_year),
      "OPTIONAL {
        ?bio schema:gender [gvp:prefLabelGVP [gvp:term ?gender]] .
      }

      OPTIONAL {
        ?focus gvp:nationalityPreferred [gvp:prefLabelGVP [gvp:term ?nationality]] .
      }
    } LIMIT 1")

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
#'
#' @param names A character string of an artist's name
#' @param early_year Match only artists who died after this year.
#' @param late_year Match only artists who were born before this year.
#' @param inclusive Logical. Should life dates be filtered inclusive of the
#'   [early_year, late_year] range?
#' @param progress_bar Display a progress bar for long vectors.
ulan_sparql_data <- function(names, early_year, late_year, inclusive, progress_bar) {
  # For long queries or if explicitly set, create and increment txtProgressBar
  if((progress_bar == "default" & length(names) >= 50) | progress_bar == TRUE) {
    pb <- txtProgressBar(min = 0, max = length(names), style = 3)
    ids <- mapply(function(a, b, c, d) {
      setTxtProgressBar(pb, (getTxtProgressBar(pb) + 1))
      ulan_sparql_data_handler(a, b, c, d)},
      names, early_year, late_year, inclusive, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    close(pb)
    # Bind all returned dataframes together and include the original input vector
    dplyr::bind_rows(ids)
  } else {
    ids <- mapply(function(a, b, c, d) ulan_sparql_data_handler(a, b, c, d),
                  names, early_year, late_year, inclusive, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    # Bind all returned dataframes together and include the original input vector
    dplyr::bind_rows(ids)
  }
}
