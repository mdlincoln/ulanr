#' Build a local ULAN database
#'
#' For use with method = "local" options on the ulan_ functions. This downloads a current database of IDs, preferred/alternate names, and biographical details from \url{http://vocab.getty.edu/sparql}. This requires that the container package \link{ulanrdb} is installed.
#'
#' @export
build_ulanrdb <- function() {
  # First check that ulanrdb is installed
  check_ulanrdb_package()

  dbpath <- ulanrdb_path()

  query <- "SELECT DISTINCT ?id ?pref_name ?alt_name ?birth_year ?death_year ?nationality ?gender
  WHERE {
  ?artist skos:inScheme ulan: ;
  dc:identifier ?id ;
  rdf:type gvp:PersonConcept ;
  xl:prefLabel [xl:literalForm ?pref_name] .

  OPTIONAL {
  { ?artist xl:altLabel [xl:literalForm ?alt_name] . } UNION
  { ?artist xl:prefLabel [xl:literalForm ?alt_name] . }
  }

  OPTIONAL {
  ?artist foaf:focus [gvp:biographyPreferred ?bio] .

  OPTIONAL {
  ?bio gvp:estStart ?birth_year ;
  gvp:estEnd ?death_year .
  }

  OPTIONAL {
  ?bio schema:gender [gvp:prefLabelGVP [gvp:term ?gender]] .
  FILTER(langMatches(lang(?gender), \"EN\"))
  }
  }

  OPTIONAL {
  ?artist foaf:focus [gvp:nationalityPreferred [xl:prefLabel [gvp:term ?nationality]]] .
  FILTER(langMatches(lang(?nationality), \"EN\"))
  }
}"

  input <- menu(c("Yes", "No"), title = paste0("Downloading table to ", dbpath, ". The file size is usually ~16MB. Proceed? (y/n)"))
  if(input == 1) {
    httr::GET(construct_sparql_url(query), write_disk(dbpath, overwrite = TRUE), progress())
  } else {
    stop("A local ULAN database must be built in order to proceed.")
  }

    # attr_tbl <- readr::read_csv(dbpath, col_types = c("icciicc"))
}

construct_sparql_url <- function(query) {
  endpoint <- "http://vocab.getty.edu/sparql"
  escaped_query <- URLencode(query, reserved = TRUE)
  paste0(endpoint, ".csv?query=", escaped_query)
}


ulanrdb_path <- function() {
  paste0(system.file("db", package = "ulanrdb", mustWork = TRUE), "/ulan_table.csv")
}

check_ulanrdb_package <- function() {
  ulanrdb_version <- "0.1"
  if (!requireNamespace("ulanrdb", quietly = TRUE)) {
    message("The ulanrdb package needs to be installed from GitHub.")
    install_ulanrdb_package()
  } else if (utils::packageVersion("ulanrdb") < ulanrdb_version) {
    message("The ulanrdb package needs to be updated from GitHub.")
    install_ulanrdb_package()
  }
}

install_ulanrdb_package <- function() {
  input <- menu(c("Yes", "No"), title = "Install the ulanrdb package?")
  if (input == 1) {
    message("Installing the ulanrdb package.")
    tryCatch(devtools::install_github("mdlincoln/ulanrdb"),
             error = function(e) {
               stop("Failed to install the ulanrdb package. Please try installing
                    the package for yourself using the following command:
                    \n
                    devtools::install_github(\"mdlincoln/ulanrdb\")")
             })
  } else {
    stop("The ulanrdb package is necessary for your chosen method.")
  }
}
