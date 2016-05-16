#' Build a local ULAN database
#'
#' For use with method = "local" options on the ulan_ functions. This downloads
#' a current database of IDs, preferred/alternate names, and biographical
#' details from \url{http://vocab.getty.edu/sparql}. This requires that the
#' container package \link{ulanrdb} is installed.
#'
#' @export
build_ulanrdb <- function() {
  # First check that ulanrdb is installed
  check_ulanrdb_package()

  if(ulanrdb_exists()) {
    input <- menu(c("Yes", "No"), title = paste0("Local database already exists. Overwrite?"))
    if(input == 2) {
      message("Not overwriting ", ulanrdb_path())
      return()
    }
  }

  delete_ulanrdb()

  input <- menu(c("Yes", "No"), title = paste0("Downloading tables to ", ulanrdb_path(), ". The total download size is usually ~26.5MB. Proceed?"))
  if(input == 1) {
    build_tables(ulanrdb_path())
  } else {
    stop("A local ULAN database must be built in order to use the local versions of the ulanr functions.")
  }

  devtools::reload(devtools::inst("ulanr"))
}

# The path of the .rda table within the ulanrdb package
ulanrdb_path <- function() {
  paste0(system.file("db", package = "ulanrdb"), "/ulan_table.rda")
}

ulanrdb_exists <- function() {
  file.exists(ulanrdb_path())
}

ulanrdb_is_loaded <- function() {
  exists("query_table")
}

load_ulanrdb <- function() {
  load(ulanrdb_path(), envir = as.environment("package:ulanr"))
}

build_tables <- function(tbl_path) {
  message("Downloading alternate names table (~18MB)")
  id_response <- httr::GET(construct_sparql_url(id_altname_query()), httr::progress())
  message("Downloading attributes table (~8.5MB)")
  attr_response <- httr::GET(construct_sparql_url(id_attributes_query()), httr::progress())

  # Read tables into dataframes
  message("Parsing downloads...")
  id_tbl <- readr::read_csv(httr::content(id_response, as = "text"), col_types = "ic")
  attr_tbl <- readr::read_csv(httr::content(attr_response, as = "text"), col_types = "iciicc")
  query_table <- dplyr::distinct(
    dplyr::mutate(dplyr::left_join(id_tbl, attr_tbl, by = "id"),
                  alt_name = tolower(gsub("[[:punct:]]", "", alt_name))))

  message("Saving final table to ", tbl_path)
  save(query_table, file = tbl_path)
}

# Retrieve Table combining all IDs with both pref and alt names unified in one
# column, which can be used as a search mechanism
id_altname_query <- function() {
  "SELECT DISTINCT ?id ?alt_name
  WHERE {
    ?artist skos:inScheme ulan: ;
            rdf:type gvp:PersonConcept ;
            dc:identifier ?id .

    { ?artist xl:altLabel [xl:literalForm ?alt_name] . } UNION
    { ?artist xl:prefLabel [xl:literalForm ?alt_name] . }
  }"
}

# Creates a shorter, but wider table with preferred names and biographical
# details, when available
id_attributes_query <- function() {
  "SELECT DISTINCT ?id ?pref_name ?birth_year ?death_year ?nationality ?gender
  WHERE {
    ?artist skos:inScheme ulan: ;
            dc:identifier ?id ;
            rdf:type gvp:PersonConcept ;
            dc:identifier ?id ;
            xl:prefLabel [xl:literalForm ?pref_name] .

    OPTIONAL {
      ?artist foaf:focus [gvp:biographyPreferred ?bio] .

      OPTIONAL {
        ?bio gvp:estStart ?birth_year ;
             gvp:estEnd ?death_year .
      }

      OPTIONAL {
        ?bio schema:gender [gvp:prefLabelGVP [gvp:term ?gender]] .
      }
    }

    OPTIONAL {
      ?artist foaf:focus [gvp:nationalityPreferred [xl:prefLabel [gvp:term ?nationality]]] .
      FILTER(langMatches(lang(?nationality), \"EN\"))
    }
  }"
}

construct_sparql_url <- function(query) {
  endpoint <- "http://vocab.getty.edu/sparql"
  escaped_query <- URLencode(query, reserved = TRUE)
  paste0(endpoint, ".csv?query=", escaped_query)
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

#' Clear the local ULAN database
#'
#' A convenience function to remove the on-disk ULAN database
#'
#' @export
delete_ulanrdb <- function() {
  unlink(ulanrdb_path())
}
