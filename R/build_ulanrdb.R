#' Build a local ULAN database
#'
#' For use with method = "local" options on the ulan_ functions. This downloads
#' a current database of IDs, preferred/alternate names, and biographical
#' details from \url{http://vocab.getty.edu/sparql}. This requires that the
#' container package \code{ulanrdb} is installed.
#'
#' @export
build_ulanrdb <- function() {

  # First check that ulanrdb is installed
  check_ulanrdb_package()

  if(interactive()) {
    # Now check if a DB already exists. If so, offer to rewrite.
    if(ulanrdb_exists()) {
      input <- utils::menu(c("Yes", "No"), title = paste0("Local database already exists. Overwrite?"))
      if(input == 2) {
        message("Not overwriting ", ulanrdb_path())
        return(invisible())
      }
    }
  }

  # If proceeding, clear out any existing db
  delete_ulanrdb()

  if(interactive()) {
    # Confirm before starting download, offering approximate size
    input <- utils::menu(c("Yes", "No"), title = paste0("Downloading tables to ", ulanrdb_path(), ". The total download size is usually ~26.5MB. Proceed?"))
    if(input != 1)
      stop("A local ULAN database must be built in order to use the local versions of the ulanr functions.")
  }

  build_tables(ulanrdb_path())

  # Once local table has been built and saved, reload the entire package in
  # order to trigger .onAttach
  devtools::reload(devtools::inst("ulanr"))
}

# The path of the .rda table within the ulanrdb package
ulanrdb_path <- function() {
  paste0(system.file("db", package = "ulanrdb"), "/ulan_table.rda")
}

# Does an .rda file for the ULAN DB exist?
ulanrdb_exists <- function() {
  file.exists(ulanrdb_path())
}

# Is the query_table object loaded?
ulanrdb_is_loaded <- function() {
  exists("query_table")
}

# Load the .rda file into the ulanr package environment. Note that this only
# works during .onAttach, before the environment is locked.
load_ulanrdb <- function() {
  load(ulanrdb_path(), envir = as.environment("package:ulanr"))
}

# Download both tables, join, and save.
build_tables <- function(tbl_path) {
  message("Downloading alternate names table (~18MB)")
  id_response <- httr::GET(construct_sparql_url(id_altname_query()), httr::progress())
  message("Downloading attributes table (~8.5MB)")
  attr_response <- httr::GET(construct_sparql_url(id_attributes_query()), httr::progress())

  # Read tables into dataframes
  message("Parsing downloads...")
  id_tbl <- readr::read_csv(httr::content(id_response, as = "text"), col_types = "ic")
  attr_tbl <- readr::read_csv(httr::content(attr_response, as = "text"), col_types = "iciicc")
  query_table <- dplyr::left_join(id_tbl, attr_tbl, by = "id")
  query_table$alt_name <- tolower(gsub("[[:punct:]]", "", alt_name))
  query_table <- dplyr::distinct(query_table)

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

# Adds correct urls and query params, while URL-encoding the SPARQL query string
construct_sparql_url <- function(query) {
  endpoint <- "http://vocab.getty.edu/sparql"
  escaped_query <- utils::URLencode(query, reserved = TRUE)
  paste0(endpoint, ".csv?query=", escaped_query)
}

# Check the status of the ulanrdb package
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
  input <- utils::menu(c("Yes", "No"), title = "Install the ulanrdb package?")
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
