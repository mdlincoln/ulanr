# A wee hack to get around .query_table not being defined elesewhere in the
# package. This avoids a CRAN note
globalVariables(c(".query_table"))

# Load the local ulanrdb table if it exists, otherwise advise the user that they
# will need to install it if they wish to use local implementations of any ulanr
# methods.
.onAttach <- function(libname, pkgname) {
  if(ulanrdb_exists()) {
    packageStartupMessage("Loading local ulanr database.")
    load_ulanrdb()
  } else {
    packageStartupMessage("To use the local implementation of ulanr functions, install ulanrdb and run build_ulanrdb().")
  }
}
