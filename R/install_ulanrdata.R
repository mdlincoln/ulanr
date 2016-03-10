# Check whether to install data for local ULAN matching, and install if
# necessary
#
# If the ulanrdata package is not installed, install it from GitHub using
# devtools. If it is not up to date, reinstall it.
check_ulanrdata_package <- function() {
  ulanrdata_version <- "0.1"
  if (!requireNamespace("ulanrdata", quietly = TRUE)) {
    message("The ulanrdata package needs to be installed from GitHub.")
    install_ulanrdata_package()
  } else if (utils::packageVersion("ulanrdata") < ulanrdata_version) {
    message("The ulanrdata package needs to be updated from GitHub.")
    install_ulanrdata_package()
  }
}

# Install the ulanrdata package after checking with the user
install_ulanrdata_package <- function() {
  input <- menu(c("Yes", "No"), title = "Install the ulanrdata package?")
  if (input == 1) {
    message("Installing the ulanrdata package.")
    tryCatch(devtools::install_github("mdlincoln/ulanrdata"),
             error = function(e) {
               stop("Failed to install the ulanrdata package. Please try installing
                    the package for yourself using the following command:
                    \n
                    devtools::install_github(\"mdlincoln/ulanrdata\")")
             })
  } else {
    stop("The ulanrdata package is necessary for your chosen method.")
  }
}
