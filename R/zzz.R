


.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Important: 'crs_column' and 'crs' arguments are deprecated and will be removed in the next version.\n",
    "If possible, use the default values of these arguments to avoid future issues."
  )
}