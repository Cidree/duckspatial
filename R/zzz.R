


.onLoad <- function(libname, pkgname) {
  op <- options()
  op_duckspatial <- list(
    duckspatial.output_type = "duckspatial_df",
    duckspatial.max_rows = 1e6,
    duckspatial.fallback_info = FALSE
  )
  toset <- !(names(op_duckspatial) %in% names(op))
  if (any(toset)) options(op_duckspatial[toset])
  
  # Make internal duckdb functions available if needed
  # This serves as a reminder that we use asNamespace("duckdb") in the code
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Important: 'crs_column' and 'crs' arguments are deprecated and will be removed in the next version.\n",
    "If possible, use the default values of these arguments to avoid future issues."
  )

  # Notify about default output change
  if (identical(getOption("duckspatial.output_type"), "duckspatial_df")) {
    packageStartupMessage(
      "\nNotice: duckspatial functions now return lazy 'duckspatial_df' (ALTREP) objects by default instead of standard 'sf' objects.",
      "\nTo restore previous behavior, set: options(duckspatial.output_type = 'sf')"
    )
  }
}

.onUnload <- function(libpath) {
  conn <- getOption("duckspatial_conn", NULL)
  if (!is.null(conn) && DBI::dbIsValid(conn)) {
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL)
  }
  options(duckspatial_conn = NULL)
}
