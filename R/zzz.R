


.onLoad <- function(libname, pkgname) {
  op <- options()
  op_duckspatial <- list(
    duckspatial.output_type = "duckspatial_df",
    duckspatial.max_rows = 1e6,
    duckspatial.duckdb_storage_version = "v1.5.0"
  )
  toset <- !(names(op_duckspatial) %in% names(op))
  if (any(toset)) options(op_duckspatial[toset])
  
  # Make internal duckdb functions available if needed
  # This serves as a reminder that we use asNamespace("duckdb") in the code
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("duckspatial", utils::packageVersion("duckspatial"), "attached"),
    "\n* Compatible with DuckDB >= v1.5.1"
  )

  # Notify about default output change
  packageStartupMessage(
      "\nDefault output has changed on v1.0.0:",
      "\n  duckspatial now returns lazy `duckspatial_df` (dbplyr) objects",
      "\n  instead of `sf` objects.",
      "\n\nTo restore the previous behaviour:",
      "\n  ddbs_options(duckspatial.mode = 'sf')"
    )
}

.onUnload <- function(libpath) {
  conn <- getOption("duckspatial_conn", NULL)
  if (!is.null(conn) && DBI::dbIsValid(conn)) {
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL)
  }
  options(duckspatial_conn = NULL)
}
