


.onLoad <- function(libname, pkgname) {
  op <- options()
  op_duckspatial <- list(
    duckspatial.output_type = "duckspatial_df",
    duckspatial.max_rows = 1e6,
    duckspatial.duckdb_storage_version = "v2.0.0"
  )
  toset <- !(names(op_duckspatial) %in% names(op))
  if (any(toset)) options(op_duckspatial[toset])
  
  # Make internal duckdb functions available if needed
  # This serves as a reminder that we use asNamespace("duckdb") in the code
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("duckspatial", utils::packageVersion("duckspatial"), "attached"),
    "\n Compatible with DuckDB >= v2.0.0"
  )

  # Notify about default output change
  packageStartupMessage(
      "\nDefault duckdb_storage_version has changed in duckspatial v1.4.0:",
      "\n  duckdb_storage_version is now v2.0.0"
    )
}

.onUnload <- function(libpath) {
  conn <- getOption("duckspatial_conn", NULL)
  if (!is.null(conn) && DBI::dbIsValid(conn)) {
    tryCatch(DBI::dbDisconnect(conn), error = function(e) NULL)
  }
  options(duckspatial_conn = NULL)
}
