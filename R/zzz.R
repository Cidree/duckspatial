


.onLoad <- function(libname, pkgname) {
  op <- options()
  op_duckspatial <- list(
    duckspatial.output_type = "duckspatial_df",
    duckspatial.max_rows = 1e6
  )
  toset <- !(names(op_duckspatial) %in% names(op))
  if (any(toset)) options(op_duckspatial[toset])
  
  # Allow dplyr to use duckspatial functions inside dplyr verbs
  # with lazy tables. We need to create a macro for each function
  # that we want to use within the verbs
  target_conn <- ddbs_default_conn(create = TRUE)
  DBI::dbExecute(
    target_conn,
    "CREATE OR REPLACE MACRO ddbs_is_simple(geom) AS ST_IsSimple(geom);
    CREATE OR REPLACE MACRO ddbs_is_valid(geom) AS ST_IsValid(geom);
    CREATE OR REPLACE MACRO ddbs_is_closed(geom) AS ST_IsClosed(geom);
    CREATE OR REPLACE MACRO ddbs_is_ring(geom) AS ST_IsRing(geom);
    CREATE OR REPLACE MACRO ddbs_is_empty(geom) AS ST_IsEmpty(geom);
    CREATE OR REPLACE MACRO ddbs_area(geom) AS (
      CASE 
        WHEN crs_duckspatial = 'EPSG:4326' THEN ST_Area_Spheroid(ST_FlipCoordinates(geom))
        ELSE ST_Area(geom)
      END
    );
    CREATE OR REPLACE MACRO ddbs_length(geom) AS (
      CASE 
        WHEN crs_duckspatial = 'EPSG:4326' THEN ST_Length_Spheroid(ST_FlipCoordinates(geom))
        ELSE ST_Length(geom)
      END
    );
    CREATE OR REPLACE MACRO ddbs_perimeter(geom) AS (
      CASE 
        WHEN crs_duckspatial = 'EPSG:4326' THEN ST_Perimeter_Spheroid(ST_FlipCoordinates(geom))
        ELSE ST_Perimeter(geom)
      END
    );
    "
  )

  
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
      "\nNotice: duckspatial functions now return lazy 'duckspatial_df' (dbplyr) objects by default instead of standard 'sf' objects.",
      "\nTo restore previous behavior, run: ddbs_options(output_type = 'sf')"
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
