#' Create a duckspatial lazy spatial data frame
#'
#' Extends tbl_duckdb_connection with spatial metadata (CRS, geometry column).
#'
#' @param x Input: tbl_duckdb_connection, tbl_lazy, or similar dbplyr object
#' @param crs CRS object or string
#' @param geom_col Name of geometry column (default: "geom")
#' @param source_table Name of the source table if applicable
#' @return A duckspatial_df object
#' @export
new_duckspatial_df <- function(x, crs = NULL, geom_col = "geom", source_table = NULL) {
  # Avoid double wrapping
  if (inherits(x, "duckspatial_df")) return(x)
  
  if (!inherits(x, "tbl_sql")) {
     # If purely data frame, we warn or error? 
     # Ideally duckspatial_df should be backed by DuckDB.
     # But we can allow wrapping local DF for consistency, though verb overrides won't work well without dbplyr.
     # Let's enforce tbl_sql/tbl_lazy for now or allow it but warn.
     if (!is.data.frame(x)) {
       cli::cli_abort("x must be a tbl_sql or data.frame")
     }
  }
  
  # Prepend our class
  structure(
    x,
    class = c("duckspatial_df", class(x)),
    sf_column = geom_col, # Keeping attribute name as sf_column for compatibility
    crs = if (inherits(crs, "crs")) crs else sf::st_crs(crs),
    source_table = source_table
  )
}

#' Check if object is a duckspatial_df
#' @param x Object to test
#' @return Logical
#' @export
is_duckspatial_df <- function(x) {
  inherits(x, "duckspatial_df")
}

#' Convert objects to duckspatial_df
#'
#' @param x Object to convert (sf, tbl_lazy, data.frame, or table name)
#' @param conn DuckDB connection (required for character table names)
#' @param crs CRS object or string (auto-detected from sf objects)
#' @param geom_col Geometry column name (default: "geom")
#' @param ... Additional arguments passed to methods
#' @return A duckspatial_df object
#' @export
as_duckspatial_df <- function(x, conn = NULL, crs = NULL, geom_col = "geom", ...) {
  UseMethod("as_duckspatial_df")
}

#' @export
as_duckspatial_df.duckspatial_df <- function(x, conn = NULL, crs = NULL, 
                                              geom_col = NULL, ...) {
  x
}

#' @export
as_duckspatial_df.sf <- function(x, conn = NULL, crs = NULL, geom_col = NULL, ...) {
  # Get CRS and geom column from sf object
  if (is.null(crs)) crs <- sf::st_crs(x)
  if (is.null(geom_col)) geom_col <- attr(x, "sf_column")
  
  # Get or create connection
  if (is.null(conn)) {
    conn <- ddbs_default_conn()
  }
  
  # Register sf as temp view
  view_name <- ddbs_temp_view_name()
  ddbs_write_vector(
    conn = conn,
    data = x,
    name = view_name,
    quiet = TRUE,
    temp_view = TRUE
  )
  
  # Create lazy table
  lazy_tbl <- dplyr::tbl(conn, view_name)
  new_duckspatial_df(lazy_tbl, crs = crs, geom_col = geom_col, source_table = view_name)
}

#' @export
as_duckspatial_df.tbl_duckdb_connection <- function(x, conn = NULL, crs = NULL, 
                                                     geom_col = "geom", ...) {
  # Auto-detect CRS if not provided (DuckDB-specific)
  if (is.null(crs)) {
    crs <- ddbs_crs(x)
  }
  
  # Extract source table for efficient get_query_list path
  source_table <- tryCatch(
    as.character(dbplyr::remote_name(x)),
    error = function(e) NULL
  )
  
  new_duckspatial_df(x, crs = crs, geom_col = geom_col, source_table = source_table)
}

#' @export
as_duckspatial_df.tbl_lazy <- function(x, conn = NULL, crs = NULL, 
                                        geom_col = "geom", ...) {
  # Auto-detect CRS if not provided
  if (is.null(crs)) {
    crs <- ddbs_crs(x)
  }
  
  # Extract source table for efficient get_query_list path
  source_table <- tryCatch(
    as.character(dbplyr::remote_name(x)),
    error = function(e) NULL
  )
  
  new_duckspatial_df(x, crs = crs, geom_col = geom_col, source_table = source_table)
}

#' @export
as_duckspatial_df.character <- function(x, conn = NULL, crs = NULL, 
                                         geom_col = "geom", ...) {
  if (is.null(conn)) {
    conn <- ddbs_default_conn(create = FALSE)
    if (is.null(conn)) {
      cli::cli_abort("{.arg conn} must be provided when using table names as input.")
    }
  }
  
  lazy_tbl <- dplyr::tbl(conn, x)
  new_duckspatial_df(lazy_tbl, crs = crs, geom_col = geom_col, source_table = x)
}

#' @export
as_duckspatial_df.data.frame <- function(x, conn = NULL, crs = NULL, 
                                          geom_col = "geom", ...) {
   # Upload to DuckDB
   if (is.null(conn)) conn <- ddbs_default_conn()
   
   view_name <- ddbs_temp_view_name()
   DBI::dbWriteTable(conn, view_name, x)
   
   lazy_tbl <- dplyr::tbl(conn, view_name)
   new_duckspatial_df(lazy_tbl, crs = crs, geom_col = geom_col, source_table = view_name)
}
